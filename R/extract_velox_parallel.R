
#' Fast extraction of raster values
#'
#' Extract values from raster layers for a given set of points or polygons. Using `velox` package in parallel for fast extraction.
#'
#' @param sf [sf](https://r-spatial.github.io/sf/index.html) data frame containing point or polygon data.
#' @param ras A Raster* object (RasterLayer, RasterStack, or RasterBrick), a _named_ list of Raster objects, or a character vector or list of paths to Raster files on disk (e.g. as obtained through `list.files`). Thus, raster files can be stored on disk, without having to load them on memory first.
#' @param funct The name of a function to summarise raster values within polygons. Default is 'mean.na' (simple average, excluding NA), but other functions can be used (in that case, provide function name without quotes, e.g. funct = median). See [velox::VeloxRaster_extract()]. No function is used when `sf` are points.
#' @param small.algo Logical. Use 'small' algorithm to detect overlap between polygons and raster cells? See [velox::VeloxRaster_extract()]. Default is FALSE.
#' @param col.names Optional. Character vector with names for extracted columns in the output dataframe. If not provided, the function will use the Raster* layer names or, if `ras` is a list of paths, the file name followed by layer names.
#' @param parallel Logical. Run function in parallel (using `future.apply`)? Default is TRUE.
#'
#' @return A sf data frame with the same number of rows as the original, and new columns containing the extracted raster values.
#' @export
#'
#'@examples
#' library(raster)
#' library(rgis)
#'
#' ## Example taken from raster::extract
#'
#' # Create polygons
#' poly1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
#' poly2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
#' polys <- spPolygons(poly1, poly2)
#' polys.sf <- sf::st_as_sf(polys)
#' sf::st_crs(polys.sf) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#'
#' # Create rasters
#' r1 <- raster(ncol = 36, nrow = 18, vals = 1:(18*36))
#' r2 <- raster(ncol = 36, nrow = 18, vals = (1:(18*36))*2)
#' ras <- stack(r1, r2)
#'
#' plot(ras, 1)
#' plot(polys, add = TRUE)
#'
#' # Extract values
#' extract.parallel <- rgis::extract_velox_parallel(sf = polys.sf, ras = ras, parallel = TRUE)
#' head(extract.parallel)
#'
#' extract.noparallel <- rgis::extract_velox_parallel(sf = polys.sf, ras = ras, parallel = FALSE)
#'
#' # Compare with raster::extract
#' raster.extract <- raster::extract(ras, polys, fun = mean, df = TRUE)
#' head(raster.extract)
#'
#'
#'
#' ### Providing named list of rasters
#' ras.list <- list(r1 = r1, r2 = r2)
#' rgis.out <- rgis::extract_velox_parallel(sf = polys.sf, ras = ras.list, parallel = FALSE)
#' head(rgis.out)

extract_velox_parallel <- function(sf = NULL, ras = NULL,
                                   funct = 'mean.na', small.algo = FALSE,
                                   col.names = NULL, parallel = TRUE) {

  stopifnot(inherits(sf, "sf"))

  sf.geometry <- unique(sf::st_geometry_type(sf))

  if (length(sf.geometry) > 1) stop("sf must contain a single geometry type (points or polygons).")

  stopifnot(sf.geometry == "POINT" | sf.geometry == "POLYGON" | sf.geometry == "MULTIPOLYGON")


  ## Function to summarise raster values when sf contains polygons
  if (sf.geometry != "POINT" & is.null(funct)) stop("Please provide a function to summarise the raster values: default is 'mean.na'.")

  # Define mean.na function
  if (is.character(funct) && funct == 'mean.na') {
    funct <- function(x) {mean(x, na.rm = TRUE)}
  }


  # Parallelise using future.apply package
  if (isTRUE(parallel)) {
    oplan <- future::plan()
    on.exit(future::plan(oplan), add = TRUE)
    future::plan(future::multiprocess)
  }

  # First argument to future_lapply must be a list
  if (!inherits(ras, "list")) {
    ras.list <- list(ras)
  } else {
    ras.list <- ras
  }

  # Run extraction
  out.list <- future.apply::future_lapply(ras.list,
                                          extract_velox,
                                          spdf = sf,
                                          fun = funct, small = small.algo,
                                          varnames = col.names)



  ## Name extracted columns ##

  if (is.null(col.names)) {
    # if ras = Raster* object, use the object name
    if (inherits(ras, c("RasterLayer", "RasterStack", "RasterBrick"))) {
      ras.names <- as.character(match.call()$ras)
    } else {
      # if ras is a *named* list, use names of list elements
      if (is.list(ras) & !is.null(names(ras))) {
        ras.names <- names(ras)
      } else {
        # if ras is an *unnamed* list, or a character vector,
        # they must be paths to files (including / or \\):
        # use file basename (with extension)
        if (grepl("/", ras) || grepl("\\\\", ras)) {
          ras.names <- unlist(lapply(ras, basename))
        } else {
          stop("ras must be a Raster* object, a named list of Raster objects, or a list or character vector of paths to raster files on disk.")
        }
      }
    }
  }



  names(out.list) <- ras.names

  for (i in seq_along(out.list)) {
    names(out.list[[i]]) <- paste(names(out.list)[[i]], names(out.list[[i]]), sep = "_")
  }


  out.df <- dplyr::bind_cols(out.list)  # list to dataframe

  # if providing col.names, use those
  if (!is.null(col.names)) {
    if (length(col.names) != ncol(out.df)) {
      stop("length of col.names does not match number of extracted columns.")
    }
    names(out.df) <- col.names
  }

  # Merge extracted columns with sf dataframe
  sf.final <- dplyr::bind_cols(sf, out.df)
  sf.final


}





extract_velox <- function(ras = NULL, spdf = NULL,
                          ..., #funct = 'mean.na', small.algo = FALSE,
                          varnames = NULL) {

  # ras = Raster* object (in memory or path to file)

  ras.vx <- velox::velox(ras)

  ## Ensure CRS of spdf and ras match
  stopifnot(raster::compareCRS(sf::st_crs(spdf)$proj4string, ras.vx$crs))

  # crop to extent of sf/sp object
  ras.vx$crop(spdf)


  # extract
  if (unique(sf::st_geometry_type(spdf)) == "POINT") {

    vals <- as.data.frame(ras.vx$extract_points(spdf))

  } else {   # POLYGON

    vals <- ras.vx$extract(spdf, df = TRUE, ...)  #fun = funct, small = small.algo)
    vals <- as.data.frame(vals[, -1])

  }



  ## Name extracted columns ##

  if (is.null(varnames)) {
    # velox does not seem to provide layer names, so using raster instead
    names(vals) <- names(raster::stack(ras, quick = TRUE))
  }


  return(vals)


}
