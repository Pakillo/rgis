
#' Fast extraction of raster values
#'
#' Extract values from raster layers for a given set of polygons. Parallelised and using `velox` package for faster extraction than using [raster::extract()].
#'
#' @param sf [sf](https://r-spatial.github.io/sf/index.html) data frame containing polygon data.
#' @param ras A (list of) Raster* objects, or a character vector of paths to Raster files. Thus, raster files can be stored on disk, without having to load them on memory first.
#' @param funct The name of a function to summarise raster values within polygons. Default is 'mean.na' (simple avergae excluding NA), but other functions can be used (in that case, provide function name without quotes, e.g. funct = median). See [velox::VeloxRaster_extract()].
#' @param small.algo Logical. Use 'small' algorithm to detect overlap between polygons and raster cells? See [velox::VeloxRaster_extract()]. Default is FALSE.
#' @param col.names Optional. Character vector with names for extracted columns in the output dataframe. If not provided, the function will use the Raster* object layer names, or the file name followed by consecutive numbers.
#' @param parallel Logical. Run function in parallel (using `future.apply`)? Default is TRUE.
#'
#' @return A sf data frame with the same number of rows as the original and new columns containing the extracted raster values.
#' @export
#'
extract_velox_parallel <- function(sf = NULL, ras = NULL,
                                   funct = 'mean.na', small.algo = FALSE,
                                   col.names = NULL, parallel = TRUE) {

  stopifnot('sf' %in% class(sf))

  ## Function to summarise raster values
  if (is.null(funct)) stop("Please provide a function to summarise the raster values. Default is 'mean.na'.")

  # Define mean.na function
  if (funct == 'mean.na') {
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
      if (is.list(ras)) {
        if (!is.null(names(ras))) {
          ras.names <- names(ras)
        } else {
          # if ras is an *unnamed* list, assume they are paths to files,
          # and use file basename (with extension)
          ras.names <- unlist(lapply(ras, basename))
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
  vals <- ras.vx$extract(spdf, df = TRUE, ...)  #fun = funct, small = small.algo)
  vals <- as.data.frame(vals[, -1])


  ## Name extracted columns ##

  if (is.null(varnames)) {
    # velox does not seem to provide layer names, so using raster instead
    names(vals) <- names(raster::stack(ras, quick = TRUE))
  }


  return(vals)


}
