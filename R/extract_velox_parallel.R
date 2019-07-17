
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

  out.list <- future.apply::future_lapply(list(ras),
                                          extract_velox,
                                          spdf = sf,
                                          fun = funct, small = small.algo,
                                          varnames = col.names)
  #plan(sequential)

  out.df <- dplyr::bind_cols(out.list)
  sf.final <- dplyr::bind_cols(sf, out.df)
  sf.final


}





extract_velox <- function(ras = NULL, spdf = NULL,
                          ..., #funct = 'mean.na', small.algo = FALSE,
                          varnames = NULL) {

  # ras = Raster* object (in memory or path to file)
  # df = sf object

  ras.vx <- velox::velox(ras)

  ## Ensure CRS of spdf and ras match
  stopifnot(raster::compareCRS(sf::st_crs(spdf)$proj4string, ras.vx$crs))

  # crop to extent of sf/sp object
  ras.vx$crop(spdf)


  # extract
  vals <- ras.vx$extract(spdf, df = TRUE, ...)  #fun = funct, small = small.algo)
  vals <- as.data.frame(vals[, -1])



  ## Name extracted columns ##

  # if providing varnames, use those
  if (!is.null(varnames)) {
    if (length(varnames) != (ncol(vals))) {
      stop("length of col.names does not match number of raster layers.")
    } else {
      names(vals) <- varnames
    }

    ## Otherwise use Raster* layer names (if available)
    ## otherwise use filename followed by numbers
  } else {
    if (!is.null(names(ras))){
      names(vals) <- names(ras)
    } else {
      warning("Using raster file names for naming columns... Be careful with potential name clashes.")
      names(vals) <- paste(basename(ras), seq_len(ncol(vals)), sep = ".")
    }

  }

  return(vals)


}