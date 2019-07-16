#' Decompress gzip file
#'
#' Automatically decompress a gzip file so that it can be read directly by any of the `raster` import functions (`raster`, `stack`, `brick`). The gzip file is decompressed to a temporary file which is then read (while the original gzip file is kept). This is useful to keep and use directly large, gzip-compressed GIS files. Decompressed files are automatically deleted when cleaning temporary files.
#'
#' @param filepath Path to the gzip file.
#'
#' @return Path to the decompressed file (which can then be read by raster functions).
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' gisdata <- brick(ungzip("~/GISdata/climate.nc.gz"))
#' }
ungzip <- function(filepath){
  R.utils::decompressFile(filepath, ext = "gz", FUN = gzfile,
                          destname = gsub(".gz", "", filepath, ignore.case = TRUE),
                          remove = FALSE, temporary = TRUE,
                          skip = FALSE, overwrite = TRUE)
}
