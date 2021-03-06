% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ungzip.R
\name{ungzip}
\alias{ungzip}
\title{Decompress gzip file}
\usage{
ungzip(filepath)
}
\arguments{
\item{filepath}{Path to the gzip file.}
}
\value{
Path to the decompressed file (which can then be read by raster functions).
}
\description{
Automatically decompress a gzip file so that it can be read directly by any of the \code{raster} import functions (\code{raster}, \code{stack}, \code{brick}). The gzip file is decompressed to a temporary file which is then read (while the original gzip file is kept). This is useful to keep and use directly large, gzip-compressed GIS files. Decompressed files are automatically deleted when cleaning temporary files.
}
\examples{
\dontrun{
library(raster)
gisdata <- brick(ungzip("~/GISdata/climate.nc.gz"))
}
}
