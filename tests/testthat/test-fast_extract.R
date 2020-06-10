context("Test fast_extract")

library(raster)
library(sf)
library(rgis)

## Example taken from raster::extract
poly1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
poly2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- spPolygons(poly1, poly2)
polys.sf <- sf::st_as_sf(polys)
sf::st_crs(polys.sf) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r1 <- raster(ncol = 36, nrow = 18, vals = 1:(18*36))
r2 <- raster(ncol = 36, nrow = 18, vals = (1:(18*36))*2)
ras <- stack(r1, r2)
##


raster.output <- raster::extract(ras, polys, fun = mean, df = TRUE)
raster.output <- raster.output[, -1]

rgis.output.parallel <- rgis::fast_extract(sf = polys.sf, ras = ras, parallel = TRUE)

rgis.output.noparallel <- rgis::fast_extract(sf = polys.sf, ras = ras, parallel = FALSE)




###############################################################

## Providing named list of rasters
ras.list <- list(r1 = r1, r2 = r2)
rgis.out <- rgis::fast_extract(sf = polys.sf, ras = ras.list, parallel = FALSE)



################################################################

## Using sf of points:
set.seed(0)
rp1 <- raster(nrows = 10, ncol = 10, resolution = c(0.1, 0.1),
              xmn = 0, xmx = 1, ymn = 0, ymx = 1,
              vals = runif(100)*10)

rp2 <- raster(nrows = 10, ncol = 10, resolution = c(0.1, 0.1),
              xmn = 0, xmx = 1, ymn = 0, ymx = 1,
              vals = runif(100)*10)

coord <- data.frame(x = runif(10), y = runif(10))
spoint <- sf::st_as_sf(coord, coords = c("x", "y"))
st_crs(spoint) <- projection(rp1)
spoint.sp <- as(spoint, "Spatial")

extract.points <- fast_extract(spoint, list(rp1 = rp1, rp2 = rp2))
extract.points.raster <- raster::extract(stack(rp1, rp2), spoint.sp, df = TRUE)[, -1]


###################################################################


test_that("output is a sf dataframe of appropriate dimensions", {
  expect_is(rgis.output.noparallel, "sf")
  expect_equal(dim(rgis.output.noparallel), c(2, 3))
  expect_equal(dim(extract.points), c(10, 3))
})


test_that("results from parallel and sequential runs match", {
  expect_equal(rgis.output.parallel, rgis.output.noparallel)
})


test_that("results from exact_velox_parallel match those from raster::extract", {
  # compared to raster::extract, rgis::fast_extract appends the raster name
  # to layer names. So, renaming the layers here to make them equal to raster::extract output
  # Here I just want to check values of extraction are consistent
  rgis.output.renamed <- sf::st_drop_geometry(rgis.output.noparallel)
  names(rgis.output.renamed) <- c("layer.1", "layer.2")
  row.names(rgis.output.renamed) <- NULL
  expect_equal(raster.output, rgis.output.renamed)

  ## Now for point sf:
  extract.points.renamed <- sf::st_drop_geometry(extract.points)
  names(extract.points.renamed) <- c("layer.1", "layer.2")
  expect_equal(extract.points.raster, extract.points.renamed)
})


test_that("names of extracted columns are correct", {
  expect_equal(names(sf::st_drop_geometry(rgis.output.noparallel)),
               c("ras_layer.1", "ras_layer.2"))
  expect_equal(names(sf::st_drop_geometry(rgis.out)),
               c("r1_layer", "r2_layer"))
})


test_that("extraction works with points sf", {
  expect_equal(extract.points$rp1_layer, c(0.133903331588954, 3.7212389963679, 7.12514678714797,
                                           2.67220668727532, 3.7212389963679, 2.05974574899301, 6.5087046707049,
                                           8.27373318606988, 2.655086631421, 3.7212389963679))
  expect_equal(extract.points$rp2_layer, c(5.11169783771038,
                                           3.53197271935642, 1.9126010988839, 7.54820944508538, 3.53197271935642,
                                           9.76170694921166, 8.80619034869596, 9.28615199634805, 6.54723928077146,
                                           3.53197271935642))
})