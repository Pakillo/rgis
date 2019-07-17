context("Test extract_velox_parallel")

library(raster)
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

rgis.output.parallel <- rgis::extract_velox_parallel(sf = polys.sf, ras = ras, parallel = TRUE)

rgis.output.noparallel <- rgis::extract_velox_parallel(sf = polys.sf, ras = ras, parallel = FALSE)

###


test_that("output is a sf dataframe of appropriate dimensions", {
  expect_is(rgis.output.noparallel, "sf")
  expect_equal(dim(rgis.output.noparallel), c(2, 3))
})


test_that("results from parallel and sequential runs match", {
  expect_equal(rgis.output.parallel, rgis.output.noparallel)
})


test_that("results from exact_velox_parallel match those from raster::extract", {
  expect_equal(raster.output, sf::st_drop_geometry(rgis.output.noparallel))
})

