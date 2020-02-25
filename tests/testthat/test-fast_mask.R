context("Test fast_mask")

library(raster)
library(rgis)

size <- 10
ras <- raster(ncol = size, nrow = size, vals = rep(1, size*size))
msk <- raster(ncol = size, nrow = size, vals = 1:ncell(ras))
msk[1:(size/2), 1:(size/2)] <- NA

ras.masked1 <- raster::mask(ras, msk)
ras.masked2 <- fast_mask(ras, msk)


msk.sp <- as(msk, 'SpatialPolygonsDataFrame')
msk.sf <- sf::st_as_sf(msk.sp)
ras.masked3 <- raster::mask(ras, msk.sf)
ras.masked4 <- fast_mask(ras, msk.sf)

test_that("values from fast_mask match raster::mask when mask is a raster", {
  expect_equal(values(ras.masked1), values(ras.masked2))
})


test_that("values from fast_mask match raster::mask when mask is a polygon sf", {
  expect_equal(values(ras.masked3), values(ras.masked4))
})
