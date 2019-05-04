context("Test spatial files pre-processing")

# test soysambu_boundaries
test_that("soysambu_boundaries is in right projection and sf class", {
  expect_equal(sf::st_crs(soysambu_boundaries)$epsg, 32736)
  expect_is(soysambu_boundaries, "sf")
})

# test soysambu_grid and breaks for sampling
test_that("soysambu_grid is correctly setup", {
  expect_equal(unique(diff(xbreaks)), 3000)
  expect_equal(unique(diff(ybreaks)), 3000)
  expect_is(soysambu_grid, "sfc_POLYGON")
})

# test soysambu raster file from Sentinel
sentinel=raster::raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel.tif")

test_that("soysambu sentinel file has right projection and resolution", {
  expect_equal(
    raster::crs(sentinel)@projargs,
    "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  expect_equal(raster::res(sentinel), c(20,20))
})

# test landuse file: should have same characteristics as Sentinel file
landuse=raster::raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/landuse.tif")

test_that("landuse file is similar to Sentinel file, from which it has been derived",{
  expect_equal(
    raster::crs(landuse)@projargs,
    "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  expect_equal(
    raster::extent(sentinel), raster::extent(landuse))
})

# test ASTER file
aster=raster::raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/aster.tif")

test_that("aster file has same extent and projection as sentinel file", {
  expect_equal(
    raster::extent(sentinel), raster::extent(aster), tol=0.1)
  expect_equal(
    raster::crs(sentinel), raster::crs(aster))
})


# test lake mask for ASTER file
demx=raster::raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/aster_lakes.tif")

test_that("lake mask for elevation ASTER file is of correct projection, extent and values", {
  expect_equal(
    raster::extent(landuse), raster::extent(demx))
  expect_equal(
    raster::crs(sentinel), raster::crs(demx))
  expect_equal(
    diff(range(raster::values(demx), na.rm=T)), 0)  # there is only one value, so the diff of the range of values is 0.
})

# sentinel and aster files clipped with soysambu boundaries
sentc=raster::raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_sentinel.tif")
asterc=raster::raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_aster.tif")

test_that("the extent of both aster and sentinel files clipped to soysambu boundary files have the same extent and projection", {
  expect_equal(
    raster::extent(asterc), raster::extent(sentc), tolerance=0.001)
  expect_equal(
    raster::crs(asterc), raster::crs(sentc))
})

# roads
data("soysambu_roads")

test_that("extent and projection of soysambu roads matches with soysambu_boundaries",{
  expect_equal(raster::extent(soysambu_roads), raster::extent(soysambu_boundaries))
  expect_equal(raster::crs(soysambu_roads), raster::crs(soysambu_boundaries))
})

