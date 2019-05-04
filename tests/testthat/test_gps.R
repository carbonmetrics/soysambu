context("Test gps files")

# set up
elm.epsg= 32736
utm36S = "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
sentc=raster::raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_sentinel.tif")

s=sf_dt(gps_points)
s=s[!is.na(live.dead), ]
n=nrow(s)
N=s[, sum(nr, na.rm=T)]
notsnares=sf_dt(gps_points)[cat %nin% c("NECK SNARE","GROUND SNARE","GUINEA FOWL TRAP") & !is.na(live.dead)]

# gps_points
test_that("GPS points have right extent and projection, and more than x snares", {
  expect_equal(sf::st_crs(gps_points)$epsg, elm.epsg)
  expect_equal(sf::st_crs(gps_points), sf::st_crs(soysambu_boundaries))
  expect_equal(nrow(s), n)    # nr of snare positions found
  expect_equal(sum(s$nr), N)  # total nr of snares found
})

# gps_tracks
test_that("GPS tracks have right extent and projection", {
  expect_equal(sf::st_crs(gps_tracks)$epsg, elm.epsg)
  expect_equal(sf::st_crs(gps_tracks), sf::st_crs(soysambu_boundaries))
})

# points of interest
test_that("points of interest have right extent and projection", {
  expect_equal(sf::st_crs(poi), sf::st_crs(elm.epsg))
  expect_equal(sf::st_crs(poi), sf::st_crs(soysambu_boundaries))
  expect_equal(sort(unique(poi$type)), c("area","community","gate","lake","lodge","lookout","office","settlement","town"))
})

