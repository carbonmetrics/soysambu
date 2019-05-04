context("Test ppp files")

# set up ----------------------------------------------
data(snares)
data(snares_lake)
data(gps_points)
data(soysambu_roads_psp)
data(soysambu_roads_lake_psp)

s=Soysambu::sf_dt(gps_points)
s=s[!is.na(live.dead), ]
n=nrow(s)
N=s[, sum(nr, na.rm=T)]

notsnares=subset(snares, cat=="WAYPOINT")

km2=201507048/1e6

# testing --------------------------------------------

# snares pointpattern
test_that("snares object is correctly set up", {
  expect_is(snares, "ppp")
  expect_equal(spatstat::npoints(snares), n)
  expect_equal(sum(spatstat::marks(snares)$snares), N)
  expect_equal(spatstat::area(spatstat::Window(snares))/1e6, 201.507, tolerance=0.001)
  expect_equal(spatstat::unitname(snares)$singular, "metre")
  expect_equal(nrow(data.frame(notsnares)), 0)
  expect_false(any(duplicated(snares)))
})

# ditto with lake included
test_that("snares lake is correctly set up", {
  expect_is(snares_lake, "ppp")
  expect_equal(spatstat::npoints(snares_lake), n)
  expect_equal(sum(spatstat::marks(snares_lake)$snares), N)
  expect_gt(spatstat::area(spatstat::Window(snares))/1e6, 201.507)
  expect_equal(spatstat::unitname(snares)$singular, "metre")
})

# roads soysambu
test_that("roads soysambu are correctly set up", {
  expect_is(soysambu_roads_psp, "psp")
  expect_is(soysambu_roads_lake_psp, "psp")
  # expect_equal(spatstat::Window(snares_lake), spatstat::Window(soysambu_roads_lake_psp))
})

