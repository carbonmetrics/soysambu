#' POI = Points of Interest:
#' lodges, gates, offices, communities


# set up
pacman::p_load(data.table, sf, usethis)
overwrite=T

# Google Maps uses its own version of a Mercator projection: EPSG=3857.
# https://en.wikipedia.org/wiki/Web_Mercator_projection

# Points of Interest =============================

p = data.table::data.table(
  waypoint = c("Lake Elmenteita",
               "Elmenteita",
               "Elmenteita & \nMbweha Gate",
               "Diatomite Gate",
               "Melia Gate",
               "Kokoto Gate",
               "Main Gate",
               "Lake Elmenteita \nSerena Camp",
               "Baruk Stores",
               "Jolai Gate",
               "Utut Gate",
               "Lookout",
               "Mbweha Camp",
               "Head Office",
               "Nairobi",
               "Nakuru",
               "Sleeping Warrior lodge",
               "Melia Plains",
               "Congreve Mbweha",
               "Jolai Quarry",
               "Triangle",
               "Nyumba Mbili",
               "Mwariki",
               "Sleeping Warrior"
  ),
  lat = c(-0.437,
          -0.487444,
          -0.479281,
          -0.432594,
          -0.375893,
          -0.359312,
          -0.385162,
          -0.404960,
          -0.401468,
          -0.487114,
          -0.480755,
          -0.391041,
          -0.478556,
          -0.470803,
          -1.280633,
          -0.281808,
          -0.531602,
          -0.396549,
          -0.460568,
          -0.515225,
          -0.373289,
          -0.380076,
          -0.429286,
          -0.500918

  ),
  lon = c(36.240,
          36.156650,
          36.157124,
          36.156413,
          36.158136,
          36.201982,
          36.236729,
          36.241092,
          36.219513,
          36.192471,
          36.245618,
          36.192180,
          36.117691,
          36.195356,
          36.816520,
          36.085008,
          36.232002,
          36.166163,
          36.137515,
          36.168585,
          36.240892,
          36.216617,
          36.163914,
          36.234456
  ),
  type = c(
    "lake",
    "settlement",
    "gate",
    "gate",
    "gate",
    "gate",
    "gate",
    "lodge",
    "settlement",
    "gate",
    "gate",
    "lookout",
    "lodge",
    "office",
    "town",
    "town",
    "lodge",
    "area",
    "area",
    "area",
    "area",
    "area",
    "area",
    "area"
  ),
  day=c(NA, NA, 1, 1, 1, 1, 2, NA, 0, 1, 2, 1, NA, 1+0+5, NA, NA, NA, 4, 2, 4, 2, 4, 2, 0),
  night=c(NA, NA,1, 0, 0, 2, 2, NA, 1, 1, 2, 2, NA, 2+1+4, NA, NA, NA, 0, 1, 0, 0, 0, 0, 0)
)

# community interviews =========================================

comm = data.table::data.table(
  waypoint=c(
    "Mwariki ABC",
    "Elmenteita",
    "Kiwanja Swara",
    "Mbaruk Centre",
    "Ol Jolai",
    "Kikopey",
    "Kiungururia",
    "Kongasis",
    "Gilgil"
  ),
  lat=c(
    -0.446140,
    -0.489208,
    -0.523109,
    -0.353501,
    -0.562500,
    -0.420683,
    -0.350745,
    -0.553414,
    -0.502881
  ),
  lon=c(
    36.148162,
    36.156693,
    36.113441,
    36.211618,
    36.213573,
    36.257033,
    36.180606,
    36.150898,
    36.319044
  ),
  type=c(
    "community",
    "community",
    "community",
    "community",
    "community",
    "community",
    "community",
    "community",
    "community"
  )
)

comm[, `:=` (day=NA, night=NA)]
p=rbind(p,comm)

# day and night refer to staffing of askaris, see interview.xlsx.

# Google Maps uses its own version of a Mercator projection: EPSG=3857.
# https://en.wikipedia.org/wiki/Web_Mercator_projection
# However applying that projection does not give results: apparently Google returns lat lon WGS84.

poi = sf::st_as_sf(p, coords=c("lon", "lat"), crs=4326)
poi=sf::st_transform(poi, crs=32736)

# test
# tm_shape(soysambu_boundaries) + tm_borders() + tm_shape(poi) + tm_dots(size=1, col="type")

usethis::use_data(poi, overwrite=overwrite)
