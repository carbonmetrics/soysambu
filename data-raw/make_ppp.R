# Make a spatstat ppp object out of snaring locations (points) and the soysambu boundaries (window)

# set up -------------------------------------------
libs=c("Soysambu", "spatstat", "maptools", "sf", "raster", "maditr", "data.table")
lapply(libs, library, character.only=T, quietly=T, verbose=F)
# without maptools, as.owin will give an error:
# https://stackoverflow.com/questions/19183105/error-in-as-owin-handling-shape-files-in-the-spatstat-package-r

overwrite=T

# make ppp object ------------------------------------------

# only snares
snares = gps_points[!is.na(gps_points$live.dead),]

# x and y vectors
xvec = st_coordinates(snares)[,1]
yvec = st_coordinates(snares)[, 2]

# window
o = as(soysambu_boundaries, "Spatial") %>% as.owin

# ppp object
p = ppp(xvec, yvec, window = o)
unitname(p) = c("metre", "metres")

marks(p) = data.frame(
  idx = snares$idx,
  id.dup = snares$id.dup,
  cmt = snares$cmt,
  name = snares$name,
  ele = snares$ele,
  label = snares$label,
  cat = snares$cat,
  snares = snares$nr,
  live_dead = snares$live.dead,
  datetime = snares$time,
  date = as.Date(snares$time)
)

# marks(p) = nndist(p)
snares=p

# store
usethis::use_data(snares, overwrite=overwrite)


# ------------------------------------

# repeat exercise for tracks

data(gps_tracks)

xvec_t = st_coordinates(gps_tracks)[,1]
yvec_t = st_coordinates(gps_tracks)[,2]

pt = ppp(xvec_t, yvec_t, window=o)
unitname(pt)=c("metre", "metres")
pointpattern_tracks = pt

usethis::use_data(pointpattern_tracks, overwrite=overwrite)


# --------------------------------------

# add lake window: this is important for edge effect analysis
# the lake functions as an extended boundary.

elm = raster::raster(here::here("inst", "extdata", "spatial", "lake_elmenteita.tif"))
lake = raster::rasterToPolygons(elm) %>% as.owin
A = Window(snares)
joint=union.owin(lake, A)
plot(joint)  # dirty edges due to unprecise shapefile / changed water levels

# z=clickpoly(add=T)
# > dput(z)
# z is nothing else than a polygon to cover messy patches where lake and soysambu boundaries do not quite match.
# the artifarcts are probably due to rising and falling water levels.
z=structure(list(type = "polygonal", xrange = c(857584.133613333,
                                              861926.427233139), yrange = c(9947426.60503242, 9955122.94613787
                                              ), bdry = list(structure(list(x = c(861267.59647703, 861597.011855085,
                                                                                  861926.427233139, 861237.64962448, 860848.340541325, 861207.70277193,
                                                                                  857584.133613333, 858841.901420449), y = c(9947935.70152578,
                                                                                                                             9948864.05395484, 9950421.29028746, 9951679.05809458, 9954703.69020216,
                                                                                                                             9955122.94613787, 9955033.10558022, 9947426.60503242)), .Names = c("x",
                                                                                                                                                                                                "y"))), units = structure(list(singular = "unit", plural = "units",
                                                                                                                                                                                                                               multiplier = 1), .Names = c("singular", "plural", "multiplier"
                                                                                                                                                                                                                               ), class = "unitname")), .Names = c("type", "xrange", "yrange",
                                                                                                                                                                                                                                        "bdry", "units"), class = "owin")
A2=union.owin(lake, z, A)
snares_lake=copy(snares)
Window(snares_lake)=A2
# plot(snares_lake, use.marks=F)

unitname(snares_lake)=c("metre", "metres")

usethis::use_data(snares_lake, overwrite=overwrite)

# -------------------------------------

# make psp object: lines from Kenya roads, narrowed down to Soysambu.
# see ./data-raw/get_spatial_files.R -> soysambu_roads

s = soysambu_roads %>%
  sf::as_Spatial() %>%
  as("psp")

unitname(s)=c("metre", "metres")

s=as.psp(s) # get rid of duplicates

unitname(s)=c("metre", "metres")

plot.psp(s, show.all=F, style="none")

soysambu_roads_psp = s
usethis::use_data(soysambu_roads_psp, overwrite=overwrite)


# ditto for the boundaries of the lake included ------

require(maptools)

sl = soysambu_roads_lake %>%
  sf::as_Spatial() %>%
  as("psp")

unitname(sl)=c("metre", "metres")

# clip the window to the same size as that of snares_lake obtained above
# Window(sl)=Window(snares_lake)

soysambu_roads_lake_psp=sl

# the windows of snares_lake and soysambu_roads_lake_psp must be the same
# Window(snares_lake)
# Window(soysambu_roads_lake_psp)=Window(snares_lake)

usethis::use_data(soysambu_roads_lake_psp, overwrite=overwrite)
