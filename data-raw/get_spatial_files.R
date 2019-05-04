# set up =====================================

library(Soysambu)
library(tmap)

overwrite=T
path=here::here("inst","extdata","spatial")
namer= . %>% paste(path, ., sep="/") # helper function

# some useful crs:
# Lake Bogoria: 32636
utm36N = "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Lake Elmenteita: 32736
utm36S = "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# GPS: 4326
lonlat = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Google: 3857
# Google Maps uses its own version of a Mercator projection: EPSG=3857.
# https://en.wikipedia.org/wiki/Web_Mercator_projection
# However applying that projection does not give results: apparently Google returns lat lon WGS84.
merc = "+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs"


# get Soysambu boundaries -----------------------------------------

soysambu_boundaries = sf::st_read(here::here("inst","extdata","spatial","SoysambuShapeFiles","Soysambu_Boundary_WGS.shp"))
soysambu_boundaries = sf::st_zm(soysambu_boundaries, drop = T, what = "ZM")  # bug in leaflet which is used by tmap
# https://gis.stackexchange.com/questions/253898/adding-a-linestring-by-st-read-in-shiny-leaflet
soysambu_boundaries=sf::st_transform(soysambu_boundaries, 32736)


# store for package use
usethis::use_data(soysambu_boundaries, overwrite=overwrite)


# make sampling grid --------------------------------------------

# data(soysambu_boundaries) # see below

# set up offsets to match grid with boundaries
offset.xmin=650
offset.ymin=0
cell.size=3000

# make grid
xmin=sf::st_bbox(soysambu_boundaries)[1] - offset.xmin
ymin=sf::st_bbox(soysambu_boundaries)[2] - offset.ymin

soysambu_grid = sf::st_make_grid(soysambu_boundaries,
                             cellsize=cell.size,
                             offset=sf::st_bbox(c(xmin, ymin)))

# get breaks for the spatstat quadrat count

sf::st_coordinates(soysambu_grid) %>% as.data.frame %>% setDT -> br
xbreaks = unique(br, by="X")[, X]
ybreaks = unique(br, by="Y")[, Y]


tm_shape(soysambu_boundaries) + tm_borders() +
  tm_shape(soysambu_grid) + tm_borders()

# save
usethis::use_data(soysambu_grid, overwrite=overwrite)
usethis::use_data(xbreaks, overwrite=overwrite)
usethis::use_data(ybreaks, overwrite=overwrite)


# Sentinel file ===========================================

# Sentinel ---------------
sen = raster::raster(here::here("inst", "extdata", "spatial","L1C_T36MZE_A008841_20181115T075004.tif"))

#' https://earthexplorer.usgs.gov/
#' entity id = 	L1C_T36MZE_A008841_20181115T075004
#' acquisition date 2018-11-15T07:50:04 to 08:01
#' tile nr = T36MZE
#' Agency=ESA
#' Sentinel-2B
#' processing level LEVEL-1C
#' projection WGS84, UTM 36S, EPSG=32736
#' resolution= 10,20,60 m

# e = drawExtent()
e = new("Extent"
        , xmin = 844779.158303658
        , xmax = 868179.935254128
        , ymin = 9937457.72094529
        , ymax = 9963228.19682745
)

# ensure that this extent matches with extent of soysambu boundaries:
ex=raster::extent(soysambu_boundaries)

cx=pmax(matrix(e),matrix(ex)) %>% matrix(ncol=2,byrow=T)

# manual correction: give some extra space to W and S boundaries

cx[1,1]=845000
cx[2,1]=9938000
cx=raster::extent(cx)


# crop Sentinel file
sentinel = raster::crop(sen, cx)
raster::plot(sentinel)
plot(soysambu_boundaries, add=T)

raster::writeRaster(sentinel, filename=namer("sentinel.tif"), overwrite=overwrite)
rm(sen)


# cluster land use --------------

# clustering of land use, unsupervised
# from https://www.gis-blog.com/unsupervised-kmeans-classification-of-satellite-imagery-using-r/

clust.landuse = function(sentinel_file, cl, ...) {
  col.cl = viridis::viridis(cl, direction=-1)
  km = kmeans(sentinel_file[], centers = cl)
  res = raster::raster(sentinel_file[[1]])
  res = raster::setValues(res, km$cluster)

  raster::plot(res,col = col.cl)
  return(res)
}

cl = 4
set.seed(1234)
landuse = clust.landuse(sentinel, cl)

# assign classes to the raster file
landcover=c("open","forest","water","bush")
z=data.table::data.table(
  ID=1:cl,
  landcover=landcover
)
levels(landuse)[[1]]=z

# plot with landuse classes
cols=viridis::viridis(cl, direction=-1)
plot(landuse, col=cols, legend=F)
legend(x="bottomright", legend=landcover, fill=cols, border=F)

# can be done directly in rasterVis
# rasterVis::levelplot(landuse, col.regions=cols)

plot(sf::st_geometry(soysambu_boundaries), add=T, lwd=4)

raster::writeRaster(landuse, file=namer("landuse.tif"), overwrite=overwrite)

# ASTER elevation ================================================

dem = raster::raster(here::here("inst","extdata","spatial","ASTGTM2_S01E036_dem.tif"))

# project extent from Sentinel to lonlat so object can be cropped:
proj=raster::projection(dem)
e=raster::projectExtent(sentinel, crs=proj)     # get extent from Sentinel in lonlat projection
dem = raster::crop(dem, e)                      # crop to extent of Sentinel file
aster = raster::projectRaster(dem, crs=utm36S)  # reproject to UTM36S, as used in Sentinel

raster::writeRaster(aster, filename=namer("aster.tif"), overwrite=overwrite)
rm(dem)


# water mask for ASTER elevation file ----------------------------------------------

# water not recognized somehow in the altitude file - add from land use file

land=data.table::copy(landuse)
land[land != 3]=NA
plot(land)

# extent for Lake Elmenteita
# e=raster::drawExtent()
dput(e)
elm=new("Extent", xmin = 856120.143059182, xmax = 865894.539273281,
                ymin = 9945787.96453438, ymax = 9956248.28328982)

# extent for Lake Nakuru
# n=raster::drawExtent()
nak=new("Extent", xmin = 845008.197889469, xmax = 847957.664817232,
        ymin = 9954533.47693647, ymax = 9963176.10095735)

# manual correction: xmin and ymax must match with sentinel extent (upper left corner):
xmin=raster::extent(sentinel)@xmin
ymax=raster::extent(sentinel)@ymax
m=matrix(raster::extent(nak), nrow=2,byrow=T)
m[1,1]=xmin
m[2,2]=ymax
nak=m

# merge
newraster=raster::raster(raster::extent(land))
newraster=raster::resample(newraster, landuse)
nakx=raster::crop(land, nak)
elmx=raster::crop(land, elm)
lake.nakuru=raster::merge(newraster, nakx)
lake.elmenteita=raster::merge(newraster, elmx)
both.lakes=raster::merge(lake.nakuru, lake.elmenteita)

warning("overwrite was set to false here; the water level number can change via the clustering operations.")
# raster::writeRaster(both.lakes, filename=namer("aster_lakes.tif"), overwrite=F)
# raster::writeRaster(lake.elmenteita, filename=namer("lake_elmenteita.tif"), overwrite=F)
#

# conservancy boundary mask ------------------------------------------

# make a file that clips the boundaries of the conservancy:
conservancy = fasterize::fasterize(sf=soysambu_boundaries, raster=sentinel)
# cons_dem = fasterize::fasterize(soysambu_boundaries, aster)  # ?

# clip sentinel
conservancy_sentinel=raster::mask(sentinel, conservancy)
raster::writeRaster(conservancy_sentinel, file=namer("conservancy_sentinel.tif"), overwrite=overwrite)

# clip landuse
conservancy_landuse = raster::mask(landuse, conservancy)
raster::writeRaster(conservancy_landuse, file=namer("conservancy_landuse.tif"), overwrite=overwrite)

# clip aster
cons_dem = raster::mask(aster, cons_dem)
raster::writeRaster(cons_dem, file=namer("conservancy_aster.tif"), overwrite=overwrite)


# basemaps ------------------------------------------------------

soysambu_bing = tmaptools::read_osm(tmaptools::bb(cons_dem), type="bing")
soysambu_osm = tmaptools::read_osm(tmaptools::bb(cons_dem), type="osm")
soysambu_stamen_terrain = tmaptools::read_osm(tmaptools::bb(cons_dem), type="stamen-terrain")
soysambu_stamen_toner = tmaptools::read_osm(tmaptools::bb(cons_dem), type="stamen-toner")

raster::writeRaster(soysambu_bing, file=namer("soysambu_bing.tif"), overwrite=overwrite)
raster::writeRaster(soysambu_osm, file=namer("soysambu_osm.tif"), overwrite=overwrite)
raster::writeRaster(soysambu_stamen_terrain, file=namer("soysambu_stamen_terrain.tif"), overwrite=overwrite)
raster::writeRaster(soysambu_stamen_toner, file=namer("soysambu_stamen_toner.tif"), overwrite=overwrite)


# roads ==================================================================

kenya_roads = sf::st_read(here::here("inst","extdata","spatial","roads","Kenya_roads_version2.shp"))
kenya_roads = sf::st_transform(kenya_roads,crs=32736)

box=sf::st_bbox(soysambu_boundaries)
soysambu_roads = sf::st_crop(kenya_roads, box)

usethis::use_data(soysambu_roads, overwrite=overwrite)

# with lake: add a buffer around soysambu boundaries
# buffer is intentionally taken large, you may have to clip later on
box.lake=sf::st_buffer(soysambu_boundaries, dist=5000) %>% sf::st_bbox()
soysambu_roads_lake=sf::st_crop(kenya_roads, box.lake)

usethis::use_data(soysambu_roads_lake, overwrite=overwrite)


# test ==============
# library(tmap)
#
# data(gps_points)
# snares = gps_points[!is.na(gps_points$live.dead), ]
# snares$cat = with(snares, stringr::str_to_title(snares$cat))
# catidx = which(colnames(snares)=="cat")
# colnames(snares)[catidx] = "Snare type"
#
# cons_slope=raster::terrain(cons_dem, opt="slope")
#
# tm_shape(cons_dem) + tm_raster() +
#   tm_shape(lake_elmenteita) + tm_raster(palette="Blues", legend.show=F) +
#   tm_shape(soysambu_boundaries) + tm_borders(lwd=2) +
#   tm_shape(snares) + tm_dots(size=0.2, shape=0)
#
# tm_shape(cons_slope) + tm_raster() +
#   tm_shape(lake_elmenteita) + tm_raster(palette="Blues", legend.show=F) +
#   tm_shape(soysambu_boundaries) + tm_borders(lwd=2) +
#   tm_shape(snares) + tm_dots(size=0.2, shape=0)
#
# tm_shape(cons_dem) + tm_raster(palette=viridis(cl)) +
#   tm_shape(lake_elmenteita) + tm_raster(palette="Blues", legend.show=F) +
#   tm_shape(soysambu_boundaries) + tm_borders(lwd=4) +
#   tm_shape(snares) + tm_dots(size=0.1,shape="Snare type", shapes=c(0,1,2,3))
#
# tm_shape(conservancy) + tm_raster(palette=viridis(cl), style="cat", labels=c("Water", "Trees", "Bushes", "Open"), title="Vegetation") +
#   tm_shape(lake_elmenteita) + tm_raster(legend.show=F, palette=viridis(cl)) +
#   tm_shape(soysambu_boundaries) + tm_borders(lwd=4, col="black") +
#   tm_shape(snares) + tm_dots(size=0.1,shape="Snare type", shapes=c(0,1,2,3), title="Snare type")
