#' Collect all required data for analysing snare patterns
#' Put all info together in raster brick.

# setup ===========================================================================

# libraries
pacman::p_load(Soysambu, raster, spatstat, maditr, data.table, rgeos, dynatopmodel, viridis, fasterize, nngeo)
setwd("/home/henk/Documents/PhD/Soysambu/Soysambu")


# other
overwrite=T

# load data
aster=raster::raster("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_aster.tif")
sentinel=raster::raster("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_sentinel.tif")
landuse=raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_landuse.tif")

s=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel10.grd")
scc=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel10_clust.grd")
sc=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel10_cons.grd")

# conservancy mask for clipping
conservancy = fasterize::fasterize(sf=soysambu_boundaries, raster=s[[1]])


# extract info from sentinel ====================================================

# land use --------------------------------------------------------------------

# work with SAVI - Soil Adjusted Vegetation Index
# this is the clustered version, see get_sentinel.R
savi=subset(scc, "SAVI")
landcover=c("Water", "Forest", "Dense forest", "Plains", "Shrubs")
z=data.table::data.table(
  ID=0:4,
  landcover=landcover
)
levels(savi)[[1]]=z
plot(savi, col=viridis(5), legend=F)
legend(x="bottomright", legend=landcover, fill=viridis(5), border=F)

# find the interface between open land and bush -------------------------------

# find open plains and aggregate
l=layerize(savi)
open=subset(l, "X4")

# invert open areas -> non open is considered to be "bush"
# so this is not the same as taking the X5 layer from l (layerize)
bush=spatialEco::raster.invert(open)

# weighing matrix
w = matrix(rep(1,121), ncol=11, nrow=11) # observation window

# find interface
interface=focal(bush, w=w, fun=mean, pad=F)
plot(interface, col=viridis(3))
breaks=c(0, 0.25, 0.75, 1)
icut=cut(interface, breaks=breaks)


# distances -----------------------------------------------------

# get distances from the conservancy boundaries ----------------

# get raster with elmenteita, cropped to boundaries+lake
# s=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel10.grd")
r1=subset(s, "SAVI")
r2=subset(sc, "SAVI")
elm=raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/lake_elmenteita.tif")
elm=raster::resample(elm, r1)
r3=merge(r2,elm)

# patch 1
e1=new("Extent", xmin = 859697.590724758, xmax = 861402.176354373,
       ymin = 9947803.98980971, ymax = 9949016.86804617)
r4=raster(e1, crs= "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
r4[]=99
r4=raster::resample(r4,r3)

# patch 2
e2=new("Extent", xmin = 857402.956223355, xmax = 860090.956639285,
       ymin = 9947574.52635957, ymax = 9954917.35676406)
r5=raster(e2, crs="+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
r5[]=99
r5=raster::resample(r5,r3)

# merge patches
r=merge(r3,r4,r5)
r[!is.na(r)]=-1

# inverse
r[is.na(r)]=1
r[r < 1]=NA

# distance from boundaries
dist_bound=distance(r)
dist_bound=mask(dist_bound,conservancy)


# distance from roads -----------------------------------------------------------

roads=rasterize(soysambu_roads_lake, r)
dist_roads=distance(roads)
dist_roads=mask(dist_roads,conservancy)

# take the minimum of distances {roads, boundaries} ----------------------------
dist=overlay(dist_bound, dist_roads, fun=min)

distances=brick(dist_bound, dist_roads, dist)
names(distances)=c("dist_bound","dist_roads","distances")
# distances=crop(distances, savi)
distance=mask(distances, conservancy)


# extract info from ASTER ======================================================

# slope, roughness ------------------------------------------------------------
slope=terrain(aster, opt="slope")
tri=terrain(aster,opt="TRI")


# topographic wetness --------------------------------------------------------

sl=raster(extent(aster),
          res=c(31,31),
          crs="+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
slx=raster::resample(aster, sl)
b=dynatopmodel::build_layers(slx)
TWI=subset(b, "atb")


# extract info from pointpattern ==============================================

# location of hotspots ------------------------------------------------------

min.snares=5                    # number of snares you want to have connected before it is a hotspot
radius=300                           # distance to the next hotspot
cc=connected(snares, R=radius*2)     # which hotspots are connected if they are dilated with diameter R?
s=split(cc)                     # split into separate hotspot areas
np=sapply(s,npoints)            # count the hotspots
X=s[np<=min.snares]             # remove the hotspots with 3 snares or less
X=unmark(superimpose(X))        # trick to glue the split object s together again
Y=s[np>min.snares]              # choose number of snares per hotspot
Y=unmark(superimpose(Y))        # create hotspots with at least 3 snares
# plot(dilation(X, r),main="", col="orange")  # these are the points that were thrown away

# plot
plotstuff(add=F)
plot(dilation(Y, radius), add=T)    # plot the hotspots
stienen(Y, add=T, bg="white")  # add stienen dilation


# get the hotspots into a raster ------------------------------------------

hs=dilation(Y,radius)                # see radius above
im=spatstat::as.im(hs)
spatstat::Window(im)=spatstat::Window(snares)
r=raster::raster(im, crs="+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
clumps=raster::clump(r)
pol = raster::rasterToPolygons(clumps, dissolve=T)
polsf=sf::st_as_sf(pol)

# get hotspot raster, clip to conservancy
hotspots=fasterize::fasterize(polsf, raster=savi)
hotspots[is.na(hotspots)]=0
hotspots=mask(hotspots, conservancy)


# add a buffer around the hotspots ---------------------------------------
hotspot_buffer = polsf %>%
  st_convex_hull %>%
  st_buffer(100) %>%
  st_union %>%
  st_intersection(soysambu_boundaries) %>%
  st_simplify

# get hotspot buffer raster, clip to conservancy
hotspot_buffer=as(hotspot_buffer, "Spatial")
hotspot_buffer=rasterize(hotspot_buffer, savi)

hotspot_buffer[is.na(hotspot_buffer)]=0
hotspot_buffer=mask(hotspot_buffer, conservancy)


# by the way... ---------------------------------------------------------

# what is the area of the hotspots?
area.m2=st_area(polsf)
area.km2=area.m2/1e6
fivenum(area.km2)

# as percentage of Soysambu?
soysambu.km2=area(Window(snares))/1e6
round(sum(area.km2/soysambu.km2)*100, 1) %>% as.integer

# get centroids of clusters --------------------------------------------

cluster_centroids=rgeos::gCentroid(pol, byid=T)
usethis::use_data(cluster_centroids, overwrite=overwrite)

# make a raster where you have the distances to the centroids everywhere --------

# coords=st_coordinates(cluster_centroids)
dist_centr=distanceFromPoints(savi, cluster_centroids)
dist_centr=mask(dist_centr, conservancy)


# put everything into a brick file ======================================

# sentinels ------------------------------------------------------------

sentinel=subset(sc, "natural")
brick(sentinel, savi, open, bush, interface, icut)
sentinels=brick(sentinel,savi,open,bush,icut,interface)
names(sentinels)=c("sentinel", "SAVI", "open_areas", "bush", "interface_class", "interface_raw")


# asters --------------------------------------------------------------
asterb=raster::brick(aster, slope, tri)
asterc=raster::crop(asterb, savi)
asterr=raster::resample(asterc,savi)
TWI=raster::resample(TWI, savi)

asters=raster::addLayer(asterr, TWI)
names(asters)=c("ASTER", "slope","TRI", "TWI")

# hotspots -----------------------------------------------------------

hotspot_brick=raster::brick(hotspots, hotspot_buffer, dist_centr)
names(hotspot_brick)=c("hotspots", "hotspot_buffered", "dist_centroid")


# all together now --------------------------------------------------

brickfile=addLayer(asters, sentinels, hotspot_brick, distances)


# write to disk =================================================

writeRaster(brickfile, file='./inst/extdata/spatial/brickfile.grd', overwrite=overwrite)
usethis::use_data(cluster_centroids, overwrite=overwrite)
