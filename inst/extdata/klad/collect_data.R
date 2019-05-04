# collect information for machine learning

library(Soysambu)
library(raster)
library(spatstat)
library(here)
library(viridis)
library(data.table)
library(rgdal)

cols=viridis(255)

# elevation

# load DEM and lake file
aster=raster("./inst/extdata/spatial/conservancy_aster.tif")
sentinel=raster("./inst/extdata/spatial/conservancy_sentinel.tif")
landuse=raster("./inst/extdata/spatial/conservancy_landuse.tif")
elm=raster("./inst/extdata/spatial/lake_elmenteita.tif")

# landuse
# plot(landuse)
#
# sent=raster("./inst/extdata/spatial/sentinel.tif")
#
# h=stack("/home/henk/Documents/PhD/Soysambu/Sentinel/L1C_T36MZE_A016248_20180802T075248.tif")

# read Sentinel files
B02=readGDAL("/home/henk/Documents/PhD/Soysambu/Sentinel/S2A_MSIL2A_20190330T073611_N0211_R092_T36MZE_20190330T110227.SAFE/GRANULE/L2A_T36MZE_A019680_20190330T075523/IMG_DATA/R10m/T36MZE_20190330T073611_B02_10m.jp2")
B03=readGDAL("/home/henk/Documents/PhD/Soysambu/Sentinel/S2A_MSIL2A_20190330T073611_N0211_R092_T36MZE_20190330T110227.SAFE/GRANULE/L2A_T36MZE_A019680_20190330T075523/IMG_DATA/R10m/T36MZE_20190330T073611_B03_10m.jp2")
B04=readGDAL("/home/henk/Documents/PhD/Soysambu/Sentinel/S2A_MSIL2A_20190330T073611_N0211_R092_T36MZE_20190330T110227.SAFE/GRANULE/L2A_T36MZE_A019680_20190330T075523/IMG_DATA/R10m/T36MZE_20190330T073611_B04_10m.jp2")
B08=readGDAL("/home/henk/Documents/PhD/Soysambu/Sentinel/S2A_MSIL2A_20190330T073611_N0211_R092_T36MZE_20190330T110227.SAFE/GRANULE/L2A_T36MZE_A019680_20190330T075523/IMG_DATA/R10m/T36MZE_20190330T073611_B08_10m.jp2")

# make raster files
B02r=raster(B02)
B03r=raster(B03)
B04r=raster(B04)
B08r=raster(B08)

# put into brick, crop to extent sentinel and clip to soysambu_boundaries
# make brick
s=brick(B02r,B03r,B04r,B08r)
names(s)=c("B02","B03","B04","B08")

# crop to sentinel extent
e=raster::extent(sentinel)
sx=crop(s,e)

# crop blank corner upper left
# e=drawExtent() #, then via dput:
ex=new("Extent", xmin = 845530.298182036, xmax = 868260.776454163,
      ymin = 9938278.69957997, ymax = 9963090.52870191)
sx=crop(sx,ex)

# clean up
rm(B02,B03,B04,B08, B02r, B03r,B04r,B08r,e,ex)

# calculate NDVI
ndvi=calc(sx, fun=function(x) (x["B08"]-x["B04"]) / (x["B08"]+x["B04"]))
sx=addLayer(sx,ndvi)
names(sx)[5]="NDVI"

gc()

# # crop to conservancy boundaries
conservancy = fasterize::fasterize(sf=soysambu_boundaries, raster=sx[[1]])
# sy=raster::mask(sx,conservancy)

# classification

# https://rspatial.org/rs/4-unsupclassification.html

ndvi[is.na(ndvi)]=0
sx[is.na(sx)]=0
nr=getValues(sx)
set.seed(99)
kmncluster=kmeans(na.omit(nr),centers=4)
knr=ndvi
knr[]=kmncluster$cluster
knr=mask(knr,conservancy)


par(mfrow=c(1,2))
plot(knr,col=cols)
plot(landuse,col=cols)
par(mfrow=c(1,1))
