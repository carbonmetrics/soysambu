# machine learning

# set up ===============

pacman::p_load(raster,sf,spatstat,maditr,data.table,lubridate,stringr,maptools,sp,scales, viridis, Soysambu, rgeos)

conservancy = fasterize::fasterize(sf=soysambu_boundaries, raster=s[[1]])

# load data
aster=raster::raster("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_aster.tif")
sentinel=raster::raster("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_sentinel.tif")
landuse=raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_landuse.tif")

s=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel10.grd")
scc=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel10_clust.grd")
sc=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/sentinel10_cons.grd")


# savi1[is.na(savi1)]=0
savi1c=clustLanduse(subset(s, "SAVI"), cl=5)
savi1c=crop(savi1c, extent(sentinel))
plot(mask(savi1c, conservancy), col=viridis(5, direction=1), main="SAVI")
plotclusters(snares, 2, 0.9, add=T)
plot(gps_tracks, col="white", add=T, cex=0.1)
plotstuff(plot.snares=T, cex=0.6,col="red")

x=subset(sc, c("B4", "B8"))
SR=overlay(x, fun=function(x,y) {return(y/x)})
plot(SR, col=viridis(255, direction=-1)) # useless

arvi=clustLanduse(subset(s, "ARVI"), cl=4)
plot(mask(arvi, conservancy), col=viridis(4), main="ARVI")
plotclusters(snares, 2, 0.9, add=T)

# find centroids of clusters, put hotspots into raster
x=plotclusters(snares, 2, 0.9)
y=as.im(x)
spatstat::Window(y)=spatstat::Window(snares)
r=raster(y, crs="+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
clumps=clump(r)
pol = rasterToPolygons(clumps, dissolve=T)
# polx=aggregate(pol, dissolve=F)
centers=gCentroid(pol, byid=T)
plot(centers)
centers=data.frame(centers)
cl.centers=ppp(x=centers$x, y=centers$y, window=Window(snares))
d=dirichlet(cl.centers)
plot(d, col="orange")
plot(cl.centers, add=T, col="orange", pch=19)

plotstuff(plot.snares=T, cex=0.4)

plot(cl.centers, pch=3)
plotclusters(snares, 2, 0.9, add=T)

# continue with SAVI

l=layerize(savi1c)
bush=subset(l, "X4")
bush=aggregate(bush, fact=2)

f=curry(plotclusters, ppp=snares, k=2, quantile=0.9, add=T)
boundaries(bush, classes=T) %>% plot

f()

# bush=copy(landuse)
# l=layerize(landuse)
# bush=subset(l, "X2")

w = matrix(rep(1,121), ncol=11, nrow=11) # observation window
interface=focal(bush, w=w, fun=mean, pad=F)
plot(interface)
breaks=c(0, 0.2, 0.8, 1)
icut=cut(interface, breaks=breaks)
landcover=c("closed","interface","open")
z=data.table::data.table(
  ID=0:2,
  landcover=landcover
)
levels(icut)[[1]]=z

plot(icut,col=viridis(3))


cols=viridis::viridis(3, direction=1)
plot(icut, col=cols, legend=F)
legend(x="bottomright", legend=landcover, fill=cols, border=F)
f()

s=subset(snares, cat=="NECK SNARE")
px=ppp_dt(s)
py = sp::SpatialPoints(px[, 1:2], proj4string=crs(bush))

px[, i := extract(icut, py)]
plot(hist(px$i))

# ditto slope, roughness
slope=terrain(aster, opt="slope")
tri=terrain(aster,opt="TRI")

px[, slope := extract(slope, py)]
px[, TRI := extract(tri, py)]

# distance to roads and boundaries
px[, dist.roads := nncross(pp_lake, soysambu_roads_psp)$dist]
px[, dist.bounds := bdist.points(pp_lake)]

# distance to nearest settlement
px[, nndist := nndist(pp_lake)]

# stienen points

par(mfrow=c(1,2))
pp_lake %>% dilation(500) %>% plot
plotstuff(cex=0.5, plot.snares=T)
title("Steiner set")

pp_lake %>% stienen
plotstuff(cex=0.5)
title("Stienen set")
par(mfrow=c(1,1))

connected(dilation(pp_lake, 500)) %>% plot(box=F)
?plotstuff(cex=0.5, plot.snares=T)
idx=identify(snares)
ix=subset(snares, idx %in% ii$idx)
d=dirichlet(ix)
dd=tiles(d)

LR=scanLRTS(snares, r=500)
pvals=eval.im(pchisq(LR,df=1,lower.tail=F))
l=levelset(pvals, 0.01, "<")
plot(l)
plotstuff(plot.snares=T, col="steelblue")
plot(LR)

lu.im = spatstat::as.im(sentinel)   # fails if maptools is not loaded
dem.im = spatstat::as.im(aster)

stienen(snares)
plotstuff(plot.snares=T,cex=0.5)
plot(dilation(snares, 500),add=T, col=viridis(1,alpha=0.3))


