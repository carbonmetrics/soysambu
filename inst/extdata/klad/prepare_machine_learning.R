  # Prepare machine learning

# set up ===============================

pacman::p_load(data.table, sf, sp, raster, spatstat, maditr, maptools, viridis, dismo, Soysambu)
overwrite=T

# load data and select ----------------
# load data from collect_machine_learning
b=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/brickfile.grd")


# select predictors ------------------

pred=subset(b, c("ASTER","slope","TRI","SAVI","open_areas",
                 "dist_centroid", "dist_bound","dist_roads","distances", "hotspot_buffered",
                 "interface_class", "interface_raw"))

# take sample ========================

# sample = background points
nsample=500
bg=randomPoints(subset(b, "SAVI"), nsample)
bgp=data.table(bg)[, label := "background"]

# snares = presence points

pr=ppp_dt(snares)[cat=="NECK SNARE",][, .(x,y)]
pr[, label := "presence"]


# together
p=rbind(bgp, pr)

rm(bg, bgp, pr)


# extract =================================

# extract function
extract_function=function(brickfile, layername, coords, buffer=NULL, method, fun=NULL, na.rm=T) {
  r=subset(brickfile,layername)
  e=raster::extract(r, coords, buffer=buffer, fun=fun, method=method)
  return(e)
}

extracting=curry(extract_function, brickfile=pred, coords=p[, .(x,y)], method="simple")
extracting_bilinear=curry(extract_function, brickfile=pred, coords=p[, .(x,y)], method="bilinear")

# get values for both snares and background sample
p[, `:=` (
  ASTER=extracting("ASTER"),
  slope=extracting("slope"),
  TRI=extracting("TRI"),
  open_areas=extracting_bilinear("open_areas", fun=mean),
  SAVI=extracting("SAVI", buffer=20),
  hotspot_buffered={
    t1=extracting("hotspot_buffered")
    t2=ifelse(is.na(t1),0,t1)
  },
  dist_roads=extracting("dist_roads"),
  dist_bound=extracting("dist_bound"),
  distances=extracting("distances"),
  dist_centroid=extracting("dist_centroid"),
  interface_raw=extracting("interface_raw"),
  interface_class=extracting("interface_class")
)]

# select variables
myvars=c("label", "dist_roads", "dist_bound", "open_areas", "SAVI")

pvars=copy(p)[label=="presence", myvars, with=F][, label := NULL]


# modelling ================================

sp=SpatialPoints(p[label=="presence", .(x,y)], CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

predraster=subset(pred, myvars[myvars != "label"])

# BIOCLIM
# bc=bioclim(predraster, sp)
bc=domain(predraster,sp)

response(bc)
pairs(bc)

# make plot of prediction: combine a brick with predictors and the model object
# predbrick=subset(pred, myvars[myvars != "label"])
predplot = predict(predraster, bc)
# predplot[predplot<0.6]=NA

par(mfrow=c(1,2))
predplot=aggregate(predplot, fact=4)
predplot[predplot<0.5]=NA
plot(predplot, col=viridis(255, direction=-1))
plotstuff()
plot(predplot, col=viridis(255, direction=-1))
# plot(pointpattern_tracks, cex=0.05, pch=1, add=T)
plotstuff(plot.snares=T, cex=0.3, pch=0, col="black")

library(ecospat)
ecospat.boyce(fit=predplot, obs=as.data.frame(sp))
