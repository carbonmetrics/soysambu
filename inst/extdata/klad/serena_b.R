#' Further hotspot analysis
#'
#' Using Baddeley p 187

# set up ================================

librarian::shelf(spatstat, data.table, sf, viridis,
                 maptools, raster,
                 dismo, Soysambu)

data(all_snares, soysambu_roads_psp, pointpattern, soysambu_roads)

aster=raster::raster("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_aster.tif")
sentinel=raster::raster("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/conservancy_sentinel.tif")
overlay = raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/soysambu_stamen_terrain.tif")


# tricky stuff:
raster::plot(sentinel)              # fails if raster namespace is not specified
lu.im = spatstat::as.im(sentinel)   # fails if maptools is not loaded
dem.im = spatstat::as.im(aster)     # ditto

po = poi %>%
  st_transform(crs=32736) %>%
  sf_dt()
po = po[type=="gate" | type=="office",]


# ---------------------

s = copy(sentinel)
s[s !=2] = NA  # plot kichaka only
plot(s)

# ---------------

# distance related measures
plot(distmap(pointpattern), col=viridis(255))
stienen(pointpattern, add=T, alpha=0.1)
plot(soysambu_roads_psp, add=T, style="none", show.all=F, col="steelblue", lwd=2)
plot(all_snares, use.marks=F, add=T, pch="+")


plot(all_snares, use.marks=F)
plot(soysambu_roads_psp, add=T, style="none", show.all=F, col="steelblue", lwd=2)

# select a region --------------------------------------
# serena_box=clickpoly(add=T)
# save(serena_box, file="~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/serena_box")

# load box ---------------------------------------------
load("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/serena_box")
serena=all_snares[serena_box]
plot(serena, use.marks=F)
plot(distmap(serena), col=viridis(255))
plot(soysambu_roads_psp, add=T)

# relation to roads around Elmenteita and main gate is different from park overall --------

# plot(all_snares, use.marks=F)
# elmenteita = spatstat::clickpoly(add=T)
# elm = all_snares[elmenteita]
# maingate=spatstat::clickpoly(add=T)
# maingate = all_snares[maingate]

roc(unmark(all_snares), distfun(soysambu_roads_psp), high=F) %>% plot
abline(a=0,b=1,col="grey50",lty=3)
auc.val = auc(unmark(all_snares), distfun(soysambu_roads_psp), high=F) %>% round(2)
title(
  main = "Soysambu snares correlation with roads",
  sub = paste("AUC value:", auc.val))

rocf = function(ppp, plottitle) {

  roc(unmark(ppp), distfun(soysambu_roads_psp), high=F) %>% plot
  abline(a=0, b=1, col="grey50", lty=3)
  auc.val = auc(unmark(all_snares), distfun(soysambu_roads_psp), high=F) %>% round(2)
  title(
    main = plottitle,
    sub = paste("AUC value:", auc.val))
}


# --------------

W = levelset(lu.im, 2, "==")
plot(W, col = "grey90", main = "", alpha= 0.1, add=F)
title(main = "Soysambu Conservancy", sub = "Shaded = acacia thickets, square = snare found")
plot(pointpattern, which.marks="cat", col = "darkblue", add = T, cex=0.8)
plot(soysambu_roads_psp, add=T, style="none", lwd=1)
contour(density(pointpattern), add=T)

# note how the snares are parallel to the road - this is the effect of doing the transect parallel,
# not necessarily how the poachers operate.

lr=scanLRTS(all_snares, r=2*bw.ppl(all_snares))
pvals=eval.im(pchisq(lr, df=1, lower.tail=F))
levelset(pvals, 0.01, "<") %>% plot
plot(all_snares, add=T, use.marks=F)
plot(window(all_snares), add=T)
plot(pvals, col=viridis(255))

all_snares %>%
  clusterset(what="domain", verbose=F, fast=T) %>%
  plot(col=c("grey95", "red"), main="hotspot analysis", add=F, legend=F, showall=F)
plot(all_snares, use.marks=F, add=T, col="red", cex=0.7)
plot(soysambu_roads_psp, add=T, lwd=1, show.all=F, style='none')

plot(window(all_snares), add=T)
window(all_snares) %>% plot
z = unmark(all_snares)

sharpen(unmark(all_snares), sigma=0.1, edgecorrect=T) %>% plot
plot(unmark(all_snares))

# -------------------------------------------------
# clustering using nnclean

Z=nnclean(pointpattern, 10, plothist=T)

par(mfrow=c(1,3))
plot(pointpattern, which.marks="snares", markscale=200)
plot(soysambu_roads_psp, add=T, lwd=1, show.all=F, style='none')
plot(Z, which.marks="class", pch=c(".", "+"), main="nnclean features")
plot(soysambu_roads_psp, style="none", add=T, col="steelblue")
plot(Z, which.marks="prob", markscale=500, main="nnclean probs")
plot(soysambu_roads_psp, style="none", add=T, col="steelblue")
par(mfrow=c(1,1))

#' for interpretation - keep in mind that this is about the locations
#' so if you have many snares on 1 m2 that does not mean that it also
#' becomes a high intensity zone,
#' also remember that this technique just separates into high and low
#' intensity zones.

# =================================

# clustering using clusterset
cl = clusterset(pointpattern, what="marks", verbose=F, fast=T)
cls = subset(cl, marks=="yes")
marks(cls) = "feature"

par(mfrow=c(1,3))
plot(pointpattern, which.marks="snares", markscale=200)
plot(soysambu_roads_psp, add=T, lwd=1, show.all=F, style='none')
plot(cls, pch="+", main="Clusterset features")
plot(soysambu_roads_psp, add=T, lwd=1, show.all=F, style='none')
plot(cls, which.marks="class", main="Clusterset probabilities")
plot(soysambu_roads_psp, style="none", add=T, col="steelblue")
par(mfrow=c(1,1))


# with highrisk zone

# create a cutoff
Qp=quantile(nndist(serena), p=0.99, type=8)

# distance map
dmap=distmap(serena)

# compare with cutoff
zone_dist=eval.im(dmap < Qp)

# plot
plot(zone_dist, col=c("grey", "steelblue"))
plot(soysambu_roads_psp, add=T)
plot(serena, use.marks=F, add=T, pch=".")

Qp=quantile(nndist(all_snares), p=0.5, type=8)
quantile(nndist(pointpattern))  # in this case pointpattern, not all snares: locations, not density.

f = function(ppp) {

  # distance map
  dmap=distmap(ppp)

  # compare with cutoff
  zone_dist=eval.im(dmap < Qp)

  # plot
  plot(zone_dist, col=c("grey", "steelblue"))
  plot(soysambu_roads_psp, add=T, style="none", show.all=F)
  plot(ppp, use.marks=F, add=T, pch="+")

}

Qp=300
f(pointpattern)

# Nogueira de Melo, 2017 - Voronoi

# X = dirichlet(pointpattern)
# Y = tiles(X)
# Z = sapply(Y, area) %>% setDT
# setnames(Z, "area.m2")
# Z[, area.ha := round(area.m2/(100*100))]

# ggplot(Z) + stat_ecdf(aes(area.ha)) + theme_bw()
# plot(X)
# a tesselation will work only if you have insight in the complete pointpattern.
# because you can not generalize to areas outside the transects.

# ------------------------

#' via species distr modeling book
#' goal: put aster and sentinel into one brick file, then do correlation
#' the brick file is not necessary for that correlation, just a neat way of keeping things together.

# https://stackoverflow.com/questions/19136330/legend-of-a-raster-map-with-categorical-data
s=as.factor(sentinel)
rat=levels(s)[[1]]
rat[["landcover"]]=c("water", "bush", "forest", "open")
levels(s)=rat
rasterVis::levelplot(s, col.regions=viridis(4))

bush=copy(sentinel)
bush[bush != 2] = 0

asterx=resample(aster, sentinel)
br = brick(sentinel, asterx, bush)
plot(br, col=viridis(255, direction=-1))

p = data.frame(pointpattern) %>% setDT
p = p[cat == "NECK SNARE" & live_dead == "LIVE",]

plot(bush, col=viridis(2), alpha=0.5, legend=F)
points(p$x, p$y, pch="+")
plot(soysambu_roads, add=T, col="red")

# find interface of open and bush
w = matrix(rep(1,49), ncol=7) # observation window
interface = focal(bush, w=w, fun=mean, na.rm=T)

plot(interface, col=viridis(255, alpha=0.5))
points(p$x, p$y, pch="+", col="red", cex=0.7)
plot(soysambu_roads, col="red", add=T)
points(po$lon, po$lat)


# extract values
px = sp::SpatialPoints(p[, 1:2], proj4string=crs(bush))
p[, iscore := extract(interface, px)]
hist(p$iscore)

# normalize iscore
p[, n.iscore := (iscore - min(iscore))/(max(iscore)-min(iscore))]
psf = dt_sf(p, coords=c("x", "y"))

# ditto with layerize

h = layerize(sentinel)
# plot(h$X4)
u = focal(h$X2, w=w, fun=mean, na.rm=T)
plot(u)
points(p$x, p$y, pch="+", col="red", cex=0.7)
plot(soysambu_roads, add=T, col="red")

#' now what you have is a continuum [0-2]
#' on the map it is clear that the points are in the buffer zone,
#' but a discretization wouldn't work: you need something fuzzy-ish.

samp = sampleRandom(interface, size=nrow(p))
iscore = p$iscore
df=rbind(
  data.table(
    treatment="samp",
    val=samp
  ),
  data.table(
    treatment="iscore",
    val=iscore
  )
)

df$treatment=factor(df$treatment)
# oneway_test(val~treatment, data=df, distribution="exact")
# coin::wilcoxsign_test(iscore ~ samp)

hs = hillShade(
  terrain(aster, opt="slope"),
  terrain(aster, opt="aspect")
)


plotstuff_lake = function(){
  points(necksnares$x, necksnares$y, pch=3, col="red")
  points(groundsnares$x, groundsnares$y, pch=4, col="red")
  plot(soysambu_roads_lake_psp, col="red", add=T, style="none", show.all=F)
  points(gates$lon, gates$lat, pch=0, col="black", cex=1, lwd=1)
  points(hotels$lon, hotels$lat, pch=20, col="black", cex=1, lwd=1)
  points(other$lon, other$lat, pch=18, col="black", cex=1, lwd=1)
  plot(Window(pp_lake), add=T, lty=4)
  plot(Window(all_snares), add=T, lty=4)
  legend("bottomleft",
         legend=c("neck snares", "ground snares", "gates", "hotels", "head office, lookout"),
         pch = c(3,4,0,20,18),
         box.lty=0)
}

plot(hs, col=grey(0:100/100))
plot(terrain(aster), col=terrain.colors(25, alpha=0.35), add=T)
plotstuff_lake()

slope=terrain(aster, opt="slope")
aspect=terrain(aster, opt="aspect")
hill=hillShade(slope, aspect, 40, 270)

# https://stackoverflow.com/questions/52150972/plot-hillshade-in-tmap
alt=aster
tm_shape(hill) + tm_raster(palette=gray(0:100/100), n=100, legend.show=F) +
  tm_shape(alt) +
  tm_raster(alpha=0.5, palette=terrain.colors(25), legend.show=T)


# =====================================

png('file.png', height=nrow(sentinel), width=ncol(sentinel))
plot(sentinel, maxpixels=ncell(r))
dev.off()

overlay_img=png::readPNG("file.png")

library(rayshader)
localtif=asterx
elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))

elmat %>%
  sphere_shade(texture = "desert") %>%
  # add_water(detect_water(elmat), color="desert") %>%
  # add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.5) %>%
  # add_shadow(elmat,0.5) %>%
  plot_3d(elmat,zscale=10,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800))

ambmat <- ambient_shade(elmat, zscale = 30)
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.5) %>%
  add_shadow(ambmat,0.5) %>%
  add_overlay(overlay_img, alphalayer=0.5) %>%
  plot_3d(elmat,zscale=10,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800))

p1 = (x-extent(elevation)@xmin)  / (extent(elevation)@xmax-extent(elevation)@xmin) * ncol(elevation)
p2 = (y-extent(elevation)@ymin)  / (extent(elevation)@ymax-extent(elevation)@ymin) * nrow(elevation)

dp = distanceFromPoints(bush, px)
dp = mask(dp, bush)
plot(dp)

# https://gis.stackexchange.com/questions/226554/create-raster-in-r-distance-to-line-feature
# sr = as(soysambu_roads, "Spatial")
# sr = rgeos::gUnion(sr, sr)
# dd = rgeos::gDistance(sr, as(bush, "SpatialPoints"), byid=T)
# bush[] = apply(dd, 1, min)
# plot(bush)
pp = pointpattern
neck.snares=subset(pp, cat=="NECK SNARE")

U=cut(lu.im, 4, labels=c("water", "bush", "forest", "open"))
plot(U, col=viridis(4))
V=tess(image=U)
quadratcount(neck.snares, tess=V)

plotstuff = function(){
  points(p$x, p$y, pch="+", col="red")
  plot(soysambu_roads_psp, col="red", add=T, style="none", show.all=F)
  points(po$lon, po$lat, pch=0, col="black", cex=1, lwd=2)
}

par(mfrow=c(1,2))
plot(lu.im, col=viridis(4, alpha=0.4))
plotstuff()
plot(U, col=viridis(4, alpha=0.5), main="")
plotstuff()
par(mfrow=c(1,1))

pointpattern %>%
  split("cat") %>%
  plot(legend=F)

normplot = function(ppp) {
  Z = distmap(ppp)
  g = ecdf(Z)
  Y = eval.im(g(Z))
  plot(Y, main="", col=viridis(255))
  # plotstuff()
  return(Y)
}

pp = pointpattern
normplot(pp)
plot(distmap(pp), col=viridis(255,option="A", direction=-1,alpha=0.6))

intensity(pp, weights=marks(pp)$snares)
intensity(pp)

par(mfrow=c(1,2))
plot(density(pp, diggle=T, sigma=bw.ppl(pp), weights=marks(pp)$snares), col=viridis(255, alpha=0.8, direction=1, option="B"))     # assumes inhomogeneous Poisson process
plotstuff()
plot(density(pp, diggle=T, sigma=bw.diggle(pp), weights=marks(pp)$snares), col=viridis(255, alpha=0.8, direction=1, option="B"))  # assumes Cox process (clustered)
plotstuff()
par(mfrow=c(1,1))

par(mfrow=c(1,2))
plot(density(serena, diggle=T, sigma=bw.ppl(serena), weights=marks(serena)$snares))     # assumes inhomogeneous Poisson process
plotstuff()
plot(density(serena, diggle=T, sigma=bw.diggle(serena), weights=marks(serena)$snares))  # assumes Cox process (clustered)
plotstuff()
par(mfrow=c(1,1))

plot(adaptive.density(serena, f=0.3, nrep=50))
nndensity(serena, k=3) %>% plot
nndensity(pp, k=4) %>% plot
plot(adaptive.density(pp, f=0.2, nrep=40))

interfacex = as.im(interface)
plot(interfacex, col=viridis(255, alpha=0.6))
plotstuff()
b=quantile(interfacex, probs=(0:4)/4, type=1)
Zcut=cut(interfacex, breaks=c(0, 0.3, 1.5, 2), labels=c("open", "openbush", "bush"))
V=tess(image=Zcut)
quadratcount(neck.snares, tess=V)
quadrat.test(neck.snares, tess=V)

plot(Zcut, col=viridis(3, alpha=0.3),
     main="Snare locations vis-a-vis vegetation")
plotstuff()

par(mfrow=c(1,2))
rh = rhohat(unmark(neck.snares), interfacex)
plot(predict(rh), col=viridis(255, option="A", alpha=0.6))
plotstuff()
plot(rh)
par(mfrow=c(1,1))

dR = distmap(soysambu_roads_psp)
plot(dR, col=viridis(255, option="A", alpha=0.6, direction=-1))
plotstuff()
neck.snares %>% Window %>% plot(add=T, lty=3)
rhohat(unmark(neck.snares), dR) %>% plot

cdf.test(neck.snares, dR, test="ks") %>% plot
cdf.test(neck.snares, dR, test="cvm") %>% plot(style="QQ")
cdf.test(neck.snares, dR, test="ad") %>% plot(style="PP")
cdf.test(neck.snares, "x") %>% plot

berman.test(unmark(neck.snares), dR) %>% plot

roc(unmark(neck.snares), distfun(soysambu_roads_psp), high=F) %>% plot
abline(a=0,b=1,col="grey50",lty=3)
grid()
auc(unmark(neck.snares), distfun(soysambu_roads_psp), high=F)
# 25% of the area holds 50% of the snares.

roc(unmark(serena), distfun(soysambu_roads_psp), high=F) %>% plot
abline(a=0,b=1,col="grey50",lty=3)
grid()
auc(unmark(serena), distfun(soysambu_roads_psp), high=F)

density(neck.snares, bw.ppl, ns=16) %>% plot

bdist.points(neck.snares)

par(mfrow=c(1,2))
bdist.pixels(Window(neck.snares)) %>% plot(col=viridis(255, direction=-1, option="A", alpha=0.6), main="Distance from boundaries")
plot(Window(neck.snares), add=T, lty=3)
plotstuff()
dR = distmap(soysambu_roads_psp)
plot(dR, col=viridis(255, option="A", alpha=0.6, direction=-1), main="Distance from roads")
plotstuff()
neck.snares %>% Window %>% plot(add=T, lty=3)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
rhohat(unmark(neck.snares), dR) %>% plot(main="Distance to roads")
rhohat(unmark(neck.snares), bdist.pixels(Window(neck.snares))) %>% plot(main="Distance to boundaries")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
roc(unmark(neck.snares), distfun(soysambu_roads_psp), high=F) %>% plot(main="ROC curve distance to roads")
abline(a=0,b=1,col="grey50",lty=3)
grid(col="grey90")
roc(unmark(neck.snares), bdist.pixels(Window(neck.snares))) %>% plot(main="ROC curve distance to boundaries")
abline(a=0,b=1,col="grey50",lty=3)
grid(col="grey90")
par(mfrow=c(1,1))

# compare image files
fmin= function(a,b) {
  x=ifelse(a>b, a, b)
}
dist.b = bdist.pixels(Window(neck.snares))
dist.r = distmap(soysambu_roads_psp)
eval.im(fmin(dist.b, dist.r)) %>% plot(col=viridis(255, alpha=0.6, option="D"))

plotstuff()
plot(Window(all_snares), add=T)
# Correlation =============



# fry points and stienen
dmap=distmap(serena)
serena %>% nndist %>% ecdf %>% plot(main="ECDF")
Qp=serena %>% nndist %>% quantile(probs=0.8)*(3.14^2)


par(mfrow=c(2,2))
plot(Window(serena))
plotstuff()
serena %>% fryplot(use.marks=F, main="Fry plot")
# serena %>% stienen(main="Stienen")
rose(nnorient(serena), clockwise=T, start="N")
zone_dist=eval.im(dmap < Qp) %>% plot(main="Mahling")
par(mfrow=c(1,1))



dmap=distmap(pointpattern)
pointpattern %>% nndist %>% ecdf %>% plot
Qp=pointpattern %>% nndist %>% quantile(probs=0.9)*3.14
par(mfrow=c(2,2))
plot(Window(pointpattern))
plotstuff()
pointpattern %>% fryplot(use.marks=F, main="Fry plot")
# serena %>% stienen(main="Stienen")
rose(nnorient(pointpattern), clockwise=T, start="N")
eval.im(dmap < Qp) %>% plot(main="Mahling", col=c("lightgrey", "steelblue"))
plotstuff()
par(mfrow=c(1,1))

# distance map

zone_dist=eval.im(dmap < Qp)
Z=connected(zone_dist, Qp)
tiles(tess(image=Z))  # can extract area from this

# librarian::shelf(wdpar, ggmap)
#
# ken <- wdpa_fetch("Kenya", wait = TRUE) %>%
#   wdpa_clean
#
# ken_data <- st_transform(ken, 4326)
#
# # download basemap imagery
# bg <- get_stamenmap(unname(st_bbox(ken_data)), zoom = 8,
#                     maptype = "watercolor", force = TRUE)
#
# # make map
# ggmap(bg) +
#   geom_sf(aes(fill = IUCN_CAT), data = ken_data, inherit.aes = FALSE) +
#   theme(axis.title = element_blank(), legend.position = "bottom")


# CSR ?
quadrat.test(serena)
hopskel(serena)
hopskel.test(serena) # strongly clustered; A < 1 (A=1 = CSR, A > 1 = regularity)

d <- distmap(cells, dimyx=256)
X <- levelset(d, 0.07)
plot(X)
Z <- connected(X)
plot(Z)
# or equivalently
Z <- connected(d <= 0.07)

# number of components
nc <- length(levels(Z))
# plot with randomised colour map
plot(Z, col=hsv(h=sample(seq(0,1,length=nc), nc)))

# how to extract the components as a list of windows
W <- tiles(tess(image=Z))


# fitting

dfault=distfun(soysambu_roads_psp)
fit=kppm(unmark(pointpattern) ~ dfault, clusters="MatClust")
par(mfrow=c(1,2))
plot(fit)
par(mfrow=c(1,1))

fit %>% leverage %>% as.im %>% persp
plot(simulate(fit, nsim=6))
