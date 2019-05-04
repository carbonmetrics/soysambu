

#
# # simple ratio
# red=subset(bbb, "B4")
# nir=subset(bbb, "B8")
# plot(nir/red, col=viridis(4, direction=-1))
# plotstuff()
# plot(nir/red, col=viridis(4, direction=-1))
# plotstuff(plot.snares=T, cex=0.5)
#
# # natural
# nat=subset(bbb, "natural")
# plot(nat, col=viridis(cl, direction=-1))
# plotstuff()
#
# # DVI
# dvi=subset(bbb, "DVI")
# plot(dvi, col=viridis(cl), direction=1)
# plotstuff()
#
# # redo clustering
# dvi=subset(b, "SAVI")
# z=clustLanduse(dvi,cl=5)
# z=mask(z, conservancy)
# plot(z, col=viridis(5, direction=1))
# plotstuff()
# zz=layerize(z)
# bush=subset(zz, "X4")
# # bush1=aggregate(bush,fact=2)
# w=matrix(1/121, nrow=11, ncol=11)
# pf=focal(bush,w=w, fun=mean, na.rm=T)
#
# f=function(x) {
#   x=1/x                   # Tukey transformation
#   x=scales::rescale(x)    # minmax transformation
#   x=ifelse(is.infinite(x),0,x)
# }
#
# pf=focal(bush,w=w,fun=sum,na.rm=T)
# h=calc(pf,f)
#
# par(mfrow=c(1,2))
# plot(pf)
# plot(h)
# par(mfrow=c(1,1))
#
# plot(pf)
# plotstuff(plot.snares=T,cex=0.6)
#
# s=subset(snares, cat=="NECK SNARE")
# px=ppp_dt(s)
# py = sp::SpatialPoints(px[, 1:2], proj4string=crs(bush))
# px[, i := extract(pf, py)]
#
# marks(s)=px$i
# plot(s, markscale=500)
#
# # this can now be tested: if you take a random sample from the raster to obtain i score, compare with i score for snares
# # you can also use i as a covariate in spatstat
# marks(s)=px$i


#
# bbush=boundaries(bush1, classes=T)
#
# plot(bbush, col=viridis(255))
#
# plot(bbush)
# plotstuff(plot.snares=T, cex=0.5)
#
# # plot boundaries
# z=layerize(dvi)
# # dvi[is.na(dvi)] = 0
# dvi[dvi==2]=1
# dvi=raster::aggregate(dvi, fact=2, fun=mean)
# pb=boundaries(dvi,classes=F)
# pc=clump(pb)
#
# plot(pb,col="grey90")
# plot(pc,add=T,col="yellow")
#





#
#
# # ----
#
# # plot results
# par(mfrow=c(2,3))
# lx=foreach(i=1:length(ll)) %do% {
#   plot(ll[[i]], col=viridis(cl), main=names(ll)[i])
# }
# # compare with original landuse file
# h=clustLanduse(b$natural, cl=cl)
# hh=mask(h,conservancy)
# plot(hh,col=viridis(cl))
# par(mfrow=c(1,1))
#
# par(mfrow=c(1,2))
# plot(ll$natural, col=viridis(cl,direction=1))
# plotstuff(plot.legend=F, plot.snares=T, cex=0.8)
# plot(hh, col=viridis(4))
# plotstuff(plot.legend=F, cex=0.8, plot.snares=T)
# par(mfrow=c(1,1))
#
# # edges
# nat=subset(bb, "natural")
# nat[!nat==4]=NA
#
# par(mfrow=c(1,2))
# plot(nat,col=viridis(4))
# plotstuff(plot.snares=T,plot.legend=F,cex=0.6,col="black")
# pb=boundaries(nat)
# plotstuff(plot.snares=T,plot.legend=F,cex=0.6,col="black")
# pc=clump(pb)
# plot(pc)
# par(mfrow=c(1,1))
#
#
# nat=subset(bb, "natural")
# plot(nat,col=viridis(cl))
#
# landcover=c("water","bush","dense bush","open")
# z=data.table::data.table(
#   ID=1:cl,
#   landcover=landcover
# )
# levels(nat)[[1]]=z
#
# f1=curry(plot, col=viridis(cl))
#
# natx=copy(nat)
# natx=aggregate(natx, fact=2,expand=F,na.rm=T)
# pb=boundaries(natx, type="outer",classes=T)
# natx[!nat==4]=NA  # only bush
# # natx[natx==4]=1
#
# w=matrix(1/25, nrow=5,ncol=5)
# pf=focal(natx,w=w, fun=median, na.rm=T)
#
# plot(pf, add=F, col="yellow")   # focal
# plot(pb, add=T,col="grey90")    # boundaries
#
# # plot(st_geometry(gps_tracks),add=T, cex=0.2,col="orange")
# plotstuff(plot.snares=T,plot.legend=F,cex=0.6,col="black")
#
#
# p=curry(plotstuff, plot.snares=T,plot.legend=F,cex=0.8)
# natx[natx==4]=1
#
#
# x=mask(pb, pf, maskvalue=1)
# p()
# # now, the neck snares will be along the edges of the yellow patches, preferably just in the grey patches.
# # this means that they are inside the bush at the edge of open to acacia land use.
# # in many cases we avoided deep bush, but not in all cases
#
#
#
