#' geographic profiling
#' calculate distance of snares and community centres

# set up
pacman::p_load(raster, sf, spatstat, data.table, Soysambu, ggplot2, viridis)

# give r km extra space to snares window
data(snares)
s=copy(snares)
Window(s)=dilation(Window(snares),10000)
# plot(unmark(s),lty="dotted")
# plotstuff(plot.lake=T)

# convert poi for communities into pointpattern
data(poi)
x=sf_dt(poi)[type=="community", !c("day","night", "type")]
comm=ppp(x=x$lon,y=x$lat,window=Window(s))

# Dirichlet (Voronoi) tesselation
d=dirichlet(comm)

# plot(unmark(comm), add=T, pch=0, col="steelblue")

y=crossdist(s,comm) %>% as.data.table
names(y)=x$waypoint
y[, idx := .I]

z=melt(y, id="idx")

ggplot(z[variable=="Elmenteita", ])+
  geom_density(aes(value),col="steelblue", bw=1000)+
  geom_rug(aes(value),col="grey")+
  xlim(0,10.5e3)+
  facet_grid(variable ~., scales="free", space="free") +
  theme_bw()

# also plot distances

# distmap(comm) %>% plot(col=viridis(255,direction=-1))
# plotstuff(plot.lake=T,plot.snares=T,cex=0.6)
# plot(unmark(comm),add=T,col="red",pch=0,cex=0.8)

# tesselations

# plot(d,lty="dotted")
# plotstuff()
# plot(unmark(comm),pch=0,add=T)

counts=quadratcount(s,tess=d)
# to go from count to density you have to calculate the areas
areas=tile.areas(d)
dens=counts/areas

# plots
plot(counts, col="steelblue", border="grey", lty="dotdash", lwd=0.5, main="")
plot(Window(snares),add=T)
plot(unmark(comm),add=T,pch=0)
legend(x=1,y=2,legend=c("community"), pch=0)
title(main="Snares per community", sub="Based on Voronoi tesselation")
