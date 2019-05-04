#' geographic profiling
#' calculate distance of snares and community centres

# set up
pacman::p_load(raster, sf, spatstat, data.table, Soysambu, ggplot2, viridis)

# give r km extra space to snares window
data(snares)
s=copy(snares)
Window(s)=dilation(Window(snares),10000)
plot(unmark(s),lty="dotted")
plotstuff(plot.lake=T)

# convert poi for communities into pointpattern
data(poi)
x=sf_dt(poi)[type=="community", !c("day","night", "type")]
comm=ppp(x=x$lon,y=x$lat,window=Window(s))

plot(unmark(comm), add=T, pch=0, col="steelblue")

y=crossdist(s,comm) %>% as.data.table
names(y)=x$waypoint
y[, idx := .I]

z=melt(y, id="idx")

ggplot(z)+
  geom_density(aes(value),col="steelblue")+
  geom_rug(aes(value),col="grey")+
  facet_grid(variable ~., scales="free", space="free") +
  theme_bw()

# also plot distances

distmap(comm) %>% plot(col=viridis(255,direction=-1))

