#' Figures for Soysambu poaching article

# set up ===============

# load multiple libraries simultaneously and install automatically if not available with `pacman`.
# Soysambu library on https://github.com/carbonmetrics/soysambu.
# This library contains raw data, data processing, plots of results.

if(!require(pacman)){
  install.packages("pacman")
}else{
  pacman::p_load(Soysambu, sf, raster, spatstat, tmap, tmaptools,
                 OpenStreetMap, mapedit, mapview, leaflet, here, lubridate, stringr,
                 viridis, maptools, ggplot2,data.table,forcats,foreach)
}

path="/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/figures/"
proj.path="/home/henk/Documents/PhD/Soysambu/Soysambu/"

plotter=curry(pdf,width=10,height=8,pointsize=10,paper="a4r")
setwd(path)

# generate messages ============

# ensure that numbers in text are up to date

km.walked=sf_dt(gps_tracks)[, signif(sum(dist_m)/1000,0)]
n.transects=length(unique(gps_tracks$track_nr))

all.snares=ppp_dt(snares)
n.snares=all.snares[, sum(snares)]

live.dead=all.snares[, .(N=sum(snares)),live_dead]
neck.foot=all.snares[, .(N=sum(snares)),cat]


hs=hopskel.test(snares,alternative="clustered")
stat=round(hs$statistic,3)
pval=round(hs$p.value,3)

message(km.walked)
message(n.snares)
message(snares.live)
message(snares.dead)
message(n.transects)
message(stat)
message(pval)


# location of Soysambu =========

landuse=raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/landuse.tif")
soysambu = tmaptools::read_osm(tmaptools::bb(landuse), type="osm", ext=1.4)

elmenteita_basemap = tmaptools::read_osm(bb(landuse), ext=1.5, type="osm") # how this was obtained

# things to plot on all maps
mapstuff = tm_layout(frame=F, legend.position=c("right", "bottom")) +
  tm_compass(position=c("right", "bottom")) +
  tm_scale_bar(position=c("right", "bottom")) +
  tm_grid(col="lightgrey")

detailed = tm_shape(elmenteita_basemap) + tm_raster() +
  tm_shape(soysambu_boundaries) + tm_borders() +
  mapstuff

# country map inset
po = poi[poi$waypoint == "Nairobi",]
thelake = poi[poi$type == "lake",]
kenya = rnaturalearth::ne_countries(country="Kenya", scale="large")

kenya_plot = tm_shape(kenya) + tm_borders() +
  tm_shape(po) + tm_dots(size = 0.3) + tm_text("waypoint", size=0.6, just="left", xmod=0.5) +
  tm_shape(thelake) + tm_dots(size=0.5, shape=1) + tm_text("waypoint", size=0.6, just="left", xmod=0.5) +
  tm_layout(frame=F)

# plot basemap with country inset
vp_kenya = grid::viewport(x = 0.17, y = 0.13, width = 0.26, height = 0.25)

plotter("soysambu.pdf")
pdf(file=paste0(path,"/","soysambu.pdf"), paper="a4r", pointsize=8)
detailed
print(kenya_plot, vp=vp_kenya)
dev.off()


# Snares found and transects ========

pdf(file=paste0(path,"/","snares_transects.pdf"),paper="a4r",pointsize=10,width=10,height=8)

par(mfrow=c(1,2))
plotstuff(plot.snares=T,plot.lake=F,add=F,cex=0.7)
plotstuff(add=F, plot.lake=F,cex=0.7,plot.legend=F)
plot(st_geometry(gps_tracks),col="grey70",add=T, cex=0.5)
par(mfrow=c(1,1))

dev.off()

roads=layered(soysambu_roads_psp,plotargs=list(col="grey60",show.all=F,show.window=F,ribbon=F))
snares.found=layered(snares, plotargs=list(cols="grey30",which.marks=cat,main=""))
snares.roads=layered(snares.found, roads)
plot(snares.roads)

# Hotspots =======================

# make distance maps
fmin= function(a,b) {
  x=ifelse(a<b, a, b)
}
dist.b = bdist.pixels(Window(pp_lake))
dist.r = distmap(soysambu_roads_lake_psp)
dist.br = eval.im(fmin(dist.b, dist.r))

baruk.stores=subset(poi,waypoint=="Baruk Stores")
hq=subset(poi,waypoint=="Head Office")

# plot
# Stienen set
plotter(file="stienen.pdf")
s=unmark(snares)
marks(s)=nndist(s)
plot(s,markscale=0.5, main="",add=F, show.window=F,legend=T)
plot(soysambu_roads_psp, col="grey60", style="none", main="", show.window=F,add=T)
plot(Window(snares),main="", add=T)
dev.off()

# Steiner set
plotter(file="hotspots.pdf")
plot(soysambu_roads_psp, col="grey60", style="none", main="", show.window=F)
plot(Window(snares),main="", add=T)
x=plotclusters(snares, distance=250,clusterpoints=5,add=T)
contour(dist.br, col="lightgrey", levels=seq(0,3000,1000),add=T)
plot(snares, use.marks=F, pch=".", main="", add=T)
# plot(st_geometry(baruk.stores),pch=7,add=T,cex=1.5)
# plot(st_geometry(hq),pch=13,cex=1.5,add=T)
plotstuff(plot.lake=F)
dev.off()


# proximity of hotspots to gates ================

gates=subset(poi,waypoint %like% "Gate")
hq=subset(poi,waypoint=="Head Office")
baruk.stores=subset(poi,waypoint=="Baruk Stores")
lookout=subset(poi,waypoint=="Lookout")
infra=rbind(gates,hq,baruk.stores,lookout)

infra=sf_dt(infra)
infra[waypoint=="Diatomite Gate",lon := lon+50]
infra[waypoint=="Utut Gate",lon:= lon-50]
infra.p=ppp(infra$lon,infra$lat,window=Window(snares))

d=distfun(infra.p)

plotter(file="distance_gates.pdf")
plot(Window(snares),main="")
plot(infra.p,add=T)
contour(d,add=T,col="lightgrey",levels=seq(from=0,to=3000,by=1000))
plot(snares, use.marks=F, cex=0.7, add=T, pch="+")
plot(soysambu_roads_psp,add=T,col="grey60")
legend("bottomleft",legend="whatever is required to make space", bty="n", text.col="white")

dev.off()

# most snare occur in the 1000-2000 meter range:

dm=distmap(infra.p)
dm$v=cut(dm$v,breaks=seq(0,5000,1000))
v=as.tess(dm)
q=quadratcount(snares,tess=v)
# (0,1e+03] (1e+03,2e+03] (2e+03,3e+03] (3e+03,4e+03] (4e+03,5e+03]
# 46            98            74             1            40


# Voronoi tesselation communities ===========

# give r km extra space to snares window
data(snares)
s=copy(snares)
Window(s)=dilation(Window(snares),10000)

# convert poi for communities into pointpattern
data(poi)
x=sf_dt(poi)[type=="community", !c("day","night", "type")]
comm=ppp(x=x$lon,y=x$lat,window=Window(s))

# Dirichlet (Voronoi) tesselation
d=dirichlet(comm)
counts=quadratcount(s,tess=d)

# plot
plotter(file="communities.pdf")
plot(counts, col="steelblue", border="grey", lty="dotdash", lwd=0.5, main="")
plot(Window(snares),add=T)
plot(unmark(comm),add=T,pch=0, main="")
dev.off()


# distance decay function ==================

# take the Elmentaita tile...
elmtile=tiles(d)[[2]]
elmsnares=snares[elmtile]

# ...and take the difference with the Elmentaita position
elm=comm[elmtile]

dist.elm=crossdist(elm,elmsnares) %>% as.vector

plotter(file="distance_decay.pdf")
dist.elm %>% density(bw="nrd",from=0) %>% plot(main="",xlab="Distance from Elmenteita village (m)",cex.axis=2,cex.lab=2)
rug(dist.elm)
dev.off()




# bush-open area interface ============

predictors=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/brickfile.grd")
bush.im=as.im.RasterLayer(subset(predictors, "interface_class"))

ground.snares=subset(snares, cat=="GROUND SNARE")
neck.snares=subset(snares, cat=="NECK SNARE")

mycols=c("white","grey80","gray40")
mycols=scales::alpha(mycols,0.5)

plotter(file="bush.pdf")
plot(bush.im,col=mycols,main="",show.all=F,box=F)
plot(Window(snares),add=T,main="")
plot(ground.snares, add=T, pch=3, col="grey30", cex=0.8, use.marks=F)
plot(neck.snares, add=T, pch=1, col="grey30",cex=0.8, use.marks=F)
legend("bottomleft",legend="whatever is required to make space", bty="n", text.col="white")

dev.off()


# repeats ============================

marks(snares)$datex=marks(snares)$date=factor(marks(snares)$date)
split(snares,"datex") %>% plot(use.marks=F)


# Serena B hotspot

serena=subset(snares, datex %in% c("2018-12-05","2019-01-17","2019-03-27","2019-03-29"))

# a=clickbox()
a=structure(list(type = "rectangle", xrange = c(860425.277862241,
                                                861286.980503061), yrange = c(9955395.72812958, 9956372.85514261
                                                ), units = structure(list(singular = "unit", plural = "units",
                                                                          multiplier = 1), class = "unitname")), class = "owin")
serena=snares[a]
serena=subset(serena,datex %in% c("2018-12-05","2019-01-17","2019-03-27"))
marks(serena)$datex=droplevels(marks(serena)$datex)
chs=convexhull.xy(serena)


# Jolai Gate hotspot

jolai.gate=subset(snares, datex %in% c("2019-03-26","2019-03-27","2019-03-29","2019-01-24"))
# b=clickbox()
b=structure(list(type = "rectangle", xrange = c(855345.956837355,
                                                856143.214070323), yrange = c(9945580.51526661, 9946647.26790227
                                                ), units = structure(list(singular = "unit", plural = "units",
                                                                          multiplier = 1), class = "unitname")), class = "owin")
jolai.gate=snares[b]
marks(jolai.gate)$datex=droplevels(marks(jolai.gate)$datex)
chj=convexhull.xy(jolai.gate)


# quarry hotspot

quarry=subset(snares, datex %in% c("2018-11-21","2018-11-22","2018-12-06","2019-01-25","2019-03-26","2019-03-27"))
# d=clickbox()
d=structure(list(type = "rectangle", xrange = c(850980.340565514,
                                                853255.426917525), yrange = c(9941178.34191932, 9943124.50012405
                                                ), units = structure(list(singular = "unit", plural = "units",
                                                                          multiplier = 1), class = "unitname")), class = "owin")
quarry=snares[d]
marks(quarry)$datex=droplevels(marks(quarry)$datex)
chq=convexhull.xy(quarry)


# plot
# plotter(file="serena_repeat.pdf")
# plot(serena,which.marks="datex", main="", show.window=F)
# chs=convexhull.xy(serena)
# plot(chs,add=T,border="lightgrey",main="")
# dev.off()
#
# plotter(file="jolai_repeat.pdf")
# plot(jolai.gate, which.marks="datex",main="",show.window=F)
# chj=convexhull.xy(jolai.gate)
# plot(chj,add=T,border="lightgrey", main="")
# dev.off()
#
# plotter(file="quarry_repeat.pdf")
# plot(quarry, which.marks="datex",main="", show.window=F)
# chq=convexhull.xy(quarry)
# plot(chq,add=T,border="lightgrey", main="")
# dev.off()

# location in soysambu map
# centroid of clusters
# quarry.centroid=centroid.owin(chq, as.ppp=T)
# serena.centroid=centroid.owin(chs, as.ppp=T)
# jolai.centroid=centroid.owin(chj, as.ppp=T)
# hotspots=superimpose(quarry.centroid,serena.centroid,jolai.centroid)
#
# plotter(file="repeat_hotspots.pdf")
# plot(Window(snares),main="")
# plot(Window(hotspots),add=T,col="lightgrey")
# plot(hotspots, add=T, pch="+")
# legend("bottomleft",legend="whatever is required to make space", bty="n", text.col="white")
# dev.off()

# areas
round(spatstat::area(chs)/(100*100)) # area in hectare
round(spatstat::area(chj)/(100*100)) # area in hectare
round(spatstat::area(chq)/(100*100)) # area in hectare

# counts by date
# ppp_dt(quarry)[, .(snare.count=sum(snares)), datex]
ppp_dt(jolai.gate)[, .(snare.count=sum(snares)), datex]
ppp_dt(serena)[, .(snare.count=sum(snares)), datex]

# plot
# overview
hotspots=layered(chq,chs,chj,plotargs=list(main="",border="grey70",col="grey90"))
hotspot.locations=layered(hotspots,Window(snares),plotargs=list(main=""))

# individual hotspots
plotargs=list(border="lightgrey", which.marks="datex",show.window=F,main="")
h1=layered(serena,chs,plotargs=plotargs)
h2=layered(jolai.gate,chj,plotargs=plotargs)
h3=layered(quarry,chq,plotargs=plotargs)

l=solist(h1,h2,h3,hotspot.locations)

foreach(i=1:length(l)) %do% {
  plotter(file=paste0("repeat",i))
  plot(l[i],main="",mar.panel=c(0,0,0,0))
  dev.off()
}

system("./pdfcrop.sh")

plotter(file="hotspots_repeats.pdf")
plot(l,main="")
dev.off()
system("pdfcrop hotspots_repeats.pdf hotspots_repeats.pdf")



# Likert plot =========

# prepare

df=closed_questions[, QuestionExplain,Score]
setnames(df,c("score","item"))

neutral=df[score==3,.N,item]
no.opinion=df[is.na(score),.N,item]

dfx=df[score %nin% c(3,NA)]
dfy=dfx[, .N,.(item,score)]
dfy[, w:=N*score]
setorder(dfy,-w)

# assign categories
x=unique(dfy,by="item")
x[c(3,8,15,20),category:="Relations"]
x[c(4,14,21), category:="Occurrence"]
x[c(23,6,11,18,22),category:="Deterrence"]
x[c(9,10,12,13,16,17),category:="Location"]
x[1:2,category:="Other"]
x[is.na(category),category:="Perspective"]

y=x[, .(item,category)]
dfy=y[dfy,on="item"]
dfy=dfy[category!="Other"]

# calculate neutral score, n
z=dfy[, .N,.(agree.disagree,item)]

dfy[, n:=sum(N),item]
dfy[, neutral:=30-n]
dfy[, item:=paste0(item,"\n(n=",n,", neutral=",neutral,")")]

# split into agree and disagree
agree=dfy[score>3,]
agree[, item:={
  t1=fct_inorder(item)
  t2=fct_rev(t1)
}]

disagree=dfy[score<3,]
disagree[, item:={
  t1=fct_inorder(item)
  t2=fct_rev(t1)
}]

agree[, score:=factor(score)]
agree[, score:=fct_rev(score)]
disagree[, score:=factor(score)]


# agrees and disagrees
agree[, agrees:=sum(N),item]
agree[, num.score:=score %>% as.character %>% as.numeric]
agree[, max.score:=max(num.score),item]
agree[max.score>num.score,agrees:=NA]

disagree[, disagrees:=sum(N),item]
disagree[, num.score:=score %>% as.character %>% as.numeric]
disagree[, min.score:=min(num.score),item]
disagree[min.score<num.score,disagrees:=NA]


# plot
width=0.6
size=2.8
col="grey30"
cols=viridis(4,direction=-1)

theme_set(theme_bw())

ggplot()+
  geom_col(data=agree,aes(item,N,fill=as.factor(score)),width=width)+
  geom_text(data=agree,aes(item,agrees,label=agrees,hjust=-0.5),colour=col,size=size)+
  geom_col(data=disagree,aes(item,N*-1,fill=as.factor(score)),width=width)+
  geom_text(data=disagree,aes(item,disagrees*-1,label=disagrees,hjust=1.5),colour=col,size=size)+
  geom_hline(yintercept=0,color=col,linetype=2)+
  coord_flip()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position="bottom")+
  scale_fill_brewer(labels=c("Strongly disagree","Disagree","Agree","Strongly agree"),palette="Blues")+
  # scale_fill_manual(labels=c("Strongly disagree","Disagree","Agree","Strongly agree"),values=cols)+
  labs(x="",y="Votes (disagree/agree)",fill="Score")+
  facet_grid(category~.,space="free",scale="free")

ggsave(filename="likert.pdf",width=8,height=8,dpi=300)


# crop pdf files ========

system("./pdfcrop.sh")
setwd(proj.path)
