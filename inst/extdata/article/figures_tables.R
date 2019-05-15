#' Figures and data for article
#'
#' For presentations, landscape is best: use orientation="a4r".
#' For publications, portrait is best in some cases: use orientation="a4".
#' Spatstat sometimes comes out small: increase size in pdf command at start of plotting.
#'
#' Bundle and sort pdf files with pdfshuffler (Ubuntu).
#' Convert pdf files to png with "pdftoppm plots.pdf plots -png -r 300" (poppler-utils in Ubuntu).
#' This will convert all pdf pages at once.
#'
#' For whatever reason, run this script with "Source With Echo" (control-shift-enter),
#' otherwise some plots will be corrupted.


# set up ========================================================================================================

libs = c("Soysambu", "data.table", "tmap", "sf", "raster", "rnaturalearth", "maditr",
         "knitr", "ggplot2", "grid", "tmaptools", "spatstat", "maptools", "viridis",
         "here")

lapply(libs, library, character.only=T, quietly=T, verbose=F)


sentinel=raster::raster(here::here("inst","extdata","spatial","conservancy_sentinel.tif"))

filetype= "pdf"
filepath= here::here("inst","extdata","figures/")
pointsize=8
orientation="a4r"

# helper function for file names
namer = . %>% paste0(filepath, ., ".", filetype)


# count mammals from census data ================================================================================

x = census_data[, .(Species = unique(Species)), year(CensusDate)][year == 2018,]
notmammal = x[stringr::str_detect(Species, "Bird") | stringr::str_detect(Species, "Fowl") |
                stringr::str_detect(Species, "Hornbill") | stringr::str_detect(Species, "Python"),]
mammal = nrow(x) - nrow(notmammal)


# introduction maps ============================================================================================

elmenteita_basemap = raster::raster(here::here("inst","extdata","spatial","soysambu_osm.tif"))
# elmenteita_basemap = tmaptools::read_osm(gps_points, ext=2, type="osm") # how this was obtained

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
vp_kenya = grid::viewport(x = 0.87, y = 0.87, width = 0.26, height = 0.25)

pdf(file=namer("soysambu_intro2"), paper=orientation, pointsize=pointsize)
detailed
print(kenya_plot, vp=vp_kenya)
dev.off()

# save
tmap_save(
  detailed,
  insets_tm = kenya_plot,
  insets_vp = grid::viewport(x=0.12,y=0.12,width=0.26,height=0.25),
  filename = namer("soysambu_intro")
)


# generate a table with an overview of snares found =========================================================

s=ppp_dt(snares)
sx=s[, .(total=sum(snares)), .(cat,live_dead)]
sx[, `:=` (
  cat=tolower(cat),
  live_dead=tolower(live_dead)
)]
setnames(sx, c("Category","Status","Total"))


# transects and snares ======================================================================================

pdf(file=namer("snares_transects"),
    paper=orientation,
    pointsize=12)

par(mfrow=c(1,2))

# tracks
plot(unmark(pointpattern_tracks), cex=0.1, main="Transect locations")
plotstuff(plot.snares=F, plot.lake=F, cex=0.7, add=T,col="steelblue")

#snares
plotstuff(plot.snares=T, plot.lake=F, cex=0.7,col="steelblue", add=F, main="Snare locations")
plot(Window(snares),add=T)

par(mfrow=c(1,1))

dev.off()


# snare densities ===========================================================================================

# make subset in order to avoid overlapping counts - desnaring was done in 3 batches which partly overlap
# make subsets for last batch - 1 for all snares with coordinates (3a), one for all snares as numeric mark (3b)
# snares3a = subset(all_snares, date >= as.Date("2019-01-17"))
# snares3b = subset(snares, date >= as.Date("2019-01-17"))

s=subset(snares, "rep" %nin% label)

# xbreaks defined and stored in get_spatial_files.R
# calculate size of quadrats
a=spatstat::quadrats(s, xbreaks=xbreaks, ybreaks=ybreaks)
b=sapply(tiles(a), spatstat::area)
tiles.km2=b/(1000*1000) # in km2

# calculate quadrat counts
quadrat.count = spatstat::quadratcount(s, xbreaks=xbreaks, ybreaks=ybreaks)
km2.count = round(quadrat.count/tiles.km2, 1)  # 1 quadrat=9km2
L = spatstat::solist(unmark(s), quadrat.count, km2.count)
names(L)= c("Snare locations", "Snare locations per quadrat (9 km2)", "Snare location density per km2")

# plot -----------------------------------------------
pdf(file=namer("snare_density"),
    pointsize=pointsize,
    paper=orientation,
    width=13, height=10)
plot(L, main="", mar.panel=c(0,0,0,0), cex.main=1.4, cex=1.4)
dev.off()


# distances from roads and boundaries ====================================================================

# plot distance to boundaries, distance to roads -------------------------------------------------------

cols=viridis::viridis(255, direction=-1,option="A",alpha=0.6)

pdf(file=namer("distances"), pointsize=pointsize, paper=orientation)

par(mfrow=c(1,2))
bdist.pixels(Window(pp_lake)) %>%
  plot(col=cols,
       main="Distance from boundaries")
plot(Window(pp_lake), add=T, lty=3)
plotstuff(plot.snares=T, plot.lake=T, cex=0.7)
dR = distmap(soysambu_roads_psp)
plot(dR,
     col=cols,
     main="Distance from roads")
plotstuff(plot.snares=T, plot.lake=T, cex=0.7)
par(mfrow=c(1,1))

dev.off()


# distance map ---------------------------------------------------------------------------------------

# compare distance to boundary and distance to roads
fmin= function(a,b) {
  x=ifelse(a<b, a, b)
}
dist.b = bdist.pixels(Window(pp_lake))
dist.r = distmap(soysambu_roads_lake_psp)
dist.br = eval.im(fmin(dist.b, dist.r))

cols=viridis::viridis(255, alpha=0.6, option="A", direction=-1)

pdf(file=namer("edge_effects_map"),
    pointsize=pointsize,
    paper=orientation,
    width=10)
plot(dist.br, col=cols,
     main="Distance from roads and boundaries",
     cex.main=0.8, cex=0.8,
     box=F,
     ribside="right",
     ridwid=0.01
     )
plotstuff(plot.snares=T, cex=0.7)
# plot(pointpattern_tracks,add=T, cex=0.1)

dev.off()

# as points --------------

dist.bp=bdist.points(pp_lake)
dist.rd=nearestsegment(pp_lake, soysambu_roads_lake_psp)
dist.brp=pmin(dist.bp,dist.rd)


# diagnostics -------------------------------------------------------------------------------------------

# roc and rhohat plots --------------------------------------------------------------

AUC=auc(unmark(pp_lake), covariate=dist.br, high=F)

pdf(file=namer("edge_effects"), pointsize=pointsize, paper=orientation)

par(mfrow=c(2,1))
plot(rhohat(unmark(pp_lake), dist.br, covname="distance"),
     main="Relation between distance of snare locations and roads and boundaries")
grid()

plot(roc(unmark(snares_lake), covariate=dist.br, high=F),
     main=paste("AUC=", round(AUC, 2)))
abline(a=0,b=1,col="grey50",lty=3)
grid()
par(mfrow=c(1,1))

dev.off()

# cumulative
pdist.b=bdist.points(pp_lake)
pdist.r=project2segment(pp_lake, soysambu_roads_psp)$d
mindist=pmin(pdist.b, pdist.r)

theme_set(theme_bw())

theme_update(plot.title = element_text(size = 12),
             plot.subtitle = element_text(size = 10, color = "grey40"),
             plot.caption = element_text(size = 8, hjust = 0, color = "grey40"))

update_geom_defaults("step", list(colour = "steelblue"))
update_geom_defaults("line", list(colour = "steelblue"))

quantile(mindist, 0.8) %>% signif(digits=0)
quantile(mindist, 0.7) %>% signif(digits=0) # 1000 m
quantile(mindist, 0.5) %>% signif(digits=0)

md=data.table(mindist=mindist)
ggplot(md, aes(mindist)) +
  stat_ecdf() +
  geom_rug(col="grey70") +
  scale_y_continuous(label=scales::percent) +
  labs(
    x="distance (m)", y="cumulative percentage", title="Cumulative distribution snare distances", subtitle="")


# census data =============================================================================================

# make plot
myspecies = c("Zebra", "Thompson Gazelle", "Impala",
              "Rothschild Giraffe",
              "Warthog", "Grants Gazelle")

census = census_data[Species %in% myspecies, ] %>%
  ggplot(aes(x=CensusDate, y=Count)) +
  geom_point(shape=0, alpha=0.5) +
  geom_smooth() +
  facet_wrap(Species~., scales="free", nrow=3) +
  labs(x = "Year of census", y = "Count", caption="Source: Soysambu Conservancy") +
  theme_bw()

pdf(file=namer("census"), pointsize=pointsize, paper=orientation)
census
dev.off()


# rainfall analysis =============================================================================

source("./data-raw/rainfall_analysis.R")

# plot ----------------------------------------------
pdf(file=namer("rainfall"), pointsize=pointsize, paper=orientation)
plot(d, main="Rainfall in Soysambu", cex=1.4)
dev.off()


# position of snares vis-a-vis vegetation ==========================================================

necksnares=subset(snares, cat=="NECK SNARE")

bushes=copy(sentinel)
bushes[bushes != 2] = 0
# bushes[bushes==2] = 1

w = matrix(rep(1,49), ncol=7) # observation window

interface = focal(bushes, w=w, fun=mean, na.rm=T)
interface=crop(interface, extent(soysambu_boundaries))

interfacex = as.im(interface)
b=quantile(interfacex, probs=(0:4)/4, type=1)
Zcut=cut(interfacex, breaks=c(0, 0.3, 1.5, 2), labels=c("open", "openbush", "bush"))
# Zcut=cut(interfacex, breaks=c(0, 0.18, 0.86, 1), labels=c("open", "openbush", "bush"))

# gives warning: Tesselation does not contain all the points of X
V=tess(image=Zcut)
quadratcount(necksnares, tess=V)
quadrat.test(necksnares, tess=V)

# find the interface score for neck snares
p = data.frame(snares) %>% setDT
p = p[cat == "NECK SNARE" & live_dead == "LIVE",]

px = sp::SpatialPoints(p[, 1:2], proj4string=crs(bushes))
p[, iscore := extract(interface, px)]
p[, iscore_tukey := 1/iscore]  # 1/y = reciprocal normalization
hist(p$iscore_tukey)

# plot -------------------------------------
pdf(file=namer("vegetation_snares"),
    pointsize=pointsize,
    paper=orientation,
    width=9)
par(mar=c(1,1,1,1))
plot(Zcut, col=viridis(3, alpha=0.3, option="D"),
     main="Snare locations vis-a-vis vegetation",
     ribside="right", ribwid=0.02,
     box=F)
plotstuff(plot.snares=T)
dev.off()


# staffing at points of interest ======================================================

pi2 = poi[poi$type != "lake" &
           poi$type != "settlement" &
           poi$type != "town",]
pi2$type = stringr::str_to_title(pi2$type)
colnames(pi2)[2]="Waypoint type"

pit = st_transform(pi2, crs=32736)
temp=data.frame(pit) %>% cbind(., st_coordinates(pit)) %>% setDT
temp[waypoint=="Melia Gate", Y:=Y-200]
temp[waypoint=="Diatomite Gate", X:=X+25]
temp[waypoint=="Utut Gate", X:=X-25]
temp[waypoint=="Sleeping Warrior lodge", X:=X-50]

# split in areas (temp2) and other (temp1)
temp1=temp[Waypoint.type != "Area",]
temp1[Waypoint.type == "Office" & day == 6, day := day - 5] # remove shifts
temp1[Waypoint.type == "Office" & night == 7, night := night - 4]
temp2=temp[Waypoint.type == "Area",]
temp3=temp2[night==0, night := NA]
temp3[day == 0, day := NA]
temp4=temp1[night==0, night := NA]

ampfactor=150

# plot  --------------------------------------------------------------------------

pdf(namer("staffing"),
    paper=orientation,
    pointsize=pointsize,
    width=9)

par(mfrow=c(1,2), mar=c(5,1,1,1))

# day
plot(Window(snares), main="Day", col="grey95", cex.main=1.2)    # window
plot(soysambu_roads_psp, col="red", add=T, style="none", show.all=F)  # roads
symbols(add=T, x=temp1$X, y=temp1$Y, circles=temp1$day*ampfactor, inches=F, ann=T, bg="yellow", fg="black") # gates
points(x=temp1$X, y=temp1$Y, pch=as.character(temp1$day), cex=0.8)    # label gates
symbols(add=T, x=temp2$X, y=temp2$Y, circles=temp2$day*ampfactor, inches=F, ann=T, bg="orange")  # areas
points(x=temp2$X, y=temp2$Y, pch=as.character(temp2$day), cex=0.8)    # label areas
text(temp1$X, temp1$Y, labels=temp1$waypoint, pos=1, cex=0.6)
text(temp2$X, temp2$Y, labels=temp2$waypoint, pos=1, cex=0.6)


# night
plot(Window(snares), main="Night", col="grey90", cex.main=1.2)   # window
plot(soysambu_roads_psp, col="red", add=T, style="none", show.all=F)  # roads
symbols(add=T, x=temp1$X, y=temp1$Y, circles=temp1$night*ampfactor, inches=F, ann=T, bg="yellow", fg="black") # gates
points(x=temp4$X, y=temp4$Y, pch=as.character(temp4$night), cex=0.8) # label gates
symbols(add=T, x=temp2$X, y=temp2$Y, circles=temp2$night*ampfactor, inches=F, ann=T, bg="green", fg="black")  # areas
points(x=temp3$X, y=temp3$Y, pch=as.character(temp3$night), cex=0.8)  # label areas
text(temp1$X, temp1$Y, labels=temp1$waypoint, pos=1, cex=0.6, col="black")
text(temp2$X, temp2$Y, labels=temp2$waypoint, pos=1, cex=0.6, col="black")

par(mfrow=c(1,1))

dev.off()

# processing in bash ========================================================

gc()

setwd(here::here("inst", "extdata", "figures"))
system("./process.sh")
