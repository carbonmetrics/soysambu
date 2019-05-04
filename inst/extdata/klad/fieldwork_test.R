# set up =========================================

library(Soysambu)

source(here::here("R", "plotstuff.R"))

setwd(here::here("inst", "extdata", "GPS_yellow", "temp_yellow"))

librarian::shelf(foreach, data.table, raster, sf, spatstat, viridis, maptools)
setwd(here::here("inst","extdata", "GPS_yellow", "temp_yellow"))

# make functions getPoints and getTracks available here
source(here::here("R", "getPoints.R"))
source(here::here("R", "getTracks.R"))
source(here::here("R", "utilities.R"))

# load data ======================================

load(here::here("inst", "extdata", "spatial", "jolai_box.rda"))
load(here::here("inst", "extdata", "spatial", "quarry_box.rda"))
load(here::here("inst", "extdata", "spatial", "serena_box.rda"))

sentinel=raster::raster(here::here("inst","extdata","spatial","conservancy_sentinel.tif"))
lu.im = spatstat::as.im(sentinel)   # fails if maptools is not loaded
W = levelset(lu.im, 2, "==")


# pick up GPS points and tracks ======================

gp=getPoints() %>% st_transform(32736) %>% sf_dt
gt=getTracks() %>% st_transform(32736) %>% sf_dt

gt[, datetime := {
  t1 = as.character(time)
  t2 = lubridate::ymd_hms(t1)
}]

gt[, datetime %>% as.Date %>% unique]
gt[, date := as.Date(datetime)]
# mwarikiC=gt[date == as.Date("2019-04-01")
mwarikiC=gt[datetime < lubridate::ymd_hms("2019-04-01 11:51:50") &
              datetime > lubridate::ymd_hms("2019-04-1 00:00:00"),]

obs=gt[datetime > lubridate::ymd_hms("2019-03-29 00:00:00")]
obs=gt[date == lubridate::ymd("2019-03-29")]
obshill=obs[hour(datetime) < 12,]
obshill[, lons := data.table::shift(lon, 1, 0)]
obshill[, lats := data.table::shift(lat, 1, 0)]
obshill[, dist := {
  l1=(lon-lons)^2
  l2=(lat-lats)^2
  l3=sqrt(l1+l2)
  l4=as.integer(l3)
}]
obshill=obshill[dist<300,]

p=unmark(pointpattern)
plotstuff(snares=F, lake=T, cex=0.6, main="Snares", add=F)
points(obshill$lon, obshill$lat)



# test
p = unmark(pointpattern)
par(mfrow=c(1,2))
plotstuff(snares=F, lake=T, cex=0.6, main="Snares", add=F)
points(gp$lon, gp$lat, cex=0.5, col=viridis(1, alpha=0.5))
plotstuff(snares=F, lake=T, cex=0.6, main="Tracks", add=F)
points(gt$lon, gt$lat, col=viridis(1, alpha=0.5), cex=0.5)
par(mfrow=c(1,1))

#' Cut everything into 3 separate hot spots:
#' 0. clean up points
#' 1. create pointpattern for the entire area
#' 2. create 3 boxes
#' 3. split
#' 4. add points from previous transects?

# pointpattern hs=hotspotsgp=gp[cmt!="NEW CONSTRUCTION" & !is.na(ele)]

# clean up points
gp=gp[cmt!="NEW CONSTRUCTION" & !is.na(ele)]
gp[, `:=` (
  cat = ifelse(cmt %nin% "GS", "NECK SNARE", "GROUND SNARE"),
  snares = stringr::str_extract(cmt, "\\d+"),
  live_dead = character(),
  datetime = lubridate::ymd_hms(time)
)]

gp[, snares := ifelse(is.na(snares), 1, snares)]
live = c("L", "2L", "3L", "L2", "L3", "L2 ZEBRA", "L DEADZEB", "L TREE", "L MISS", "CUT MISS", "CUT", "LEFT CUT", "LEF CUT")
other = c("ST", "START", "END", "BUTCHERING", "KUNI", "OPEN POACHING", "HOLE FECE")
dead = c("EX SNARE", "D", "D2", "D3", "2D", "3D")

gp[cmt %in% live, live_dead := "LIVE"]
gp[cmt %in% dead, live_dead := "DEAD"]
gp[cmt %like% "L DEADZEB", live_dead := "LIVE"]
gp[cmt %like% "GS" & is.na(live_dead), live_dead := "LIVE"]

gp = gp[cmt %nin% other]

gp[cmt %like% "GS HOLE", `:=` (
  cat = "GROUND SNARE",
  live_dead = "DEAD"
)]

gp[, day := .GRP, as.Date(datetime)]

gp[cmt == "REM" & day > 1, status := "REMOVED"]
gp[cmt %like% "CUT" & day > 1, status := "NOCHANGE"]
gp[cmt == "NEW" & day > 1, status := "NEW"]
gp[is.na(status) & day > 1, status := "NOCHANGE"]
gp[cmt %like% "MISS" & day > 1, status := "MISSED"]


# pointpatterns ======================================

# overall----------------------------------------------
hs=with(gp, ppp(x=lon, y=lat, window=Window(pointpattern)))
marks(hs)=data.frame(
  name = gp$name,
  ele = gp$ele,
  datetime=gp$time,
  cmt=gp$cmt,
  cat = gp$cat,
  snares = gp$snares,
  live_dead = gp$live_dead,
  status = gp$status,
  day = gp$day
)

# define boxes
# serena_box=clickpoly(add=T)
serena_box=as.rectangle(serena_box)

# jolai_box=clickpoly(add=T)
jolai_box=as.rectangle(jolai_box)

# quarry_box=clickpoly(add=T)
quarry_box=as.rectangle(quarry_box)

# save boundary box files
save(serena_box, file=here::here("inst", "extdata", "spatial", "serena_box.rda"))
save(jolai_box, file=here::here("inst", "extdata", "spatial", "jolai_box.rda"))
save(quarry_box, file=here::here("inst", "extdata", "spatial", "quarry_box.rda"))

# points ------------------------------------
serena=hs[serena_box]
jolai=hs[jolai_box]
quarry=hs[quarry_box]

# roads ---------------------------------------
roads=soysambu_roads_psp

roads_serena=roads
Window(roads_serena)=Window(serena)

roads_quarry=roads
Window(roads_quarry)=Window(quarry)

roads_jolai=roads
Window(roads_jolai)=Window(jolai)

# plotting

par(mfrow=c(1,3))
plot(serena, use.marks=F)
plot(roads_serena, add=T)
plot(jolai, use.marks=F)
plot(roads_jolai, add=T)
plot(quarry, use.marks=F)
plot(roads_quarry,add=T)

par(mfrow=c(1,2))

q1=quarry %>% subset(day==1)
plot(q1, use.marks=F, main="Quarry, day 1")
q2=quarry %>% subset(day==2)
plot(q2, use.marks=F, main="Quarry, day 2")
pairs=closepairs(quarry, rmax=5, twice=F, neat=T, what="ijd")
q1=quarry[pairs$i]
q2=quarry[pairs$j]

split_day=function(ppp){
  f=factor(marks(ppp)$day)
  splitted=split(ppp, f)
  plot(splitted, use.marks=F, main=paste("Split by day,", substitute(ppp)))
  return(splitted)
}

split_close=function(ppp,rmax){
  cl=closepairs(ppp,rmax=rmax,what="ijd",distinct=T,neat=T)
  found=cl$i %>% sort
  miss=setdiff(1:ppp$n, found) %>% sort
  found=ppp[found]
  miss=ppp[miss]

  par(mfrow=c(1,2))
  plot(found, use.marks=F, main=paste("found back: n=", found$n))
  plot(miss,use.marks=F, main=paste("missing: n=", miss$n))
  par(mfrow=c(1,1))
  return(list(found=found, miss=miss))
}

missing=function(ppp){
  dt=ppp %>% data.frame %>% setDT
  mycols=c("name", "datetime","cmt","cat","snares","live_dead","status")
  dt[, (mycols):= lapply(.SD,as.character), .SDcols=mycols]
  m=dt[status=="MISSED",]
  if(nrow(m)>0){
    cat("correcting for records labeled as missed\n")
    day2=dt[status != "MISSED",]
    day1=dt[is.na(status)]
    dx=rbind(day1, day2)
  }else{
    dx=dt
  }
  dy=dx[, .N,day][, diff(N)]
  return(dy)
}

pp_dt=function(ppp){
  ppp %>% data.frame %>% data.table
}

changes=function(ppp){
  dt=pp_dt(ppp)
  dt[day==2, .N, status]
}

split_close(quarry,rmax=5)$found %>% data.frame

q=split_day(quarry)
cld=crosspairs(q[[1]],q[[2]],what="ijd",rmax=4)

par(mfrow=c(1,3))
stienen(serena)
plot(roads_serena,add=T)
stienen(jolai)
plot(roads_jolai,add=T)
stienen(quarry)
plot(roads_quarry,add=T)
par(mfrow=c(1,1))

# large amt of dead snares
x = gp %>% data.frame %>% setDT
x[, datetime := {
  t1 = as.character(time)
  t2 = lubridate::ymd_hms(t1)
}]

y=x[datetime > lubridate::ymd_hms("2019-03-29 08:34:00 UTC") &
    datetime < lubridate::ymd_hms("2019-03-29 10:01:00 UTC")]

y[, nr := stringr::str_extract(cmt, "\\d+") %>% as.numeric]
y[is.na(nr) & cmt != "ST", nr := 1]


# after running gps_read_clean...:

z=gps_tracks %>% st_transform(32736) %>% sf_dt
z=unique(z, by="datetime")

z[, `:=` (
  datetime={
    t1=as.character(time)
    t2=ymd_hms(t1)
  }
)]

z[, `:=` (
  datetimes=data.table::shift(datetime, 1, 0),
  lons=data.table::shift(lon, 1, 0),
  lats=data.table::shift(lat,1,0)
)]

z[, dist := {
  l1=(lon-lons)^2
  l2=(lat-lats)^2
  l3=sqrt(l1+l2)
  l4=as.integer(l3)
}]

z[, timediff := as.integer(datetime-datetimes)]

z[, speed := {
  t1=timediff
  t2=as.integer(t1)
  t3=dist/t2 # m/s
  t4=t3/1000*3600 # km/h
  t5=round(t4,1)
}]

setkey(z, datetime)
# z=unique(z, by="track_seg_point_id")
# z=z[lons != 0]
# z[, cumdist := cumsum(dist), as.Date(datetime)]
# plot(z$cumdist)

plotstuff(snares=F, lake=T, cex=0.6, main="Snares", add=F)
zz=z[speed<4,]
points(zz$lon, zz$lat)

o = as(soysambu_boundaries, "Spatial") %>% as.owin
p=ppp(zz$lon,zz$lat,o)
p=as.ppp(p) # remove rejects
