#' GPS files
#'
#' Read GPS information (waypoints and tracks) from 2 different GPS units.
#' There are 2 GPS units: etrex10 (yellow) and an etrex20 (red).
#'

# set up ----------------------------------------------------------

overwrite=T
libs=c("foreach", 'data.table', "Soysambu", "tmap", "tmaptools", "raster", "sf", "lubridate", "spatstat")
lapply(libs, library, character.only=T, quietly=T, verbose=F)

# data(soysambu_boundaries)

# source(here::here("R", "plotstuff.R")) # helper function for plotting

# helper function for checking data points on map
f = function(x) {
  plotstuff(add=F)
  with(x, points(lon,lat, cex=0.5, col="steelblue"))
}

# read ------------------------------------------------------------

# garmin etrex10
setwd(here::here("inst", "extdata", "GPS_yellow"))
system("detox *") # make file names portable

gps_yellow = getPoints()
gps_yellow$label = "GPS YELLOW"

gps_yellow_tracks = getTracks()
gps_yellow_tracks$label = "GPS YELLOW TRACKS"

# garmin etrex20
setwd(here::here("inst", "extdata", "GPS_red"))
system("detox *") # make file names portable

gps_red = getPoints()
gps_red$label = "GPS RED"

gps_red_tracks = getTracks()
gps_red_tracks$label = "GPS RED TRACKS"

# combine
gps_points = rbind(gps_yellow, gps_red)
gps_tracks = rbind(gps_yellow_tracks, gps_red_tracks)

setwd(here::here())


# clean GPS points -----------------------------------------------

# convert to projection UTM36S
gps_points = gps_points %>% st_transform(32736)

# convert sf to data.table
df = sf_dt(gps_points)

# convert time factors to time class and cmt to character
df[, `:=` (
  time = {
    t1 = as.character(time)
    t2 = lubridate::ymd_hms(t1)
  },
  cmt = as.character(cmt)
)]

# remove duplicates
df=unique(df, by=c("time", "name")) # somewhere somehow etrex10 files ('yellow') ended up in the etrex20 ('red') folder

# remove blanks
df=df[!is.na(cmt)]

# sort and assign unique ID
setkey(df, time)
df[, idx := .I]

# pre-assign fields for snare categorization
df[, `:=` (
  cat = character(),
  live.dead = character(),
  nr = integer(),
  dummy = cmt
)]

# Isolate waypoints from Soysambu: after 2018-11-20
df = df[time >= as.Date("2018-11-20"),]

# Waypoint name categories
start.names = c("START", "ST", "ST SERENAB", "ASTART", "WT", "STARU")
end.names = c("END")
dead.snares = c("D", "DS", "DD", "DDD", "OLD DEAD NOT ATTACH", "DS GROUPTREES", "LOOSE SN",
                "SDEADGAZ", "WIRE D", "DEAD TR", "D OLD", "LOOSE D", "LINE D")
live.snares = c("L", "LS", "L NEW", "LIVE")
generic.snares = c("MY S", "OLDS", "AS", "S", "BIGS", "BS", "SNA", "SNARE", "ASN ZEBRA",
                   "LOOSE LIVE", "S HEDGE", "SHEDGE", "BIG L", "BIG SN", "S KIL", "SDEADGAZ")
ground.snares = c("GS", "AGS", "XGS", "PGS", "GRD", "LANDMINE", "GS NEW")
dead.ground.snares = c("GSHOLE", "GS PREP", "LOOSE D", "GS LOC",
                       "HOLE GS", "DGS")
waypoints = c("23L14D", "31L14D2GS", "18 L 27D", "HOUSE", "OPENPLAINEDGE", "QUARY", "CHECK ISOL TREE",
              "CORNER", "ROAD", "UNDER POWERLINE EFENCE", "HOLE", "PUNDA MILIA AMP", "BOOSTER")

# dummy column to extract number of snares from without touching comments
df[, nr := stringr::str_extract(dummy, "\\d+")]
df[, dummy := stringr::str_replace_all(dummy, "\\d+", "")]

# categorization

df[dummy %in% end.names, cat := "WAYPOINT"]
df[dummy %in% start.names, cat := "WAYPOINT"]
df[dummy %in% dead.snares, `:=` (
  cat = "SNARE",
  live.dead = "DEAD"
)]

df[dummy %in% generic.snares, `:=` (
  cat = "SNARE",
  live.dead = "LIVE"
)]

df[dummy %in% ground.snares, `:=` (
  cat = "GROUND SNARE",
  live.dead = "LIVE"
)]

df[dummy %in% live.snares, `:=` (
  cat = "SNARE",
  live.dead = "LIVE"
)]

df[cmt == "23L14D", `:=` (
  name = "ABATTOIR",
  cat = "WAYPOINT",
  dummy = "ABATTOIR",
  nr = NA
)]

df[cmt == "31L14D2GS", `:=` (
  name = "UNKNOWN",
  cat = "WAYPOINT",
  nr = NA
)]

df[cmt == "18 L 27D", `:=` (
  name = "UNKNOWN",
  cat = "WAYPOINT",
  nr = NA
)]

df$dummy=NULL

df[cmt %like% "GATE", `:=` (
  cat = "WAYPOINT"
)]

df[name == "JOLAI GATE", `:=` (
  cmt = "JOLAI GATE",
  cat = "WAYPOINT"
)]

df[name == "KOKOTO GATE", `:=` (
  cmt = "KOKOTO GATE",
  cat = "WAYPOINT"
)]

df[name == "MAIN GATE", `:=` (
  cmt = "MAIN GATE",
  cat = "WAYPOINT"
)]

df[cmt == "TEREWAS A S HERE", `:=` (
  cmt = "SNARE LOCATION",
  cat = "SNARE",
  live.dead = "DEAD"
)]

df[, nr := as.integer(nr)] # to avoid warning in next command that sets nr of snares

df[cmt == "LD", `:=` (
  cat = "SNARE",
  live.dead = "LIVE",
  nr = 2
)]

df[cmt == "KUNI", cat := "ILLEGAL FIREWOOD"]
df[cmt == "MOTORC TRACK", cat := "ILLEGAL TRANSPORT"]

df[cmt == "3WATERTHROUGHS", `:=` (
  cat = "WAYPOINT",
  nr = NA
)]

df[cmt == "KANGA S", `:=` (
  cat = "GUINEA FOWL TRAP"
)]

df[cmt == "GSHOLE", `:=` (
  cmt = "GS PREP",
  cat = "GROUND SNARE",
  live.dead = "DEAD"
)]

df[cmt == "LIVE DEAD 2", `:=` (
  cat = "SNARE",
  live.dead = "LIVE",
  nr = 2
)]

df[cmt == "L1 D1", `:=` (
  cat = "SNARE",
  live.dead = "LIVE",
  nr = 2
)]

df[cmt == "LIVE ISOL TREE", `:=` (
  cat = "SNARE",
  live.dead = "LIVE"
)]

df[cmt == "MODFENCE", cmt := "DAMAGED FENCE"]

df[cmt == "HIDDEN L OPEN AREA", `:=` (
  cat = "SNARE",
  live.dead = "LIVE",
  nr = 1
)]

df[cmt == "L1D2", `:=` (
  cat = "SNARE",
  live.dead = "LIVE",
  nr = 3
)]

df[cmt == "5 D 10M APART", `:=` (
  cat = "SNARE",
  live.dead = "DEAD",
  nr = 5
)]

df[cmt == " D ISOL TREES", `:=` (
  cat = "SNARE",
  live.dead = "DEAD",
  nr = 1
)]

df[cmt == "GS PREP", `:=` (
  cat = "GROUND SNARE",
  live.dead = "DEAD",
  nr = 1
)]

df[cmt == "KANGA S", `:=` (
  live.dead = "DEAD",
  nr = 1
)]

df[cmt == "IRE", cmt := "WIRE"]
df[cmt =="ENTRY PINT", cmt := "ENTRY POINT"]
df[cmt == "ASTART", cmt := "START"]
df[cmt == "WT", cmt := "START"]

df[cmt == "P", cmt := "Poacher caught"]
df[cmt == "HOLE", cmt := "Open sceptic tank"]
df[cmt == "PAUGH", cmt := "Poacher found"]
df[cmt == "ASN ZEBRA", cmt := "Snared zebra, dead"]
df[cmt == "MAIN QUES GATE", cmt := "Main gate"]
df[cmt == "OPENPLAINEDGE", cmt := "Edge of open plain"]
df[cmt == "SDEADGAZ", cmt := "Snared dead impala"]
df[cmt == "P", cmt := "Poacher caught"]
df[cmt %like% "NOTHIG", cmt := "NOTHING, OPEN AREA"]
df[cmt == "QUARY", cmt := "QUARRY"]
df[cmt == "ASUNBIRD LODGE", cmt := "sunbird lodge"]
df[cmt == "F ORDST", cmt := "forest"]
df[cmt == "BRRIER GATE", cmt := "BARRIER GATE"]
df[, cmt := toupper(cmt)]

df[cat %like% "SNARE" & is.na(nr), nr := 1]
df[is.na(cat), cat := "WAYPOINT"]
df[cat == "SNARE", cat := "NECK SNARE"]

df[cmt == "L2 ZEBRA", `:=` (
  cat = "LIVE",
  nr = 2
)]

# remove repeat transects snares and observations for Jolai Gate, Serena B and Quarry ------------
df[, label := "old"]

# get new transect points
new=df[time > lubridate::ymd_hms("2019-03-26 00:00:00")]
mwarikiC=new[time < lubridate::ymd_hms("2019-04-01 11:51:50") &
              time > lubridate::ymd_hms("2019-04-1 00:00:00"),]
raptorW=new[time <= lubridate::ymd_hms("2019-03-29 10:01:30") &
                          time > lubridate::ymd_hms("2019-03-29 08:10:15 ")]
obshill=new[time <= lubridate::ymd_hms("2019-03-29 08:10:15") &
              time >= lubridate::ymd_hms("2019-03-29 07:10:29")]
nyumba=new[time>=lubridate::ymd_hms("2019-03-27 11:26:44") &
             time<=lubridate::ymd_hms("2019-03-27 11:48:36")]

plotstuff(add=F)
with(mwarikiC, points(lon,lat))
with(raptorW, points(lon,lat))
with(obshill, points(lon,lat))
with(nyumba, points(lon,lat))

mwarikiC[, label:="MwarikiC"]
raptorW[, label:="RaptorW"]
obshill[, label:="Observation Hill"]
nyumba[, label := "Nyumba Mbili"]
news=rbind(mwarikiC, raptorW, obshill, nyumba)

# get repeat points
reps=new[idx %nin% news$idx,]
reps[, label := "repeats"]

# filter out the new values (reps, news) and add the labeled versions obtained above.
temp=df[idx %nin% new$idx,]  # remove all new points (after 2019-03-27)
dfx=rbind(temp, news, reps)

# refine the labels for news and repeats
dfx[name %between% c(303,314), label := "Jolai Gate rep0"]
dfx[name %between% c(343,355), label := "Jolai Gate rep1"]
dfx[name %between% c(437,439), label := "Jolai Gate rep2"]

dfx[name %between% c(315,331), label := "Quarry rep0"]
dfx[name %between% c(332,342), label := "Quarry rep1"]
dfx[name %between% c(433,436), label := "Quarry rep2"]

dfx[name %between% c(356,368), label := "Serena B rep0"]
dfx[name %between% c(416,432), label := "Serena B rep1"]
dfx[name %in% c(443,444), label := "Serena B rep2"]

# corrections and explanations for repeats
# removed by poacher
dfx[cmt == "REM", `:=` (
  cat="NECK SNARE",
  live.dead=NA,
  nr=0
)]

# no change: snare was previously cut by rangers, not detected by poacher
dfx[cmt %like% "CUT" | cmt %like% "NOCH", `:=` (
  cat = "NECK SNARE",
  nr=0
)]

# discover new snares: missed by rangers
dfx[cmt=="CUT MISS", `:=` (cat="NECK SNARE", nr=0)] # cut snare missed during repeat
dfx[cmt=="L MISS", `:=` (cat="NECK SNARE", nr=1)] # live snare missed during repeat
dfx[cmt=="D MISS", `:=` (cat="NECK SNARE", nr=1)] # dead snare missed during repeat

dfx[cmt=="L2 ZEBRA", `:=` (cat="NECK SNARE", live.dead="LIVE", nr=2)]   # adult zebra snared, escaped with snare around neck
dfx[cmt=="L DEADZEB", `:=` (cat="NECK SNARE", live.dead="LIVE", nr=1)]  # dead young zebra just snared

dfx[cmt=="D NEW", `:=` (cat="NECK SNARE", live.dead='DEAD', nr=1)]

#' remark:
#' initially I followed up each individual snare and logged it during repeats.
#' later on, I just identified them with the GPS and did not log anything if there was no change.
#' so for analysis you only need changes: new, removed and missed.
#' so repeat initial locations for rep0, then mutate.


# correct for locations where you find dead and live snare in the same point -----------------------

# isolate cases where there is a combination of multiple snares at the same location
t1=dfx[stringr::str_detect(cmt, "L") & stringr::str_detect(cmt, "D") & name %nin% c("UNKNOWN","ABATTOIR")]
t2=t1[stringr::str_detect(cmt, "\\d+") | cmt=="LD" | stringr::str_detect(cmt, "LIVE DEAD")]

# number of snares
snare.sums = stringr::str_extract_all(t2$cmt, "\\d+") %>% lapply(as.numeric) %>% lapply(sum) %>% unlist
t2[, snares := snare.sums]
t2[cmt=="LD", snares := 2]

# row expansion
t3=t2[rep(1:.N, snares)]                    # <----- brilliant trick for row expansion, https://stackoverflow.com/questions/29995920/replicating-rows-in-data-table-by-column-value/29996558#29996558
t3[, grp := .GRP, name]
t3[, id := 1:.N, grp]
t3[id == 1, `:=` (
  live.dead="LIVE",
  nr=1
)]
t3[id > 1, `:=` (
  live.dead="DEAD",
  nr=1
)]
t3[, c("id","snares","grp") := NULL]

# set snares category
t3[, cat := "NECK SNARE"]   # it is never the case that multiple live/dead groundsnares are found in the same location

# remove t3 indices from df and and add t3
df1=dfx[idx %nin% t3$idx,]
dfx=rbind(df1, t3)
rm(df1)

# add extra index to avoid duplicate warning
dfx[, id.dup := 1:.N, name]

# remove waypoints that have no numeric name
dfx=dfx[!stringr::str_detect(name, "\\D+"),]

# sort on time, chronological
setkey(dfx, time)

# set back to sf object and set projection ------------------------

gps_points = dt_sf(dfx) %>%
  sf::st_set_crs(32736) %>%
  sf::st_transform(32736)
# https://epsg.io/?q=Kenya%3B+Tanzania%3B+Uganda; UTM36S note that this is Elmenteita; e.g. for Bogoria you cross the equator and need apply 32636

# clean out points that are not within the boundaries of Soysambu
# find intersection - include only GPS points that are within the boundaries of the conservancy
gps_points = sf::st_intersection(gps_points, soysambu_boundaries)

gps_points$Area=NULL
gps_points$Habitat=NULL
gps_points$Id=NULL

# store
usethis::use_data(gps_points, overwrite=overwrite) # see file GPS_data.R for documentation


# ====================================================

# process gps_tracks

# set to UTM36S
gps_tracks=gps_tracks %>% st_transform(32736)

# remove points outside the boundaries
gps_tracks=sf::st_intersection(gps_tracks, soysambu_boundaries)

# convert to data.table
x = sf_dt(gps_tracks)

# clean up ----------------------------------
# convert to time class
x[, time := {
  t1 = as.character(time)
  t2 = lubridate::ymd_hms(t1)
}]

x[, c("Id", "Habitat","Area") := NULL]

# remove duplicates
x =unique(x, by="time")

# Isolate waypoints from Soysambu: after 2018-11-20
x = x[time >= as.Date("2018-11-20"),]

# sort on time
setkey(x, time)

# remove repeat transects for Jolai Gate, Quarry and Serena B

# assign unique ID
x[, idx := .I]

# Garmin tracking deafult is "auto", which is not a good idea.
# it records at a variable rate (time, distance) optimized to show the track, but no documentation on algorithm.
# so calculate speed manually

# prepare for row diff distance
x[, `:=` (
  times=data.table::shift(time, 1, 0),
  lons=data.table::shift(lon, 1, 0),
  lats=data.table::shift(lat,1,0)
)]

# Euclidean distance
x[, dist := {
  l1=(lon-lons)^2
  l2=(lat-lats)^2
  l3=sqrt(l1+l2)
  l4=as.integer(l3)
}]

# calculate speed
x[, timediff := as.integer(time-times)]

x[, speed := {
  t1=timediff
  t2=as.integer(t1)
  t3=dist/t2 # m/s
  t4=t3/1000*3600 # km/h
  t5=round(t4,1)
}]

# check and stats ------------------
plotstuff(add=F)
with(x[speed<4,], points(lon, lat))

trackstats = x[lats != 0, .(
  n=.N,
  dist=sum(dist)
), date]

library(foreach)
foreach(i = 1:nrow(trackstats)) %do% {
  track=trackstats[i, date]
  tracks=x[date==track & lats != 0,]
  f(tracks)
  title(paste("track", i, ":", track))
}

# name tracks ...

tracknames = data.table(
  track=c(
    48,47,46,45,44,43,42,41,40,
    39,38,37,36,35,34,33,32,31,30,
    29,28,27,26,25,24,23,22,21,20,
    19,18,17,16,15,14,13,12,11,10,
    9,8,7,6,5,4,3,2,1
  ),
  name=c(
    "Serena B", "Obs","Mwariki C", "Quarry, Raptor W, Obs Hill, Jolai Gate, Serena B, Nyumba Mbili", "Quarry","Obs","Obs","Obs","Routes",
    "Obs", "Routes", "Twiga Plains 61","Elmenteita","Jolai S","Obs","Jolai S","Routes","Jolai Hill","Routes",
    "Monkey Bridge","Routes","Obs","Central N and S","Routes","Kokot","Routes","Triangle E and W","Routes","Quarry",
    "Routes and Mbweha","Routes and Mbweha rep","Congreve E","Obs","Jolai Gate N and S","Obs","Routes and Euphorbia","Mbaruk Stores","Obs","Congreve S",
    "D-gate","Booster","Punda Milia","Routes","Routes, Serena, Maendeleo, Nyambu Mbili","Routes, Mwbeha, Elmenteita E, Jolai Plains","Obs, Jolai Plains","Elmenteita E",
    "Serena B, Mwbeha, Maendeleo, Nyambu Mbili, Kokot, HQ"
  )
)

tracknames=tracknames[order(track),]
tracks=cbind(tracknames, trackstats)
all=x[tracks, on="date"][, c("lons", "lats","i.dist","times") := NULL]
setnames(all, c("dist","timediff","speed","track"), c("dist_m", "timediff_s", "speed_km_h", "track_nr"))


# clean up tracks ...

# interactive helper function (requires internet)
fx = function(mydata) {
  temp=dt_sf(mydata) %>% st_set_crs(32736) %>% st_transform(4326)
  tm_shape(temp) + tm_dots()
}

all[name %nin% c("Routes", "Obs"), clean_name := name]
all[name == "Routes and Euphorbia" & idx > 5700, new_name := "Euphorbia"]

clean_names = all$clean_name %>% unique %>% sort

foreach(i = 1:length(clean_names)) %do% {
  track_name = clean_names[i]
  track=all[clean_name == track_name,]
  f(track)
  title(paste("track", unique(track$track_nr), ":", track_name))
}

all[clean_name == "Triangle E and W" & speed_km_h < 5, new_name := "Triangle"]
all[clean_name == "Serena B", new_name := "Serena B"]
all[track_nr == 5 & speed_km_h < 5 & track_seg_point_id %nin% c(0:6,628,632,641), new_name := "North"]
all[track_nr == 4 & track_seg_point_id %between% c(775,1085), new_name := "Jolai Gate"]
all[track_nr == 4 & track_seg_point_id %between% c(486,680), new_name := "Quarry"]
all[track_nr == 4 & track_seg_point_id %between% c(32,291), new_name := "Mbweha"]
all[track_nr == 19 & track_seg_point_id %between% c(27,300), new_name := "Mbweha"]
all[track_nr == 13 & track_seg_point_id %between% c(149,314), new_name := "Euphorbia"]
all[track_nr == 45 & speed_km_h < 5 & track_seg_point_id %nin% c(115:119,280:295), new_name := "Quarry, Jolai Gate, Obs Hill, Leopard Ridge, Obs Hill, Serena B, Nyumba Mbili"]
all[track_nr %in% c(20,44), new_name := "Quarry"]
all[track_nr == 7, new_name := "Punda Milia"]
all[track_nr == 3 & track_seg_point_id %between% c(8,36), new_name := "Quarry"]
all[track_nr == 3 & track_seg_point_id %between% c(44,53), new_name := "Jolai Plains W"]
all[track_nr == 46, new_name := "Mwariki C"]
all[track_nr == 29, new_name := "Monkey Bridge"]
all[track_nr == 12, new_name := "Mbaruk Stores"]
all[track_nr == 24, new_name := "Kokot Gate"]
all[track_nr %in% c(33,35), new_name := "Jolai S"]
all[track_nr == 31, new_name := "Jolai Hill"]
all[track_nr == 15, new_name := "Jolai Gate"]
all[track_nr == 2 & track_seg_point_id %between% c(8,68), new_name :="Elmenteita"]
all[track_nr == 36 & track_seg_point_id %between% c(44,93), new_name := "Elmenteita"]
all[track_nr == 9 & track_seg_point_id %between% c(5,300), new_name := "D Gate"]
all[track_nr == 10 & track_seg_point_id %between% c(117,329), new_name := "Congreve S"]
all[track_nr == 17, new_name := "Congreve N"]
all[track_nr == 26 & track_seg_point_id %nin% c(185:207), new_name := "Central"]
all[track_nr == 8 & track_seg_point_id > 650, new_name := "Booster"]

transects=all[!is.na(new_name)]
transects[track_nr == 3, new_name := NA]

new_name = transects$new_name %>% unique %>% sort
foreach(i = 1:length(new_name)) %do% {
  temp=all[!is.na(new_name),]
  track_name = new_name[i]
  track=temp[new_name == track_name,]
  f(track)
  title(paste("track", unique(track$track_nr), ":", unique(track$new_name)))
}

par(mfrow=c(2,2))
transects[!is.na(new_name),]  %>% f
title("transects")
all %>% f
title("all")
transects[is.na(new_name),] %>% f
title("no new name")

par(mfrow=c(1,1))

transects[, clean_name := NULL]

# convert back to sf and set projection
gps_tracks = dt_sf(transects) %>%
  sf::st_set_crs(32736) %>%
  sf::st_transform(crs = 32736)
# https://epsg.io/?q=Kenya%3B+Tanzania%3B+Uganda.

# NB these are all the tracks, including single measurement and routes.
# subset to !is.na(new_name) to get transects.

# store
usethis::use_data(gps_tracks, overwrite=overwrite)


# --------------------------------------------------------



# get basemap for Lake Elmenteita ---------------------------------
# elmenteita_basemap = tmaptools::read_osm(gps_points, ext=2, type="osm")

# store
# raster::writeRaster(elmenteita_basemap, filename = "./inst/extdata/spatial/soysambu_basemap.tif")

