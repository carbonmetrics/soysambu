pacman::p_load(spatstat,Soysambu,sf)

pplot=curry(plot.ppp,use.marks=F,main="")


# split transects --------------------------------------

s=sf_dt(gps_tracks)
s[, trackdate := lubridate::ymd_hms(time) %>% as.Date]
tr=ppp(x=s$lon,y=s$lat,window=Window(snares))

# split by track
marks(tr)=factor(s$track_nr)
ss=split(tr)
# plot(ss)

# make ppp of tracks with track nr and date ------------

tr=unmark(tr)
marks(tr)=data.frame(
  trackdate=factor(s$trackdate),
  tracktime=s$time,
  tracknr=s$track_nr,
  name=s$new_name
)

st=split(tr,"trackdate",un=F)

# isolate hotspots -------------------------------------

# each site must have been visited 4 times on 4 different dates

f=function(p) {
  x=ppp_dt(p)
  y=unique(x$trackdate)
  z=unique(x$tracknr)

  cat("trackdates:", paste(y,collapse=", "), "\n")
  cat("tracknrs:", paste(z,collapse=", "))
}

# with `identify(tx)`

# quarry
a=st[["2018-12-06"]]
b=st[["2019-01-18"]]
c=st[["2019-03-30"]]
d=st[["2019-03-27"]]

a=subset(a,tracknr==3 & hour(tracktime) <=7)
b=subset(b,name=="Quarry")
c=subset(c, as.numeric(tracktime) < 1553934617)
d=subset(d,name=="Quarry")

quarry=superimpose(a,b,c,d)


# Jolai Gate
e=st[["2019-03-27"]]
f=st[["2019-03-29"]]
g=st[["2019-03-30"]]
h=st[["2019-01-24"]]

e1=subset(e,as.numeric(tracktime) <= 1553677995)
e2=subset(e1, name != "Quarry")
f1=subset(f,as.numeric(tracktime) <=1553853727)
f2=subset(f1,as.numeric(tracktime) > 1553847325)
g1=subset(g, as.numeric(tracktime) >= 1553934617)
h1=subset(h, name=='Jolai Gate')

jolai_gate=superimpose(e2,g1,h1)

# Serena
i=st[["2019-01-17"]]
j=st[["2019-03-27"]]
k=st[["2019-03-29"]]
l=st[["2019-04-01"]]

i1=subset(i, as.numeric(tracktime) <= 1547712785)
j1=subset(j, as.numeric(tracktime) >= 1553705672)
k1=subset(k, as.numeric(tracktime) >= 1553861311)
l1=subset(l, name=="Serena B")

serena=superimpose(i1,j1,k1,l1)
# pplot(serena)

# extract from snares database
data(snares)

# make data.table out of snares
s=ppp_dt(snares)

# make data.table out of tracks
serena.df=ppp_dt(serena)[, label := "Serena"]
jolai.df=ppp_dt(jolai_gate)[, label := "Jolai Gate"]
quarry.df=ppp_dt(quarry)[, label := "Quarry"]
df=rbind(serena.df,jolai.df,quarry.df)
df[, name := NULL]
df[, idx := 1:.N,label]

# find seperate hotspots
serena.win=convexhull.xy(df[label=="Serena"])
jolai.win=convexhull.xy(df[label=="Jolai Gate"])
quarry.win=convexhull.xy(df[label=="Quarry"])

# make ppp
serena.s=snares[serena.win]
jolai.s=snares[jolai.win]
quarry.s=snares[quarry.win]

# split ppp on date
marks(jolai.s)$date=factor(marks(jolai.s)$date)
jolai.split=split(jolai.s, "date")

marks(serena.s)$date=factor(marks(serena.s)$date)
serena.split=split(serena.s, "date")

marks(quarry.s)$date=factor(marks(quarry.s)$date)
quarry.split=split(quarry.s, "date")

# plot(unmark(jolai.split))
# plot(unmark(serena.split))

# plot(jolai.s,which.marks="date")
serena.ss=subset(serena.s,date != "2019-03-29")
marks(serena.ss)$date=droplevels(marks(serena.ss)$date)
# plot(serena.ss,which.marks="date")

# plot(serena.ss %mark% nndist(serena.ss),markscale=0.3)
# plot(jolai.s %mark% nndist(jolai.s), markscale=0.3)

marks(snares)$datex=marks(snares)$date=factor(marks(snares)$date)
split(snares,"datex") %>% plot(use.marks=F)

serena=subset(snares, datex %in% c("2018-12-05","2019-01-17","2019-03-27"))
plot(snares, use.marks=F)
plotstuff()

# a=clickbox()
a=structure(list(type = "rectangle", xrange = c(860425.277862241,
                                                861286.980503061), yrange = c(9955395.72812958, 9956372.85514261
                                                ), units = structure(list(singular = "unit", plural = "units",
                                                                          multiplier = 1), class = "unitname")), class = "owin")
snares[a] %>% plot(use.marks=F,pch="+")
serena=snares[a]
serena=subset(serena,datex %in% c("2018-12-05","2019-01-17","2019-03-27"))
marks(serena)$datex=droplevels(marks(serena)$datex)
plot(serena,which.marks="datex",main="")
chs=convexhull.xy(serena)
plot(chs,add=T,border="lightgrey",main="")

round(area(chs)/(100*100)) # area in hectare


jolai.gate=subset(snares, datex %in% c("2019-03-26","2019-03-27","2019-03-29","2019-01-24"))
# b=clickbox()
b=structure(list(type = "rectangle", xrange = c(855345.956837355,
                                                856143.214070323), yrange = c(9945580.51526661, 9946647.26790227
                                                ), units = structure(list(singular = "unit", plural = "units",
                                                                          multiplier = 1), class = "unitname")), class = "owin")
jolai.gate=snares[b]
marks(jolai.gate)$datex=droplevels(marks(jolai.gate)$datex)
plot(jolai.gate, which.marks="datex",main="")
chj=convexhull.xy(jolai.gate)
plot(chj,add=T,border="lightgrey", main="")
round(area(chj)/(100*100)) # area in hectare
