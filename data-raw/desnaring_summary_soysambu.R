#' Analysis of snaring report spreadsheet
#' The xls does not split out snares found per area, but it contains names and dates.
#' It can therefore be used at least for an estimation of patrol effort for selected hotspots.

# set up
pacman::p_load(readxl, data.table, stringr, maditr, zoo, lubridate,knitr,sf,Soysambu,spatstat)

# read
x = read_excel("./inst/extdata/other_data/SnaringReport.xlsx", skip=1) %>% setDT
setnames(x, names(x) %>% tolower %>% make.names)

# database of locations
y=x[, .(location,latitude,longitude, no.of.snares,date)]

y[, location:= location %>%
    tolower %>% str_squish %>%
    str_replace("totals","total") %>% str_replace("area","") %>% str_replace("from","") %>%
    str_replace("miliyu","meliyu") %>% str_replace(" ya ","") %>%
    str_replace("hse","house")]

dates=dmy(y$date)
exceldates=y$date %>% as.numeric %>% as.Date(origin="1899-12-30")
y=cbind(y,dates,exceldates)
y[is.na(exceldates), datex:=dates]
y[is.na(datex), datex:= exceldates]
y=y[, c("date","dates","exceldates"):=NULL]
y[, datex:=na.locf(datex)]


# get coordinates
z=y[!is.na(latitude), .(longitude,latitude,datex)]
z[, latitude:=latitude*-1]

lonlat=st_as_sf(z,coords=c("longitude","latitude"),crs=4326)
utm=st_transform(lonlat,crs=32736)

# transform to ppp
a=sf_dt(utm)
b=ppp(x=a$lon,y=a$lat,window=Window(snares), marks=as.factor(utm$datex))
b=as.ppp(b)
plot(b,which.marks="datex", main="")

# split by date
split(b,by="datex") %>% plot(use.marks=F)
