#' snares for Hellen:
#' get total snares and put into quadrat count
#'

# set up --------------------------------------------

library(Soysambu)
library(spatstat)

data(all_snares)

filepath= here::here("inst","extdata","klad") %>% paste0("/")
pointsize=8
orientation="a4r"

namer = . %>% paste0(filepath, ., ".", filetype)

# setwd(here::here("inst","extdata","klad"))

# make ppp object ----------------------------------------
x=Window(all_snares)$xrange
y=Window(all_snares)$yrange

xs=seq(x[1],x[2]+1000,1000)
ys=seq(y[1],y[2]+1000,1000)

# split into live and dead -------------------------

# all snares
pdf(file=namer("all_snares"),
    pointsize=pointsize,
    paper=orientation,
    width=13, height=10)
quadratcount(all_snares, xbreaks=xs, ybreaks=ys) %>% plot(main="All snares")
plotstuff()
dev.off()


# split
dead=subset(all_snares, live_dead=="DEAD")
pdf(file=namer("dead_snares"),
    pointsize=pointsize,
    paper=orientation,
    width=13, height=10)
quadratcount(dead, xbreaks=xs, ybreaks=ys) %>% plot(main="Dead snares")
plotstuff()
dev.off()


live=subset(all_snares, live_dead=="LIVE")
pdf(file=namer("live_snares"),
    pointsize=pointsize,
    paper=orientation,
    width=13, height=10)
quadratcount(live, xbreaks=xs, ybreaks=ys) %>% plot(main="Live snares")
plotstuff()
dev.off()

# write to xlsx
library(openxlsx)
temp1=all_snares %>% data.frame %>% setDT
setnames(temp1, c("x","y"), c("lon","lat"))
temp1 %>% dt_sf %>% st_set_crs(32736) %>% st_transform(4326) %>% sf_dt %>% write.xlsx(file=here::here("inst","extdata","klad","snares.xlsx"))
