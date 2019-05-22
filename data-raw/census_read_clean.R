#' Soysambu census raw data processing
#'
#' Takes the spreadsheet /home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/201805_SoyWildlifeCensusAccumulated  (version 1).xlsx
#' and reads each individual sheet. Only total counts per animal per census are stored.

# set up ============

pacman::p_load(lubridate, stringr, readxl, data.table, foreach, zoo,
               ggplot2, Soysambu, gganimate, pacman)
overwrite=F

census.file="/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/other_data/201905_SoyWildlifeCensusAccumulated  (version 1).xlsx"

mysheets = readxl::excel_sheets(census.file)
date.sheets = mysheets[stringr::str_detect(mysheets, "\\d{4}")]
census.sheets = which(mysheets %in% date.sheets)

# clean up function

l = foreach(i=1:length(census.sheets)) %do% {

  # read
  x = read_excel(census.file, sheet=census.sheets[i],col_names=F)
  setDT(x)
  setnames(x,paste0("col",1:ncol(x)))

  # set everything to lowercase
  x=x[, lapply(.SD,tolower)]

  # replace "totals" with "total" and "animal" with "species"
  x=x[, lapply(.SD,function(x) str_replace_all(x, "totals","total"))]
  x=x[, lapply(.SD,function(x) str_replace_all(x,"animal","species"))]

  # detect the word "Totals"
  totals=x[, lapply(.SD,function(x) grep("total",x))]

  # consolidate data

  # get column 1 and the column with "totals" in it
  y=x[, c("col1",names(totals[,2])),with=F]
  setnames(y,c("species","count"))

  # remove junk
  y=y[!is.na(count),]
  y=y[species != "total"]
  y=y[species != "species"]

}

# bind results into database
names(l) = date.sheets
ll = rbindlist(l, idcol=T)
setnames(ll, ".id","date")

# the big cleanup
ll[, Species := species %>% str_to_title %>% str_squish()]
ll[, Species := str_replace(Species, "\\.$", "")]
ll=ll[Species!="Species",]
ll=ll[Species!="Total"]

ll[Species=="Aadvark", Species := "Aardvark"]
ll[Species == "Bat Eared Fox", Species := "Bat-Eared Fox"]
ll[Species == "Bb.jackals" | Species == "B.b.jackals" | Species=="B.b. Jackal", Species := "Bb. Jackal"]

ll[Species == "Colobus", Species := "Colobus Monkey"]
ll[Species == "Gol-Jackals", Species := "Gol-Jackal"]
ll[Species == "Grants G", Species := "Grants Gazelle"]
ll[Species == "M'sai Giraffe", Species := "Masai Giraffe"]
ll[Species == "Mtn Reedbuck" | Species=="Mtn. Reedbuck", Species := "Mountain Reedbuck"]
ll[Species == "Roth Giraffe", Species := "Rothschild Giraffe"]
ll[Species == "Rothschilds Giraffe" | Species == "Giraffe", Species := "Rothschild Giraffe"]
ll[Species == "Steinbuck", Species := "Steinbok"]
ll[Species == "Stripped Hyena", Species := "Striped Hyena"]
ll[Species == "Sykes" | Species == "Sykes M", Species := "Sykes Monkey"]
ll[Species == "Thomsons Gazelle" | Species == "Tommies", Species := "Thompson Gazelle"]
ll[Species == "Vervet" | Species == "Vervet M", Species := "Vervet Monkey"]
ll[Species == "Vultures", Species := "Vulture"]
ll[Species == "Warthogs", Species := "Warthog"]
ll[Species == "Wildebeast", Species := "Wildebeest"]
ll[Species == "Bushbuck", Species := "Bush Buck"]
ll[Species == "Bushpig" | Species == "Bushpigs", Species := "Bush Pig"]
ll[Species == "H Badger" | Species == "H. Badger", Species := "Honey Badger"]
ll[Species == "Serval", Species := "Serval Cat"]
ll[Species == "Sid St-Jackals", Species := "Sid St-Jackals"]
ll[Species == "Springhare", Species := "Spring Hare"]
ll[Species=="Guinea Fowl (Flocks)", Species := "Guinea Fowl Flocks"]
ll[Species=="Sid St-Jackals",Species:="Sid St-Jackal"]

ll[, CensusDate := tolower(date) %>% str_to_title]
ll[, CensusDate := {
  t1 = str_extract(CensusDate, "\\D+")
  t2 = substr(t1, 1, 3)
  t3 = str_extract(CensusDate, "\\d{4}")
  t4 = paste(t2,t3)
  t5 = as.yearmon(t4)
  t6 = as.Date(t5)
}]

ll[, Count := as.integer(count)]

df = ll[!is.na(Species)]

df=ll[, .(CensusDate,Species,Count)]


# plot ==============

# remove species that have a low total count ...
nulls=df[, .(count=sum(Count)), Species][count<10,Species]

# ...or with a low number of observations
lowcounts=df[, .N,Species][N<5,Species]

# cleanup
dfx=df[Species %nin% nulls,]
dfx=dfx[Species %nin% lowcounts,]

# static plot -----
theme_set(theme_bw())

ggplot(dfx,aes(x=CensusDate,y=Count))+
  geom_point(shape=1,size=0.7,col="grey60")+
  stat_smooth(geom='ribbon', aes(ymin = ifelse(..ymin.. < 0, 0, ..ymin..)),alpha = .3)+
  theme(text=element_text(size=9,  family="Aller"))+
  facet_wrap(Species~.,scales="free_y")

# animated plot -----

myspecies=c("Thompson Gazelle","Dikdik","Zebra","Eland")

p=ggplot(dfx[Species %in% myspecies], aes(x=CensusDate,y=Count))+
  geom_line()+
  facet_wrap(Species~.,scale="free_y")

px=p+geom_point()+
  transition_reveal(CensusDate)

animate(px,fps=4)

setwd("~/Documents/PhD/Soysambu/Soysambu/inst/extdata/other_data")
anim_save(filename="census_animated.gif")


# close out -----

# register in R package
if(overwrite){
  census_data = df
  usethis::use_data(census_data, overwrite=overwrite) # see file CensusData.R for documentation
}


