#' Soysambu census raw data processing
#'
#' Takes the spreadsheet /home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/201805_SoyWildlifeCensusAccumulated  (version 1).xlsx
#' and reads each individual sheet. Only total counts per animal per census are stored.

# set up ============

librarian::shelf(lubridate, stringr, readxl, data.table, foreach, zoo, ggplot2)
overwrite=F

census.file = "./inst/extdata/other_data/censusdata.xlsx"

mysheets = readxl::excel_sheets(census.file)
date.sheets = mysheets[stringr::str_detect(mysheets, "\\d{4}")]
census.sheets = which(mysheets %in% date.sheets)



# clean up function

l = foreach(i=1:length(census.sheets)) %do% {

  x = read_excel(census.file, sheet=census.sheets[i])
  setDT(x)

  find.species = which(x=="Species" | x == "Animal" | x == "Animal.", arr.ind=T)                 # should occur once
  find.totals = which(x == "Total" | x == "TOTAL" | x == "TOTALS" | x == "Total.", arr.ind=T)    # may occur twice: once in rows, once in cols

  if (nrow(find.totals) == 1) {

    # occurs only in first columns, as row total
    y = x[find.species[1]:(find.totals[1]-1), ]

    # find totals in columns
    first.row = y[1,] %>% as.character %>% tolower
    last.col = which(first.row %like% "total")
    last.col.name = colnames(y)[last.col]

    z = y[, .SD, .SDcols=c(1,last.col)]
    setnames(z, c("Species", "Totals"))
    z = z[-1,]

  } else {

    # trim data.table by using totals in first column
    last.row = find.totals[1,1]
    y = x[find.species[1]:(last.row-1),]

    # the second "totals" must refer to a column
    last.col = find.totals[2,2]
    z = y[, .SD, .SDcols=c(1,last.col)]
    setnames(z, c("Species", "Totals"))
    z = z[-1,]

  }
}

names(l) = date.sheets
ll = rbindlist(l, id=T)

ll[, Species := tolower(Species) %>% str_to_title %>% str_squish()]
ll[, Species := str_replace(Species, "\\.$", "")]

ll[Species=="aadvark", Species := "Aardvark"]
ll[Species == "Bat Eared Fox", Species := "Bat-Eared Fox"]
ll[Species == "Bb.jackals" | Species == "B.b.jackals", Species := "Bb. Jackal"]
ll[Species == "Colobus", Species := "Colobus Monkey"]
ll[Species == "Gol-Jackals", Species := "Gol-Jackal"]
ll[Species == "Grants G", Species := "Grants Gazelle"]
ll[Species == "M'sai Giraffe", Species := "Masai Giraffe"]
ll[Species == "Mtn Reedbuck", Species := "Mountain Reedbuck"]
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

setnames(ll, c("CensusDate", "Species", "Count"))

ll[, CensusDate := tolower(CensusDate) %>% str_to_title]
ll[, CensusDate := {
  t1 = str_extract(CensusDate, "\\D+")
  t2 = substr(t1, 1, 3)
  t3 = str_extract(CensusDate, "\\d{4}")
  t4 = paste(t2,t3)
  t5 = as.yearmon(t4)
  t6 = as.Date(t5)
}]

ll[, Count := as.numeric(Count)]

df = ll[!is.na(Species)]


# plot ==============

myspecies = c("Zebra", "Thompson Gazelle", "Impala",
              "Waterbuck", "Buffalo", "Rothschild Giraffe",
              "Bush buck", "Warthog", "Grants Gazelle")

df[Species %in% myspecies, ] %>%
  ggplot(aes(x=CensusDate, y=Count)) +
  geom_point(shape=0, alpha=0.5) +
  geom_smooth() +
  facet_wrap(Species~., scales="free", nrow=2)


# register in R package
census_data = df
usethis::use_data(census_data, overwrite=overwrite) # see file CensusData.R for documentation

# species in 2018 census

x = census_data[, .(Species = unique(Species)), year(CensusDate)][year == 2018,]
notmammal = x[str_detect(Species, "Bird") | str_detect(Species, "Fowl") |
                str_detect(Species, "Hornbill") | str_detect(Species, "Python"),]
mammal = nrow(x) - nrow(notmammal)
