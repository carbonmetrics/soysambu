#' Interview data


library(data.table)
library(maditr)
library(readxl)

path=here::here("inst", "extdata","interviews", "interviews.xlsx")

overwrite=T

# get closed questions, remove names and save ---------------

closed = readxl::read_excel(path, sheet="ClosedQuestions")
setDT(closed)

# years experience rangers
yrs = closed[, stringr::str_extract(Who, "\\d+")]
yrs = yrs[!is.na(yrs)] %>% as.integer
years_experience_rangers = yrs
usethis::use_data(years_experience_rangers, overwrite = overwrite)

# closed questions
closed_questions = closed[, .(ID, Question, QuestionExplain, Score, Explain_Notes)]
closed_questions[Question==1, ranger.id := 1:.N]
closed_questions[, ranger.id := zoo::na.locf(ranger.id)]
closed_questions[, Score := as.integer(Score)]

usethis::use_data(closed_questions, overwrite = overwrite)


# frequencies and violence --------------------------------

# read
freq=readxl::read_excel(path, sheet="Freq_seeing_poachers", na=c("NA"))
setDT(freq)

# clean
setnames(freq, names(freq) %>% make.names)
names(freq) %>% make.names

# condense
freq_poachers=freq[Question==23, .(Freq, No.report, Violence, Remarks )]
freq_poachers[No.report == "No support", No.report := NA]
freq_poachers[, ranger.id := .I]

# assign frequencies: e.g. M2 = twice per month = 2/30.4
freq.table=data.table(
  Freq=c("W2","W","M2","M","Q","Y", "N", NA),
  Freqf = c("Twice per week","Weekly","Twice per month","Monthly", "Quarterly","Yearly","Never", NA),
  Freqd=c(2/7, 1/7, 2/30.4, 1/30.4, 1/(3*30.4), 1/365, 0, NA )
)

freq_poachers=freq_poachers[freq.table, on="Freq"]
freq_poachers[, Freqf := factor(Freqf, levels=c("Twice per week","Weekly","Twice per month","Monthly", "Quarterly","Yearly","Never", NA))]

# sort on ranger ID
setorder(freq_poachers, ranger.id)
usethis::use_data(freq_poachers, overwrite=overwrite)


# get open questions, remove names and save --------------

open = readxl::read_excel(path, sheet = "OpenQuestions")
setDT(open)

open_questions = open[, .(Nr, Question, Answer)]
open_questions[Nr==1, ranger.id := 1:.N]
open_questions[, ranger.id := zoo::na.locf(ranger.id)]

usethis::use_data(open_questions, overwrite = overwrite)


# get main clue answers, remove names and save -----------

clues = readxl::read_excel(path, sheet = "Clues")
setDT(clues)

clue_questions = clues[Nr==1, Answer]
clue_questions = clue_questions[!is.na(clue_questions)]
usethis::use_data(clue_questions, overwrite = overwrite)


# get ranger estimations for number of snares, clean up ------

# read
x = readxl::read_excel(path, sheet = "Estimates")
setDT(x)
x = x[1:19,]

# separate data.table for snares found
snares.found = x[18,] %>% t %>% as.data.table(keep.rownames=T)
setnames(snares.found, c("area", "snares_found"))
snares.found[, snares_found := as.numeric(snares_found)]

# separate data.table for dates of transects
dates = x[19] %>% t %>% as.data.table(keep.rownames=T)
dates[, V1 := {
  t1 = as.integer(V1)
  t2 = as.Date(t1, origin="1899-12-30")
  }]
setnames(dates, c("area", "transect_date"))
dates[area=="Euphorbia ridge E", transect_date := as.Date("2019-01-24")]
dates=dates[-1,]


# process snares estimated ------------------------------------
x = x[-19,]  # remove data field
y = t(x) %>% as.data.table(keep.rownames=T)
y = y[rn != "Euphorbia ridge E",]  # no estimations made
y = y[, V18:= NULL]  # remove totals

z = melt(y, id.vars="rn")[, .(rn, value)]
setnames(z, c("area", "snares_estimated"))
z[, snares_estimated := as.numeric(snares_estimated)]
z = z[!is.na(snares_estimated)]

# join with snares found
z = snares.found[z, on="area"]

# join with transect dates
z = dates[z, on="area"]

# calculate group estimate average, n obs, min, max
z[, `:=` (
  n = .N,
  min = min(snares_estimated),
  max = max(snares_estimated),
  group_avg = mean(snares_estimated)
), .(area, transect_date)]

z[, in_range := abs(snares_found >= min & snares_found <= max)]

# note that Euphorbia Ridge was desnared twice, but no estimates were made.
# therefore the number of unique areas in y is different from that of z, because all NAs were removed in z (and all Euphorbia records are NA).

zz = z[, .(
  n = mean(n),
  in_range = mean(in_range),
  min = mean(min),
  max = mean(max),
  group_avg = mean(group_avg),
  snares_found = mean(snares_found),
  PD = sum((snares_estimated - group_avg)^2) / n,
  IE = sum((snares_estimated - snares_found)^2) / n
), area]

zz = unique(zz, by = "area")
zz[, CE := IE - PD]
mycols = c("PD", "IE")
zz[, c("PDsq", "IEsq") := lapply(.SD, sqrt), .SDcol = mycols]
zz[, CEsq := IEsq - PDsq]
zz[, r := CE/IE]

setkey(zz, area)
zz[area == "Check Height__1", area := "Check Height, repeat"]

snare_estimates_raw = z
usethis::use_data(snare_estimates_raw, overwrite=overwrite)

snare_estimates = zz
usethis::use_data(snare_estimates, overwrite=overwrite)


# observations of poachers ========

sightings=read_excel(path=path,sheet="poacher_sightings") %>% setDT

setcolorder(sightings,c("Frequency","n","f","EV"))
sightings[, `:=` (
  f=f %>% as.numeric %>% round(3),
  EV=round(EV,3)
)]

library(knitr)
kable(sightings,format="latex",booktabs=T)


# no report and violence raw:

noreport=read_excel(path=path,sheet="sightings_noreport_violence") %>% setDT
noreport=noreport[, .(`No report`,Violence)]
setnames(noreport,c("no_report","violence"))
noreport[, .N,no_report][]
noreport[, .N,violence]
