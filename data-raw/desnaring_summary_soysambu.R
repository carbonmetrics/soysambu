#' Analysis of snaring report spreadsheet
#' The xls does not split out snares found per area, but it contains names and dates.
#' It can therefore be used at least for an estimation of patrol effort for selected hotspots.

# set up

librarian::shelf(readxl, data.table, stringr, maditr, zoo, lubridate)

x = read_excel("./inst/extdata/other_data/SnaringReport.xlsx", skip=1) %>% setDT
setnames(x, names(x) %>% tolower %>% make.names)
y = x[, .(date, location)]

# clean up

y[, date := zoo::na.locf(date)]
y[, location := {
  t1 = tolower(location)
  t2 = stringr::str_replace(t1, "\\.", "")
  t3 = stringr::str_replace(t2, "area", "")
  t4 = stringr::str_squish(t3)
  }]
y = y[!is.na(location) & location != "total" & location != "totals",]



# clean up
y[str_detect(date, "2108$"), date := str_replace(date, "2108", "2018")]
y[str_detect(date, "2107$"), date := str_replace(date, "2107", "2017")]

y[, date1 := lubridate::dmy(date)]

y[is.na(date1), date1 := {
t1 = as.numeric(date)
t2 = as.Date(t1, origin = "1899-12-30")
}]

z = y[, location, date1]
setnames(z, "date1", "date")

# stats

z[, .N, location]

payoff = data.table(
  . = c("run", "dare"),
   run = c("0,0", "1,0"),
   dare = c("0,1", "-10,-10")
    )

kable(payoff)

