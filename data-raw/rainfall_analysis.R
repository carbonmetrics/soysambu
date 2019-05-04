libs=c("data.table", "readxl", "zoo", "stringr", "maditr",
                 "lubridate", "forcats", "ggplot2")

lapply(libs, library, character.only=T, quietly=T, verbose=F)

# read data and clean up ----------------------------------------------

r = read_excel("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/other_data/rainfall.xls", skip=1)
setDT(r)

setnames(r, colnames(r) %>% str_replace(., "\\.", "") %>% tolower)
r = r[, !c("total mm", "total inches")]
r = r[yr != "TOTALS" & yr != "AVG."]

# correct years
r[, yr := as.numeric(yr)]
r[yr == 1668, yr := 1968]
r[duplicated(yr), yr := 1973]

# r[, diff.yr := c(0, diff(yr))]

m = melt(r, id.vars="yr")

m[, `:=` (
  year = as.integer(yr),
  month = str_to_title(variable)
)]
m[, c("variable", "yr") := NULL]
setcolorder(m, c("year", "month", "value"))
setnames(m, "value", "rain.mm")

m[, date := paste(15, month, year) %>% dmy]
months = fct_inorder(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
m[, month := factor(month, levels = months)]
# m[, month := month(date)]

# static plot -------------------------------------------------

ggplot(m) +
  geom_boxplot(aes(x = month, y = rain.mm))

p = ggplot(m) +
  geom_line(aes(x = as.integer(month), y = rain.mm, group=year))

# as time series ----------------------------------------------

s = ts(m$rain.mm, start=c(1948,1), end=c(2018,12),freq=12)
s.annual=aggregate(s)/12
d = stl(s, s.window="periodic")

# plot(d)
