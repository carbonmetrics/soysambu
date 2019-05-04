#' Inspect data.table
#'
#' Returns following information:
#' \itemize{
#'  \item variable name in order of appearance
#'  \item class
#'  \item avail.vals: available vals = nrow of data.table - (na.vals + inf.vals + nan.vals)
#'  \item uniq.vals: number of unique values
#'  \item dup.vals: duplicate values = available.vars - uniq.vals. A NA value is not counted as a dup.value.
#'  \item na.vals: number of blank values
#'  \item negs: number of negative values
#'  \item zero.vals: number of zero values
#'  \item inf.vals: number of infinite values
#'  \item nan.vals: number of NaN values
#'  \item na.rate: (na.vals + inf.vals + nan.vals) / avail.vals
#'  \item zero.rate: zero.vals / avail.vals
#'  \item dup.rate: dup.vals / avail.vals
#' }
#'
#' @param mydf data.table
#' @return overviewtable
#' @export


view = function(mydf) {

  # convert to data.table if a data.frame is supplied
  if (!is.data.table(mydf)) {
    warning("Sorry, I want a data.table, and converted your data.frame.")
    setDT(mydf, keep.rownames = T)
  }


  # get values

  f = . %>% t %>% as.data.table(keep.rownames = T) %>% setnames("rn", "variable")

  # overview of variable classes
  classes = lapply(mydf, class) %>%
    lapply('[[', 1) %>%
    as.data.table

  classes = classes %>% t %>% as.data.table
  setnames(classes, "classes")

  # overview of variable characteristics
  na.vals = mydf[, lapply(.SD, function(x) sum(is.na(x)))] %>% f
  uniq.vals = mydf[, lapply(.SD, uniqueN)] %>% f
  negs = mydf[, lapply(.SD, function(x) sum(x < 0))] %>% f
  zero.vals = mydf[, lapply(.SD, function(x) sum(x == 0))] %>% f
  inf.vals = mydf[, lapply(.SD, function(x) sum(is.infinite(x)))] %>% f
  nan.vals = mydf[, lapply(.SD, function(x) sum(is.nan(x)))] %>% f


  # clean up

  x = cbind(classes, na.vals[, .(variable)], na.vals[, .(V1)], uniq.vals[, .(V1)], negs[, .(V1)], zero.vals[, .(V1)], inf.vals[, .(V1)], nan.vals[, .(V1)] )
  setnames(x, cc(class, variable, na.vals, uniq.vals, negs, zero.vals, inf.vals, nan.vals))


  # calculate remaining parameters

  x[, avail.vals := nrow(mydf) - (na.vals + inf.vals + nan.vals)]

  x[, `:=` (
    dup.vals = avail.vals - uniq.vals + abs(na.vals > 0),  # a NA value counts as a unique value, which you don't want here
    negs = ifelse(is.na(negs), 0, negs)
  )]

  x[, `:=` (
    na.rate = {
      n1 = (na.vals + inf.vals + nan.vals) / nrow(mydf)
      n2 = round(n1, 3)
      n3 = ifelse(is.na(n2), 0, n2)
    },

    zero.rate = {
      z1 = zero.vals / avail.vals
      z2 = round(z1, 3)
      z3 = ifelse(is.na(z2), 0, z2)
    },

    dup.rate = {
      d1 = dup.vals / avail.vals
      d2 = round(d1, 3)
      d3 = ifelse(is.na(d2), 0, d2)
    }
  )]


  # final cleanup

  x[, `:=` (
    negs = ifelse(class == "character" | class == "factor", 0, negs),
    zero.vals = ifelse(is.na(zero.vals), 0, zero.vals),
    dup.rate = ifelse(avail.vals == 0, 0, dup.rate),
    dup.vals = ifelse(dup.vals == -1 & avail.vals == 0, 0, dup.vals)
  )]

  y = x[, .(variable, class,
            avail.vals, uniq.vals, dup.vals,
            na.vals, negs, zero.vals, inf.vals, nan.vals,
            na.rate, zero.rate, dup.rate
  )]

  setorder(y, variable)

  return(y)

}



