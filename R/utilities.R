#' Checks programmatically whether there is an internet connection.
#'
#' @param ... Function is called without any arguments.
#' @examples
#' if (has_internet()) {
#'     message("yes, you have internet!")
#' }
#' @return Boolean
#' @export

# check whether there is an internet connection

has_internet <- function(...) {

  !is.null(curl::nslookup("r-project.org", error = FALSE))

  }

# -----------------------------------------------------

#' Convert a data.table to a sf object
#'
#' @param dt data.table
#' @return sf object
#' sf = dt_sf(df)
#' @export

dt_sf = function(dt, coords=c("lon", "lat")) {

  require(pacman)
  pacman::p_load(data.table, sf)

  df = setDF(dt)

  sf.obj = sf::st_as_sf(df, coords=coords)

  return(sf.obj)
}

# -----------------------------------------------------

#' Convert a sf object to a data.table
#'
#' @param sf.obj An sf object
#' @return data.table object
#' @export

sf_dt = function(sf.obj) {

  require(pacman)
  p_load(data.table, sf)

  name = NULL  # avoid package check complaint about non-defined global vars

  if (class(sf.obj)[1] != "sf") {
    stop("Your input is not a sf object.")
  }

  mycoords = sf::st_coordinates(sf.obj)
  colnames(mycoords) = c("lon", "lat")
  sf::st_geometry(sf.obj) = NULL

  setDT(sf.obj)

  if ("name" %in% colnames(sf.obj)) {
    sf.obj[, name := as.character(name)]
  }

  sf.obj = cbind(sf.obj, mycoords)

  return(sf.obj)
}


# ------------------------------------------------------

#' convert a ppp object to a data.table
#'
#' @param p a spatstat ppp object
#' @return dt data.table object
#' @export

ppp_dt=function(p){

  require(pacman)
  pacman::p_load(data.table, spatstat)

  if (class(p) != "ppp") {
    stop("Sorry, this is not a ppp object.")
  } else {
    d = data.frame(p)
    setDT(d)
    return(d)
  }
}


# --------------------------------------------------------------------

#' Pipe graphics
#'
#' Like dplyr, ggvis also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom maditr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
NULL

# ------------------------------------------------------------------

#' Negate \code{\%in\%}
#'
#' Inverses the function %nin% using Hmisc::`%nin%`
#'
#' @importFrom Hmisc %nin%
#' @name %nin%
#' @rdname notin
#' @examples
#' data(mtcars)
#' c(5,6) %in% mtcars$cyl
#' c(5,6) %nin% mtcars$cyl
#' @export
NULL

# ------------------------------------------------------------------

#' foreach \code{\%dopar\%}
#'
#' Export %dopar% operator from foreach package
#'
#' @importFrom foreach %dopar%
#' @name %dopar%
#' @rdname doparallel
#' @export
NULL

# ------------------------------------------------------------------

#' setDT
#'
#' Export setDT from data.table
#'
#' @importFrom data.table setDT
#' @name setDT
#' @rdname setDT
#' @export
NULL

# ----------------------------------------------------------------

#' unpack a list
#'
#' This function is from library PBSmodelling.
#' Atomizes a list into its constituents.
#' @param mylist : a list object with data.tables, objects etc
#' @return items from list
#' @export
#' @examples
#' l = list(a = "a", b = "b", c = "c")
#' unpackList(l)

unpackList = function(mylist) {

  foreach(i = 1:length(mylist)) %do% {
    assign(names(mylist)[i], mylist[[i]], envir = .GlobalEnv)
  }

}

# -----------------------------------------------------------

#' Curry a function
#'
#' Curry a function. Comes from library functional.
#' @param function 1, 2, i
#' @return curried function
#' @export
#' @examples
#' f = function(a, b) {c = a * b; return(c)}
#' f(2,3)
#' g = curry(f, b = 10)
#' f(2)

curry = function (FUN, ...)
{
  .orig = list(...)
  function(...) do.call(FUN, c(.orig, list(...)))
}


# -------------------------------------------------------

#' Convert table to data.table
#'
#' Takes a table and converts it into a data.table.
#' The name of the table is used to name the row headers.
#'
#' If you have a data.table with counts, you can convert it into a table with:
#' \code{with(df, xtabs(N ~ var1 + var2))}
#' @seealso This \href{https://cran.r-project.org/web/packages/DescTools/vignettes/TablesInR.pdf}{DescTools vignette}
#' @param tab a table
#' @param labels.cols should each column be labeled with the table name?
#' @return df data.table object with the rownames header taken from the tab name
#' @examples
#' data(mtcars)
#' mytab = with(mtcars, table(mpg, cyl))
#' table2dt(mytab)
#' @export

table2dt = function(tab, label.cols = F) {
  dt = tab %>% as.data.frame.matrix %>% setDT(keep.rownames = T)
  rn = deparse(substitute(tab))
  setnames(dt, "rn", rn)

  if (label.cols) {

    oldnames = names(dt)
    newnames = paste(oldnames, rn, sep = "_")
    newnames[1] = oldnames[1]
    setnames(dt, newnames)
  }

  return(dt)
}
