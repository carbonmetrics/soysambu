#' Convert table to data.table
#'
#' Takes a table and converts it into a data.table.
#' The name of the table is used to name the row headers.
#'
#' If you have a data.table with counts, you can convert it into a table with:
#' \code{with(df, xtabs(N ~ var1 + var2))}
#'
#'
#' @seealso This \href{https://cran.r-project.org/web/packages/DescTools/vignettes/TablesInR.pdf}{DescTools vignette}
#' @param tab a table
#' @param label.cols should each column be labeled with the table name?
#' @return df data.table object with the rownames header taken from the tab name
#' @examples
#' library(data.table)
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
