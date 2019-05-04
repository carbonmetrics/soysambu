#' Contrast calculate and visualise measures of inequality and concentration
#'
#' @param x a numerical vector
#' @param varname variable name for plot headers
#' @param layout either "h" (horizontal) or "v" (vertical) plots; default = "v"
#' @return Lorentz curve plot and inequality/concentration measures
#' @examples
#' data(mtcars)
#' contrast(mtcars$hp, "horsepower")
#' @export
#'
#' @section Details:
#' The \href{https://en.wikipedia.org/wiki/Lorenz_curve}{Lorentz curve} shows the cumulative percentage of a variable against the cumulative percentage of the population.
#' E.g. 20 percent of the population has 10 percent of the total income.
#' A larger 'sag' in the line means larger inequality.
#'
#' The \href{https://en.wikipedia.org/wiki/Gini_coefficient}{Gini index} summarises the inequality that is visualised in the Lorentz curve.
#' The index is calculated by dividing the area between the dotted line (perfect equality) and the Lorentz line by the total area under the dotted line.
#'
#' The \href{https://en.wikipedia.org/wiki/Income_inequality_metrics}{Hoover index} is defined as the maximum vertical distance between the dotted line and the Lorentz curve.
#' It is shown as a dotted line in the Lorentz curve (percentages).
#' The Hoover index is the proportion of all (income) which would have to be redistributed to achieve a state of perfect equality.
#'
#' The cumulative distribution plots the value of the variable against the cumulative proportion.
#' For example, when you read 80% (y-axis) and the corresponding x-value = 10, then:
#' 20 percent of the values are bigger than 10 and
#' 80 percent of the values are smaller than 10. The individual values are plotted as vertical dashes on the x-axis ("rug").


contrast = function(x, varname, layout = "v", myunits = "none") {

  library(ggplot2)
  library(ineq)
  library(data.table)
  library(gridExtra)

  if (class(x) == "factor") {
    warning("converting factor variable to numeric.")
    x = as.numeric(x)
  }

  if (class(x) == "character") {
    x = factor(x) %>% as.numeric
    warning("converting character variable to numeric via factor.")
  }

  if (class(x) == "logical") {
    stop("sorry, can't do anything with class logical.")
  }

  if (class(x) == "POSIX.ct") {
    stop("sorry, can't do anything with dates.")
  }


  # set up for plotting =======

  theme_set(theme_bw())

  theme_update(plot.title = element_text(size = 12),
               plot.subtitle = element_text(size = 10, color = "grey40"),
               plot.caption = element_text(size = 8, hjust = 0, color = "grey40"))

  update_geom_defaults("step", list(colour = "steelblue"))
  update_geom_defaults("line", list(colour = "steelblue"))

  layout = switch(layout,
                  "h" = 2,
                  "v" = 1) %>% as.integer


  # Lorentz curve ============

  lorentz = Lc(x)

  dfl = data.table(
    p = lorentz$p,
    L = lorentz$L,
    L.general = lorentz$L.general)


  # hoover index -------------
  dfl[, delta := p - L]
  hoover.index = dfl[, max(p-L)]
  position = dfl[delta == hoover.index, p]
  hoover.idx = round(hoover.index, 3)


  # gini index ----------------
  gini.idx = Gini(x, na.rm = T) %>% round(3)


  # plot lorentz ------------------
  l = ggplot(dfl) +
    geom_line(aes(p, L)) +
    geom_abline(slope = 1, linetype = "longdash", col = "grey70") +
    geom_vline(xintercept = position, linetype = "dotted", col = "grey50") +
    scale_x_continuous(label = scales::percent) +
    scale_y_continuous(label = scales::percent) +
    labs(x = "cumulative percentage of population",
         y = "cumulative percentage of variable",
         title = paste("Lorentz curve,", varname),
         subtitle = paste0("Visualises inequality: the more depressed the line is, the larger the inequality.
Hoover index (vertical dotted line): ", hoover.idx, ", Gini index: ", gini.idx, ", observations: ",  length(x[!is.na(x)]), "."))


  # plot cumulative --------------
  raw = data.table(x)[order(x),]
  raw[, xperc := x/max(x)]
  raw[, idx := .I]

  mean.val = round(mean(x, na.rm = T), 2)
  median.val = round(median(x, na.rm = T), 2)
  subt=paste0("mean: ", mean.val,
             ", median: ", median.val,
             ", units: ", myunits,
             ", observations: ", length(x[!is.na(x)]),
             ".")

  xlab=quantile(x, probs=seq(0,1,0.25)) %>% round

  cumul.perc = ggplot(raw) +
    stat_ecdf(aes(x=xperc)) +
    geom_rug(aes(xperc), col = "grey70") +
    geom_abline(slope = 1, linetype = "longdash", col = "grey70") +
    scale_x_continuous(label = scales::percent,
                       sec.axis=dup_axis(labels=xlab, name=paste(myunits))) +
    scale_y_continuous(label = scales::percent) +
    labs(x = paste(myunits, "%"), y = "cumulative percentage",
         title = paste("Cumulative distribution,", varname),
         subtitle = subt)

  # combine plots ----------------
  plots = arrangeGrob(l, cumul.perc, ncol = layout)
  plot(plots)

}
