#' Plotstuff
#'
#' Overlay for plots of Soysambu: roads, lake, snares, gates.
#' @param plot.snares boolean: whether or not to plot the existing neck and foot snares as defined in object "snares".
#' @param plot.lake boolean: whether or not to plot Lake Elmenteita?
#' @param plot.legend boolean: do you want to include a legend in the plot?
#' @param plot.allroads boolean do you want to plot all roads around the lake, or just the roads around the conservancy?
#' @param cex relative symbol size (default=1)
#' @param col color of symbols (default="red")
#' @param main title of plot
#' @param add whether plot is superimposed on existing plot (default=TRUE) or whether a standalone plot must be made.
#' @return plot
#' @export


plotstuff = function(plot.snares=F, plot.lake=T, plot.legend=T, plot.allroads=F, cex=1, lty=4, lwd=1, col="red", main="", add=T, ...){

  # setup -------------------------------------------------------

  library(spatstat)
  library(sf)
  library(stringr)
  library(data.table)

  # set up from points of interest poi ----------------------

  pi = poi[poi$type != "lake" &
             poi$type != "settlement" &
             poi$type != "town" &
             poi$type != "area" &
             poi$type != "community",]
  baruk.stores=subset(poi,waypoint=="Baruk Stores")
  pi=rbind(pi,baruk.stores)

  pi$type = stringr::str_to_title(pi$type)
  colnames(pi)[2]="Waypoint type"

  pix = pi %>%
    st_transform(crs=32736) %>%
    sf_dt

  gates=pix[stringr::str_detect(waypoint, "Gate"),]
  hotels=pix[waypoint %like% "Camp" | stringr::str_detect(waypoint, "lodge"),]
  # other=pix[waypoint=="Lookout" | waypoint=="Head Office"]
  hq=pix[waypoint=="Head Office",]
  baruk.stores=pix[waypoint=="Baruk Stores"]
  lookout=pix[waypoint=="Lookout"]


  # set up snares from pointpattern -------------------------

  necksnares=spatstat::subset.ppp(snares, cat=="NECK SNARE")
  groundsnares=spatstat::subset.ppp(snares, cat=="GROUND SNARE")


  # plotting --------------------------------------------

  # baseplot
  plot(Window(snares), lty=lty, lwd=lwd, main=main, add=add)

  # plot snares?
  if (plot.snares) {
    plot(unmark(necksnares), pch=3,cex=cex,cols=col, add=T)
    plot(unmark(groundsnares), pch=4,cex=cex,cols=col, add=T)
  }

  # plot all roads?
  if (plot.allroads) {
    plot(soysambu_roads_lake_psp, col=col, style="none", add=T, main="", show.window=F, lwd=lwd)
  } else {
    plot(soysambu_roads_psp, col=col, style="none", add=T, main="", show.window=F, lwd=lwd)
  }

  # plot lake?
  if (plot.lake) {
    plot(Window(pp_lake), add=T, lty=lty,lwd=lwd)
  }

  # plot legend?
  if (plot.legend) {
    legend("bottomleft",
           legend=c("neck snares", "ground snares", "gates", "hotels", "head office", "lookout","settlement"),
           pch = c(3,4,0,1,2,8,7),
           box.lty=0,
           cex=cex)
  }

  # plot poi
  points(gates$lon, gates$lat, pch=0, col="black", cex=cex)
  points(hotels$lon, hotels$lat, pch=1, col="black", cex=cex)
  points(hq$lon, hq$lat, pch=2, col="black", cex=cex)
  points(lookout$lon, lookout$lat, pch=8, col="black", cex=cex)
  points(baruk.stores$lon, baruk.stores$lat, pch=7, col="black", cex=cex)

}

