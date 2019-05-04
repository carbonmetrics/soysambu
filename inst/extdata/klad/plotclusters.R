#' Plot clusters
#'
#' Plots estimated clusters based on Stienen sets (nearest distance) and Steiner sets (dilated sets).
#' @param ppp spatstat ppp object
#' @param kth nearest neighbour
#' @param quantile probability for quantile of nearest distance.
#' @return owin dilation object
#' @export
#' @examples
#' x=plotclusters(snares, 2, 0.9)
#' im=spatstat::as.im(x)
#' spatstat::Window(im)=spatstat::Window(snares)
#' r=raster::raster(im, crs="+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#' clumps=raster::clump(r)
#' pol = raster::rasterToPolygons(clumps, dissolve=T)
#' centers=rgeos::gCentroid(pol, byid=T)
#' centers=data.frame(centers)
#' cl.centers=spatstat::ppp(x=centers$x, y=centers$y, window=Window(snares))


plotclusters=function(ppp,k,quantile, add=F, lwd=1){
  require(spatstat)
  r=nndist(ppp, k=k) %>% quantile(quantile)
  # cat("r=", r)
  hh=subset(ppp, nndist(ppp, k=k)<r)
  stienen(hh, add=add, lwd=lwd)
  plot(dilation(hh, r), add=T)
  # plotstuff()

  return(dilation(hh,r))
}
