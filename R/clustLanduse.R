#' Unsupervised clustering of land use using a Sentinel image
#'
#' Clusters a Sentinel image in a given number of clusters.
#' Code is from \url{https://www.gis-blog.com/unsupervised-kmeans-classification-of-satellite-imagery-using-r/}
#'
#' @param sentinel Sentinel image (raster)
#' @param cl Required number of clusters
#' @param ... Other arguments
#' @return Clustered raster image
#' @examples
#' sent = system.file("extdata/spatial", "sentinel.tif", package="Soysambu")
#' myrasterfile = raster::raster(sent)
#' r = clustLanduse(myrasterfile, cl=3)
#' @export

clustLanduse = function(sentinel, cl, ...) {

  # preset colors
  col.cl = viridis::viridis(cl)

  # clustering operation
  set.seed(1964)
  km = stats::kmeans(sentinel[], centers = cl, nstart=5, iter.max=10)
  res = raster::raster(sentinel[[1]])
  res = raster::setValues(res, km$cluster)

  raster::plot(res,col = col.cl)
  return(res)
}
