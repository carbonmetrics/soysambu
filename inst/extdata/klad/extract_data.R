
# extract values ===================================================

# set up sampling mechanism ----------------------------------------

# get the points
px=ppp_dt(snares)


# set up points for extraction from rasters
py = sp::SpatialPoints(px[, 1:2], proj4string=crs(savi))

extracting=function(x, method="simple", buffer=NULL, fun=NULL) {
  r=subset(brickfile,x)
  e=raster::extract(r, py, method=method, buffer=buffer, fun=fun)
  return(e)
}


# get the values and put into one data.table -----------------------

# distance to roads and boundaries ---------------------------------
px[, dist_roads := nncross(snares_lake, soysambu_roads_lake_psp)$dist]
px[, dist_bounds := bdist.points(snares_lake)]


# extract from brickfile -------------------------------------------
px[, `:=` (
  ASTER=extracting("ASTER"),
  slope=extracting("slope"),
  TRI=extracting("TRI"),
  open_areas=extracting("open_areas", buffer=100, fun=mean),
  SAVI=extracting("SAVI", method="bilinear"),
  hotspot_buffered={
    t1=extracting("hotspot_buffered")
    t2=ifelse(is.na(t1),0,t1)
  }
)]


# how to take a sample from a raster and calculate distance to cluster centroids ------
s=sampleRandom(size=10, sentinel, sp=T, na.rm=T) %>% st_as_sf
nn=st_nn(s, polsf[-1]) %>% unlist # gives the hotspot nr that is closest
cluster_centroids=cluster_centroids[nn,]
dist=st_distance(s, cluster_centroids, by_element=T)
