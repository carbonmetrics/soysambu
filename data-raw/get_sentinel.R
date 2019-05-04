#' Sentinel files were acquired from the Copernicus Open Access Hub
#' https://scihub.copernicus.eu/dhus/#/home
#' The image was processed using SNAP.

# set up ----------------------------

pacman::p_load(data.table, raster, maditr, foreach, Soysambu, viridis)

overwrite=T

# get files -------------------------

myfiles=c("/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_ndwi_ndwi.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_ndvi.data/ndvi.img",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_savi.data/savi.img",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_RGB_IR.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_RGB_nat.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_B3.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_B4.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_B8.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_B2.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_ndi45_ndi45.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_dvi_dvi.tif",
          "/home/henk/Documents/PhD/RefFiles/S2A_MSIL1C_20190310T073731_N0207_R092_T36MZE_20190310T094046_arvi_arvi.tif"
          )

# read original raster file ...
s=raster("./inst/extdata/spatial/sentinel.tif")

# ... and crop

# the original Sentinel raster is a little too big: there are NA values in the upper left corner
e=new("Extent", xmin = 846064.962729103, xmax = 867930.984598011,
      ymin = 9938150.38059285, ymax = 9963000.47132858)

# crop
l=foreach(i=1:length(myfiles)) %do% {
  x=raster(myfiles[i])
  y=crop(x,e)
}

# store originals in a brick file -------------------------
names(l)=c("NDWI","NDVI","SAVI","infrared","nat", "B3","B4","B8","B2", "NDI45","DVI","ARVI")

# put into a brick file
b=brick(l$NDWI, l$NDVI, l$SAVI, l$NDI45, l$DVI, l$ARVI, l$infrared, l$nat, l$B2, l$B3, l$B4, l$B8)
names(b)=c("NDWI","NDVI","SAVI", "NDI45", "DVI", "ARVI","infrared","natural", "B2", "B3", "B4", "B8")

# crop to boundaries of conservancy ---------------

conservancy = fasterize::fasterize(sf=soysambu_boundaries, raster=b[[1]])

ll=foreach(i=1:length(names(b))) %do% {
  z=raster::subset(b, names(b)[i])
  zx=mask(z,conservancy)
}

names(ll)=names(b)
bb=brick(ll$NDWI,ll$NDVI,ll$SAVI, ll$NDI45, ll$DVI, ll$ARVI, ll$infrared,ll$natural, ll$B2, ll$B3, ll$B4, ll$B8)
names(bb)=names(b)

# cluster landuse on each of the brick files -----------------
# need the full rectangular once, not the ones cropped to Soysambu boundaries in view of NA values
# crop in the loop itself

cl=5

lll=foreach(i=1:length(names(b))) %do% {
  z=raster::subset(b, names(b)[i])
  zx=clustLanduse(z, cl=cl)
  title(main=names(b)[i])
  plotstuff(plot.snares=T,col="white",cex=0.8, plot.allroads=T)
  zy=mask(zx,conservancy)
}

names(lll)=names(b)

bbb=brick(lll$NDWI,ll$NDVI,lll$SAVI,lll$NDI45, lll$DVI, lll$ARVI, lll$infrared,lll$natural, lll$B2, lll$B3, lll$B4, lll$B8)
names(bbb)=names(b)

plot(bbb,col=viridis(cl))

# write
sentinel10=b
sentinel10_cons=bb
sentinel10_clust=bbb

# write using native grd format, otherwise names of layers are lost and things mess up
writeRaster(sentinel10, file="./inst/extdata/spatial/sentinel10.grd", overwrite=overwrite)
writeRaster(sentinel10_cons, file="./inst/extdata/spatial/sentinel10_cons.grd", overwrite=overwrite)
writeRaster(sentinel10_clust, file="./inst/extdata/spatial/sentinel10_clust.grd", overwrite=overwrite)

