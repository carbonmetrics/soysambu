pacman::p_load(geoviz,rayshader,Soysambu, raster,viridis, sf)
aster=raster("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/aster.tif")

sunangle = 270
zscale = 25

# take snare from park core (Euphorbia)
# x=identify(snares)  # 111
y=snares[111]
z=ppp_dt(y)[, .(x,y)]
bb=st_as_sf(z,coords=c("x","y"))
bb=st_set_crs(bb,32736)
dd=st_transform(bb,crs=4326)
coord=st_coordinates(dd)

# get DEM file
DEM=mapzen_dem(lat=coord[2],lon=coord[1],square_km=25)

# get overlay
stamen_overlay = slippy_overlay(DEM, image_source = "stamen", image_type = "watercolor", png_opacity = 0.3)

# elevation overlay
elevation_overlay = elevation_shade(DEM, elevation_palette = c("#000000", "#FFFFFF"), png_opacity = 0.6)

# Calculate the 'rayshader' scene (see 'rayshader' documentation)

elmat = matrix(
  raster::extract(DEM, raster::extent(DEM), method = 'bilinear'),
  nrow = ncol(DEM),
  ncol = nrow(DEM)
)

scene = elmat %>%
  sphere_shade(sunangle = sunangle, texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_overlay(elevation_overlay) %>%
  add_overlay(stamen_overlay)


# Render the 'rayshader' scene

rayshader::plot_3d(
  scene,
  elmat,
  zscale = zscale,
  solid = FALSE,
  shadow = TRUE,
  shadowdepth = -100
)

# in raster

# utm36S = "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# alt=projectRaster(DEM,crs=utm36S)
alt=aster
slope=terrain(alt, opt="slope")
aspect=terrain(alt, opt="aspect")
hill=hillShade(slope,aspect,40,270)
plot(hill,col=grey(0:100/100),legend=F,main="Soysambu")
plot(alt,col=terrain.colors(25,alpha=0.35),add=T)
plotstuff()
