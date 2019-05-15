pacman::p_load(geoviz,rayshader,Soysambu, raster,viridis, spatstat, sf)

sunangle = 270
zscale = 25

# take snare from park core (Euphorbia)
# x=identify(snares)  # 111
data(snares)
y=snares[111]
z=ppp_dt(y)[, .(x,y)]
bb=st_as_sf(z,coords=c("x","y"))
bb=st_set_crs(bb,32736)
dd=st_transform(bb,crs=4326)
coord=st_coordinates(dd)

# get DEM file
DEM=mapzen_dem(lat=coord[2],lon=coord[1],square_km=25)

# get overlay
stamen_overlay = slippy_overlay(DEM, image_source = "stamen", image_type="toner-background", png_opacity = 0.3)

# elevation overlay
elevation_overlay = elevation_shade(DEM, elevation_palette = c("#000000", "#FFFFFF"), png_opacity = 0.6)

# Calculate the 'rayshader' scene (see 'rayshader' documentation)

elmat = matrix(
  raster::extract(DEM, raster::extent(DEM), method = 'bilinear'),
  nrow = ncol(DEM),
  ncol = nrow(DEM)
)

scene = elmat %>%
  sphere_shade(sunangle = sunangle, texture = "bw") %>%
  add_overlay(elevation_overlay) %>%
  add_overlay(stamen_overlay)


# Render the 'rayshader' scene

rayshader::plot_3d(
  scene,
  elmat,
  zscale = zscale,
  solid = FALSE,
  shadow = TRUE,
  shadowdepth = -100,
  windowsize=1200
)

render_snapshot(filename="./inst/extdata/figures/soysambu_3d.png")

# add_gps_to_rayshader(
#   DEM,
#   igc$lat,
#   igc$long,
#   igc$altitude,
#   line_width = 1.5,
#   lightsaber = TRUE,
#   colour = "red",
#   zscale = zscale,
#   ground_shadow = TRUE
# )
