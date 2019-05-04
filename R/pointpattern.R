#' Point pattern points of snares
#'
#' The package "spatstat" requires its own format ("ppp").
#' The number of snares was added as a mark.
#' See all_snares for a ppp with unique snare locations
#'
#' @name: snares
#' @format spatstat ppp object for snares
#' @source desnaring transect points; ./data-raw/make_ppp.R.
#' @keywords snares, desnaring, spatstat, point pattern
"snares"

# ==================================================

#' Point pattern tracks
#'
#' @name: pointpattern_tracks
#' @format spatstat ppp object for gps_tracks
#' @source desnaring transect track points; ./data-raw/make_ppp.R
#' @keywords snares, desnaring, spatstat, point pattern
"pointpattern_tracks"

# ================================================

#' Line segment patterns
#'
#' @name: soysambu_roads_psp
#' @format spatstat::psp
#' @source convert from soysambu_roads sf object, see ./data-raw/get_spatial_files
#' @keywords roads, spatstat
"soysambu_roads_psp"

# ================================================

#' Line segment patterns
#'
#' As above, but with a bigger bounding box that covers the lake.
#' That was required to make a distance map that uses the lake as East boundary.
#'
#' @name: soysambu_roads_lake_psp
#' @format spatstat::psp
#' @source convert from soysambu_roads sf object, see ./data-raw/get_spatial_files, make_ppp.R
#' @keywords roads, spatstat
"soysambu_roads_lake_psp"

# =================================================

#' Pointpattern with lake included
#'
#' @name: pp_lake
#' @format spatstat::ppp
#' @source combined windows of pointpattern of snares, lake elmentaita and a polygon to clean up non-overlaps; see make_ppp.R
#' @keywords lake, spatstat, snares
"pp_lake"

