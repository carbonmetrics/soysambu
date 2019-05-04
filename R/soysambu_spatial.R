# ========================================================
#' Soysambu spatial: Soysambu roads
#'
#' Road network in and around Soysambu.
#'
#' @name: soysambu_roads
#' @source ./data-raw/get_spatial_files.R; https://datacatalog.worldbank.org/dataset/kenya-roads-0
#' @keywords Soysambu, roads
"soysambu_roads"


# ===============================================

#' Soysambu roads lake
#'
#' As above, but with the boundary of the lake included.
#' This is done via the owin Window of the statstat::pp_lake object, see make_ppp.R.
#'
#' @name: soysambu_roads_lake
#' @source get_spatial_files
"soysambu_roads_lake"


# ============================================

#' Calculated centroids of clusters
#'
#' Clusters were calculated in spatstat using the plotclusters function.
#' The centroids in this dataset are necessary to calculate the distance of a sampled point in a raster
#' to the centroid using e.g. nngeo::st_nn or spatstat::nearest.raster.point.
#'
#' @name: cluster_centroids
#' @source collect_machine_learning.R, plotclusters.R
"cluster_centroids"
