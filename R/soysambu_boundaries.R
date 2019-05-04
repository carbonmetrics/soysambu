#' Soysambu boundaries
#'
#' shp file converted to sf; contains boundaries of Soysambu Conservancy.
#'
#' @name: soysambu_boundaries
#' @source: Soysambu Conservancy, kcombes@soysambuconservancy.org
#' \describe{
#'     \item{soysambu_boundaries}{sf object}
#' }
#' @keywords Soysambu, boundaries
"soysambu_boundaries"


#' Soysambu sample grid
#'
#' Grid of 3x3 km that overlay the conservancy boundaries
#'
#' @name: soysambu_grid
#' @source: sf library
#' \describe{
#'     \item{soysambu_grid}{sf object}
#' }
#' @keywords Soysambu, boundaries
"soysambu_grid"


#' Soysambu sample grid breaks
#'
#' Grid of 3x3 km that overlay the conservancy boundaries: xbreaks
#' You will need this vector in order to match the sf sampling grid with the spatstat quadrat count.
#'
#' @name: xbreaks
#' @source: ./data-raw/get_spatial_files.R
#' \describe{
#'     \item{xbreaks}{vector}
#' }
#' @keywords Soysambu, boundaries, breaks
"xbreaks"


#' Soysambu sample grid breaks
#'
#' Grid of 3x3 km that overlay the conservancy boundaries: xbreaks
#' You will need this vector in order to match the sf sampling grid with the spatstat quadrat count.
#'
#' @name: ybreaks
#' @source: ./data-raw/get_spatial_files.R
#' \describe{
#'     \item{ybreaks}{vector}
#' }
#' @keywords Soysambu, boundaries, breaks
"ybreaks"
