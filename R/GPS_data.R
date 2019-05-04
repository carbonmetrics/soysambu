#' GPS waypoints and GPS tracks
#'
#' Waypoint were collected during desnaring transects in Soysambu in the period from 2018-11-20 to 2019-01-31.
#'
#' @name: gps_points
#' @format sf object in latlon WGS84 projection with 335 records and 13 fields.
#' @source Fieldwork; processed in ./data-raw/GPS_read_clean.R.
#' \describe{
#'     \item{ele}{Elevation above sea level in meters.}
#'     \item{time}{Date and time of registration of waypoint.}
#'     \item{cmt}{Original comment with waypoint.}
#'     \item{name}{Serial number or name of waypoint.}
#'     \item{label}{GPS unit of origin: yellow (Garmin etrex10) or red (Garmin etrex20).}
#'     \item{lon}{longitude.}
#'     \item{lat}{latitude.}
#'     \item{cat}{Category of waypoint: (snare, waypoint, ground snare, illegal firewood, illegal transport,  guinea fowl trap). }
#'     \item{live.dead}{Whether the snare was live or dead.}
#'     \item{nr}{Number}
#'     \item{idx}{Unique serial number.}
#'     \item{time diff}{Time difference between 2 waypoints per date.}
#'     \item{duration}{Time difference as per duration class.}
#'     \item{run}{Number of transect.}
#' }
#' @keywords GPS, snares, desnaring, transects
"gps_points"


# -------------------------------------------

#' GPS waypoints and GPS tracks
#'
#' Waypoint were collected during desnaring transects in Soysambu in the period from 2018-11-20 to 2019-01-31.
#'
#' @name: gps_tracks
#' @format sf object in latlon WGS84 projection with 335 records and 13 fields.
#' @source Fieldwork; processed in ./data-raw/GPS_read_clean.R.
#' \describe{
#'     \item{ele}{Elevation above sea level in meters.}
#'     \item{time}{Date and time of registration of waypoint.}
#'     \item{cmt}{Original comment with waypoint.}
#'     \item{name}{Serial number or name of waypoint.}
#'     \item{label}{GPS unit of origin: yellow (Garmin etrex10) or red (Garmin etrex20).}
#'     \item{lon}{longitude.}
#'     \item{lat}{latitude.}
#'     \item{cat}{Category of waypoint: (snare, waypoint, ground snare, illegal firewood, illegal transport,  guinea fowl trap). }
#'     \item{live.dead}{Whether the snare was live or dead.}
#'     \item{nr}{Number}
#'     \item{idx}{Unique serial number.}
#'     \item{time diff}{Time difference between 2 waypoints per date.}
#'     \item{duration}{Time difference as per duration class.}
#'     \item{run}{Number of transect.}
#' }
#' @keywords GPS, snares, desnaring, transects
"gps_tracks"

# -------------------------------------------

#' Points of interest (POI)
#'
#' Points of interest were collected using Google Earth. These include: gates, offices, camps.
#'
#' @name: poi
#' @format sf object in latlon WGS84 projection with 13 records and 2 fields.
#' @source Google Earth; processed in ./data-raw/GPS_read_clean.R.
#' \describe{
#'  \item{waypoint}{Description of point of interest}
#'  \item{type}{type of waypoint: gate, lodge, settlement, area}
#'  \item{day}{staffing of rangers during the day. This is not the full staffing, just the staffing at the waypoints listed here.}
#'  \item{night}{staffing of rangers at night. This is not the full staffing, just the staffing at the waypoints listed here.}
#'  \item{geometry}{sfg column with longitude, latitude coordinates, crs=4326}
#' }
#' @keywords POI, points of interest
"poi"

