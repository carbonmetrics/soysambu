#' Get waypoints from Garmin GPS unit.
#'
#' Finds all files in a folder for which the name starts with "Waypoints".
#' Reads each file using tmaptools::read_GPS and converts dates to date class.
#' Given slow execution time, all cores are used using the doParallel package.
#'
#' @param ... Function is called without any arguments.
#' @return data.frame with the fields "ele", "time", "cmt", "name" and "geometry".
#' @export


getPoints = function(...) {

  cl=i=NULL  # to avoid package check complaints about non-defined global vars

  file_list = list.files(pattern = "^Waypoints")
  cat("Found", length(file_list), "waypoint files, processing... \n")

  cl = parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)

  l = foreach::foreach(i = 1:length(file_list),
              .combine=rbind,
              .packages=c("tmaptools")) %dopar% {

                # read
                x = tmaptools::read_GPX(file_list[i])
                y = subset(x, select = c("ele", "time","cmt", "name", "geometry"))

              }

  return(l)
  parallel::stopCluster(cl=cl)
}
