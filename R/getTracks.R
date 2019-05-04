#' Get tracks from a Garmin GPS unit.
#'
#' Finds all files in a folder for which the name starts with "Track".
#' Reads files using tmaptools::read_GPX and converts date to date class.
#' Given the slow execution time, all cores are used using doParallel.
#'
#' @param ... Function is called without any arguments.
#' @return data.frame with all tracks. The tracks are labeled with the filename.
#' Fields are "track_seg_point_id", "ele" and "time".
#' @export


getTracks = function(...) {

  cl=i=NULL  # to avoid package check complaints about non-defined global vars

  # register parallel backend
  cl = parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)

  track_list = list.files(pattern="^Track")
  cat("Found", length(track_list), "tracks, processing... \n")

  l = foreach::foreach(i = 1:length(track_list),
              .combine=rbind,
              .packages=c("tmaptools")) %dopar% {

                x = tmaptools::read_GPX(track_list[i])
                mytracks = x[[1]][[1]]
                mypoints = x[[2]]
                y = subset(mypoints, select = c("track_seg_point_id", "ele", "time"))
                y$date = track_list[i]
                y

              }

  return(l)
  parallel::stopCluster(cl=cl)
}
