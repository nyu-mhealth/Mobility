#' Calculate Standard Deviation of Spatial Points
#'
#' This function measures the distribution of spatial points by calculating standard deviation of
#' the distance between points and their centroid. Centroid is the mean longitude and latitute of
#' all the points provided. Distance is calculated based on Vincenty's great circle distance formula
#' with default unit as meters.
#'
#' @param df a data frame object.
#' @param coor longitude and latitude of the spatial points in the format of c("lon","lat").
#'
#' @return a number of standard deviation.
#'
#' @examples
#' lon<- c(-70.1, -70.5, -70.3)
#' lat<- c(40.74, 40.75, 40.76)
#' lonlat<- data.frame(lon, lat)
#' sdspatialpoints(lonlat, coor = c("lon","lat"))
#'
#' @importFrom geosphere distVincentyEllipsoid
#'
#' @export


sdspatialpoints<- function(df, coor = NULL, ...){
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }
  if (is.null(coor)) {
    stop("Geographic coordinates must be supplied.")
  }

  if (!is.null(coor)) {
    # meancoor<- c(mean(df[,coor[1]]),mean(df[,coor[2]]))
    meancoor <- c(mean(df[[coor[1]]]), mean(df[[coor[2]]]))
    latlon<- cbind(df[,coor])
    latlon$dist<- distVincentyEllipsoid(latlon, meancoor, a=6378137, b=6356752.3142, f=1/298.257223563)
    latlon$dist2<- latlon$dist^2
    N<- nrow(latlon)
    distsum<- sum(latlon$dist2)
    sd<- sqrt(distsum/N)
    return(sd)
    }
}
