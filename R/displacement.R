#' Calculate Displacement of Spatial Locations
#'
#' This function calculates the distance between two consecutive stay events using their geolocations.
#'
#' @param df a data frame object.
#' @param coor longitude and latitude of the spatial points in the format of c("lon","lat").
#' @param groupvar grouping object to stratify time objects. Recommend to be ID for each individual.
#' If \code{groupvar} is not specified, the entire data frame will be considered as one group to
#' calculate displacement.
#'
#' @return a list of distance values. Unit is meters.
#'
#' @seealso \code{\link{slide}}
#'
#' @examples
#' data(mobility)
#' mobility_stay<- stayevent(mobility, coor = c("lon","lat"), time = "datetime", dist.threshold = 100,
#' time.threshold = 30, time.units = "mins", groupvar = "id")
#' mobility_stay<- aggregate(cbind(id, stayeventlon, stayeventlat)~stayeventgroup, mobility_stay, mean)
#' mobility_stay$dispm<- displacement(mobility_stay, coor = c("stayeventlon","stayeventlat"), groupvar = "id")
#'
#' @importFrom geosphere distVincentyEllipsoid
#'
#' @export

displacement<- function(df, coor = NULL, groupvar = NULL){
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }
  if (is.null(coor)) {
    stop("Geographic coordinates must be supplied.")
  }

  if (is.null(groupvar)) {
    df2<- df[,coor]
    suppressMessages(
      df2 <- slide(df2, Var = coor[1], slideBy = 1)
    )
    suppressMessages(
      df2 <- slide(df2, Var = coor[2], slideBy = 1)
    )
    message("groupvar is not specified, treating all the data as one group.")
  }

  if (!is.null(groupvar)) {
    df2<- cbind(df[,coor], df[groupvar])
    suppressMessages(
      df2 <- slide(df2, Var = coor[1], GroupVar = groupvar, slideBy = 1)
    )
    suppressMessages(
      df2 <- slide(df2, Var = coor[2], GroupVar = groupvar, slideBy = 1)
    )
  }

  coor2<- paste0(coor, "1")
  latlon<- df2[,coor]
  latlonlag2<- df2[,coor2]
  dist<- distVincentyEllipsoid(latlon, latlonlag2, a=6378137, b=6356752.3142, f=1/298.257223563)

  return(dist)
}
