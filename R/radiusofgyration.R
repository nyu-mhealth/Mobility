#' Calculate Radius of Gyration of Spatial Points
#'
#' This function calculates radius of gyration, defined as the distance moved within a certain
#' time period. Distance is calculated using Vincenty's formula.
#'
#' @param df a data frame object.
#' @param coor longitude and latitude of the spatial points in the format of c("lon","lat").
#' @param time a POSIXct time object, used to calculate time period.
#' @param time.units time units used to group spatial location points. Choose from hour, date,
#' and month. Radius of gyration is calculated within each time frame.
#' @param groupvar grouping object to stratify time objects. Recommend to be ID for each individual.
#' If \code{groupvar} is not specified, \code{time.units} will be used to sort data and calculate radius of gyration.
#'
#' @return a list of radius of gyration value matching to each spatial point in data frame. points in
#' the same time period sepecified in time.units have the same radius of gyration.
#'
#' @seealso \code{\link{sdspatialpoints}}
#'
#' @examples
#' data("mobility")
#' mobility$rg<- radiusofgyration(mobility, coor = c("lon","lat"), time = "datetime", time.units = "date", groupvar = "id")
#'
#' @import lubridate
#' @import dplyr
#' @importFrom geosphere distVincentyEllipsoid
#'
#' @export

radiusofgyration<- function(df, coor = NULL, time = NULL, time.units = c("hour", "date", "month"), groupvar = NULL) {
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }
  if (is.null(coor)) {
    stop("Geographic coordinates must be supplied.")
  }
  if (is.null(time)) {
    stop("A time object must be supplied.")
  }
  if (!is.POSIXct(df[time][[1]])){
    stop("Time variable must be POSIXct format.")
  }
  if (length(time.units)!=1) {
    stop("time.units must be a single value.")
  }
  if (!time.units %in% c("hour", "date", "month")) {
    stop("time.units must be hour, date, or month. ")
  }

  if (is.null(groupvar)) {
    df2<- df[,c(coor, time)]
    f<- get(time.units)
    df2[,time.units]<- f(df2[,time])
    df2<- df2[order(df2[,time]),]
    df2$group<- seqgroup(df2, var = time.units)
    message("groupvar is not provided, using time.units as grouping varialbe.")
  } else if (!is.null(groupvar)) {
    df2<- df[,c(groupvar, coor, time)]
    f<- get(time.units)
    df2[,time.units]<- f(df2[,time])
    df2<- transform(df2, newid =
                            as.numeric(interaction(get(groupvar), get(time.units), drop=TRUE)))
    df2<- df2[order(df2[,groupvar],df2[,time]),]
    df2$group<- seqgroup(df2, var = "newid")
  }

# deprecated code using plyr
#  df3<- ddply(df2, .(group), function(z){
#    data.frame(rg = sdspatialpoints(z, coor = coor))
#  })

  df3 <- df2 %>%
    group_by(group) %>%
    group_modify(~ data.frame(rg = sdspatialpoints(.x, coor = coor)))

  df2<- merge(df2, df3, by="group")
  return(df2$rg)
}
