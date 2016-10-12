#' Calculate Stay Events
#' 
#' This function calculates stay events based on spatial locations and time stamps of mobility data. A distance 
#' threshold and a time threshold are given to determine if points are in the same stay event set. The centroid 
#' of points in one stay event set is calculated as the spatial location of the stay event. 
#' 
#' @param df a data frame object. 
#' @param coor longitude and latitude of the spatial points in the format of c("lon","lat").
#' @param time a POSIXct time object, used to calculate time period. 
#' @param dist.threshold a distance threshold used to determine if points are in the same group. The 
#' format is numeric. Unit is meters.
#' @param time.threshold a numeric object. Value should have the same unit specified in the \code{time.units} 
#' parameter. 
#' @param time.units a character string indicating which units time difference is caluclated in. 
#' @param groupvar grouping object to stratify time objects. Recommend to be ID for each individual. 
#' If \code{groupvar} is not specified, \code{time.units} will be used to sort data and calculate radius of gyration.
#' 
#' @return a data frame with three new columns added showing stayevents and locations. 
#' 
#' @note \code{stayevent} function doesn't sort the data frame. It is recommended to sort data frame based on 
#' time and then run the function. 
#' 
#' @seealso \code{\link{groupdist}},\code{\link{grouptime}},\code{\link{seqgroup}}
#' 
#' @references Toole, J.L., et al. The path most traveled: Travel demand estimation using big data resources. Transport.
#' Res. Part C (2015), \link{http://dx.doi.org/10.1016/j.trc.2015.04.022}
#' 
#' @examples 
#' data(mobility)
#' mobility_stay<- stayevent(mobility, coor = c("lon","lat"), time = "datetime", dist.threshold = 100, 
#'                  time.threshold = 30, time.units = "mins", groupvar = "id")
#'
#' @export


stayevent<- function (df, coor = NULL, time = NULL, dist.threshold = NULL, time.threshold = NULL, 
                      time.units = c("auto", "secs", "mins", "hours", "days", "weeks"), 
                      groupvar = NULL, ...) {
  df$distgroup<- groupdist(df, coor = coor, threshold = dist.threshold, groupvar = groupvar)
  df$timegroup<- grouptime(df, time = time, units = time.units, threshold = time.threshold, groupvar = "distgroup")
  df$stayeventgroup<- ifelse(!is.na(df$distgroup) & df$timegroup == 1, df$distgroup, NA)
  
  stayevents<- aggregate(cbind(get(coor[1]), get(coor[2]))~stayeventgroup, df, mean)
  names(stayevents)<- c("stayeventgroup", "stayeventlon", "stayeventlat")
  
  df<- mergewithorder(df, stayevents, by="stayeventgroup")
  df$distgroup<- NULL
  df$timegroup<- NULL
  
  return(df)
}