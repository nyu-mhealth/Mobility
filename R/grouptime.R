#' Group by Time Threshold
#'
#' This function groups rows by time difference. A time threshold must be given. If the time difference
#' is longer than time threshold, rows are put in the same group. Otherwise, group number is NA.
#'
#' @param df a data frame object.
#' @param time a POSIXct time object used to calculate time difference
#' @param units a character string indicating which units time difference is caluclated in.
#' @param threshold a numeric object. Value should have the same unit specified in the \code{units} parameter.
#' @param groupvar a character string. The name of a variable that is used to group rows. Time difference is
#' calculated using the earliest and lastest time in each group. Note: data frame is not sorted in the function,
#' recommend to sort before running the function.
#'
#' @return a list of 1 and 0 indicating whether a set of rows are in the same group
#' or not in any groups.
#'
#' @seealso \code{\link{seqgroup}}
#'
#' @import lubridate
#' @import plyr
#'
#' @export

grouptime<- function(df, time = NULL, units = c("auto", "secs", "mins", "hours", "days", "weeks"),
                     threshold = NULL, groupvar = NULL) {
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }

  if (!is.POSIXct(df[time][[1]])){
    stop("Time variable must be POSIXct format.")
  }

  if (is.null(threshold)){
    stop("A threshold must be supplied to generate time groups.")
  }

  if (is.null(groupvar)){
    timediff<- as.numeric(difftime(df[nrow(df),time], df[1,time], units = units))
    df$timegroup<- ifelse(timediff>=threshold, 1, 0)
    return(df$timediff)
  }

  if (!is.null(groupvar)){
    df3<- ddply(df, .(get(groupvar)), function(z){
      data.frame(timediff = as.numeric(difftime(z[nrow(z),time], z[1,time]), units = units))
    })
    df3$timegroup<- ifelse(df3$timediff>=threshold, 1, 0)
    names(df3)<- c(groupvar, "timediff","timegroup")
    df3<- subset(df3, !is.na(get(groupvar)))
    df<- mergewithorder(df, df3, by=groupvar)

    return(df$timegroup)
  }
}
