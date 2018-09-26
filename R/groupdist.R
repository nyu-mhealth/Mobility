#' Group Spatial Points by Distance
#'
#' This function groups spatial points based on the distance to the next point. If distance is
#' smaller than a threshold, points are considered in the same group. Group number for points
#' outside the threshold is NA. Depending on how many spatial points are in the data frame,
#' runtime might be long.
#'
#' @param df df a data frame object.
#' @param coor longitude and latitude of the spatial points in the format of c("lon","lat").
#' @param threshold a distance threshold used to determine if points are in the same group. The
#' format is numeric. Unit is meters.
#' @param groupvar grouping object to stratify spatial points. Recommend to be ID for each individual.
#' If \code{groupvar} is not specified, all spatial points processed based on their original sequence.
#'
#' @return a list of numbers and NAs indicating whether a set of spatial points are in the same group
#' or not in any groups.
#'
#' @seealso \code{\link{seqgroup}}
#'
#' @examples
#' data(mobility)
#' mobility$distgroup<- groupdist(mobility, coor = c("lon", "lat"), threshold = 100, groupvar = "id")
#'
#' @import plyr
#' @importFrom geosphere distVincentyEllipsoid
#' @importFrom DataCombine slide
#'
#' @export

groupdist<- function(df, coor = NULL, threshold = NULL, groupvar = NULL) {
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }
  if (is.null(coor)) {
    stop("Geographic coordinates must be supplied.")
  }
  if (is.null(threshold)) {
    stop("A threshold must be specified.")
  }

  if (is.null(groupvar)) {
    group<- rep(1, nrow(df))
    df2<- cbind(df[,coor], group)
    suppressMessages(
      df2 <- slide(df2, Var = coor[1], slideBy = -1)
    )
    suppressMessages(
      df2 <- slide(df2, Var = coor[2], slideBy = -1)
    )
    suppressMessages(
      df2 <- slide(df2, Var = coor[1], slideBy = 1)
    )
    suppressMessages(
      df2 <- slide(df2, Var = coor[2], slideBy = 1)
    )
    message("groupvar is not specified, treating all the data as one group.")
  }

  if (!is.null(groupvar)) {
    group<- rep(1, nrow(df))
    df2<- cbind(df[,coor], df[groupvar], group)
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
  dist2<- distVincentyEllipsoid(latlon, latlonlag2, a=6378137, b=6356752.3142, f=1/298.257223563)
  df2<- cbind(df2, dist2)
  df2$disttestgroup<- ifelse(df2$dist2<= threshold, 1, 0)
  df2$disttestgroup<- ifelse(is.na(df2$disttestgroup), 2, df2$disttestgroup)

  df2$distgroup<- seqgroup(df2, var = "disttestgroup")
  df2$distgroup<- ifelse(df2$disttestgroup!=1, NA, df2$distgroup)

  suppressMessages(
    df2 <- slide(df2, Var = "distgroup", slideBy = -1)
  )
  df2$distgroup<- ifelse(is.na(df2$distgroup), df2$`distgroup-1`, df2$distgroup)

  return(df2$distgroup)
}
