#' Calculate Stay Points
#'
#' This function identifies latent stay points based on spatial locations of stay events. A spatial
#' grid with adjustable cell sizes is overlaid on stay events and the centroid of all the stay
#' events fallen with one grid is considered a stay point. All the stay events are viewed as visits
#' to the stay point.
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
#' @importFrom sp SpatialPoints
#'
#' @export
#'
#'

gridcentroid <- function(df, coor = NULL, cell.size = NULL,
                      cell.units = "meters", point.id = NULL){
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }
  if (is.null(coor)) {
    stop("Geographic coordinates must be supplied.")
  }
  if (is.null(cell.size)) {
    stop("A cell size (in meters) must be provided.")
  }
  if (!cell.units == "meters") {
    stop("Cell size must be in meters")
  }
  if (is.na(point.id)){
    df$point.id <- seq(1, nrow(df))
    point.id <- "point.id"
  }

  grid.points <- df[c(coor, point.id)]

  if (nrow(grid.points) == 1){
    staypoint.id <- 1
    staypointlon <- df[1, coor[1]]
    staypointlat <- df[1, coor[2]]
    df <- cbind(df, staypoint.id, staypointlon, staypointlat)
  } else {
    points <- SpatialPoints(grid.points)
  }
}

staycandidate$cand_idno<- seq(1:nrow(staycandidate))
staycandidate2<- subset(staycandidate, select=c(i, mlat, mlon, cand_idno))

staycandidate2<- staycandidate2[,c(3,2,1,4)]
staypoint<- NULL
staycandidate_grid<- NULL
K<- max(staycandidate2$i)
for (k in 1:K){
  points <- staycandidate2[staycandidate2$i==k,]
  if (nrow(points)!=0){
    points<- SpatialPoints(points)
    proj4string(points) <-  CRS("+proj=longlat +datum=WGS84")
    a<- min(points$mlat)
    b<- min(points$mlon)
    grid<- GridTopology(cellcentre.offset= c(b,a), cellsize = c(0.0254,0.0254), cells.dim = c(1000,1000))
    sg<- SpatialGrid(grid) # 2km grid cell size
    poly<- as.SpatialPolygons.GridTopology(grid)
    proj4string(poly) <-  CRS("+proj=longlat +datum=WGS84")
    result <- data.frame(points,grid=over(points,poly))
    spk <- aggregate(cbind(mlat, mlon)~grid,data=result,mean)
    spk$id<- k
    staypoint<- rbind(staypoint, spk)
    staycandidate_grid<- rbind(staycandidate_grid, result)
  }
}
staypoint<- rename(staypoint, c(mlat="splat",mlon="splon"))
