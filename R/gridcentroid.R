#' Calculate Stay Points
#'
#' This function identifies latent stay points based on spatial locations of stay events. A spatial
#' grid with adjustable cell sizes is overlaid on stay events and the centroid of all the stay
#' events fallen with one grid is considered a stay point. All the stay events are viewed as visits
#' to the stay point.
#'
#' @param df a data frame object.
#' @param coor longitude and latitude of the spatial points in the format of c("lon","lat").
#' @param cell.size size of the the spatial grid to overlay with spatial points.
#' @param cell.units unit of the cell size length. It has to be meters.
#' @param point.id id of the input spatial points.
#'
#' @return a data frame with three new columns added showing stay points and locations.
#'
#' @note \code{gridcentroid} function can be slow if the spatial points are scattered in a large area and grid size is
#' small. It is recommended to run in small batches.
#'
#' @seealso \code{\link{stayevents}}
#'
#' @references Toole, J.L., et al. The path most traveled: Travel demand estimation using big data resources. Transport.
#' Res. Part C (2015), \link{http://dx.doi.org/10.1016/j.trc.2015.04.022}
#'
#' @examples
#' data(mobility)
#' mobility_stay<- stayevent(mobility, coor = c("lon","lat"), time = "datetime", dist.threshold = 100,
#'                  time.threshold = 30, time.units = "mins", groupvar = "id")
#'
#' @import sp
#'
#' @export
#'
#'

gridcentroid <- function(df, coor = NULL, cell.size = NULL,
                      cell.units = "meters", point.id = NULL,
                      group.id = NULL){
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
  if (is.null(group.id)) {
    group.id <- rep(1, nrow(df))
    df <- cbind(df, group.id)
    message("group.id is not specified, treating all the data as one group.")
  }

  grid.points <- df[c(coor, point.id, group.id)]
  grid.points <- unique(grid.points)
  grid.points[c(coor, point.id, group.id)] = apply(grid.points[c(coor, point.id, group.id)], 2,
                                         function(x) as.numeric(as.character(x)))

  if (nrow(grid.points) == 1){
    staypoint.id <- as.numeric(paste0(df[point.id], "_1"))
    staypointlon <- df[1, coor[1]]
    staypointlat <- df[1, coor[2]]
    df <- cbind(df, staypoint.id, staypointlon, staypointlat)
  } else {
    # create a spatial grid
    a <- min(grid.points[, coor[1]])
    b <- min(grid.points[, coor[2]])
    cellsize <- 0.0001 / 7.871 * cell.size
    c <- ceiling((max(grid.points[, coor[1]]) - a) / cellsize)
    d <- ceiling((max(grid.points[, coor[2]]) - b) / cellsize)
    cellsdim <- max(c, d)
    grid<- GridTopology(cellcentre.offset= c(a, b),
                        cellsize = c(cellsize, cellsize),
                        cells.dim = c(cellsdim, cellsdim))
    sg<- SpatialGrid(grid)
    poly<- as.SpatialPolygons.GridTopology(grid)
    proj4string(poly) <-  CRS("+proj=longlat +datum=WGS84")
    # join points to grid and get centroid
    points <- SpatialPoints(grid.points)
    proj4string(points) <-  CRS("+proj=longlat +datum=WGS84")
    result <- data.frame(points,grid=over(points,poly))
    spk <- aggregate(cbind(result[, 1], result[, 2])~get(group.id)+grid, data=result, mean)
    names(spk) <- c(group.id, "grid", "splon", "splat")
    spk$staypoint.id <- 1:nrow(spk)
    spk$staypoint.id <- paste0(spk$grid, "_", spk$staypoint.id)
    result <- merge(result, spk, by=c(group.id, "grid"))
    result <- result[ , -which(names(result) %in% coor)]
    df <- merge(df, result, by=c(group.id, point.id))
  }
  return(df)
  }


