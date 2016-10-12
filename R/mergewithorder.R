#' Merge with Order
#' 
#' This function merges two dataset without changing the order of the base 
#' dataset. 
#' 
#' @param df1 a data frame object, the order of which needs to be unchanged.
#' @param df2 a data frame object be joined with \code{df1}
#' @param by unique identifiers used to join \code{df1} and \code{df2}, same 
#' as \code{by} in \code{merge}
#' 
#' @return a joined data frame with the same order as \code{df1}
#' 
#' @seealso \code{\link{merge}}
#' 
#' @examples 
#' data(mobility)
#' mobility$distgroup<- groupdist(mobility, coor = c("lon", "lat"), threshold = 100, groupvar = "id")
#' latmean<- aggregate(lat~distgroup, mobility, mean)
#' names(latmean)<- c("distgroup","latmean")
#' mobility<- mergewithorder(mobility, latmean, by = "distgroup")
#' 
#' @export

mergewithorder<- function(df1, df2, by = NULL) {
  if (is.atomic(df1)) {
    df1 <- data.frame(x = df1)
  }
  if (is.atomic(df2)) {
    df2 <- data.frame(x = df2)
  }
  if (is.null(by)) {
    stop("Unique identifier must be specified to merge.")
  }
  
  df1$sequenceid<- seq(1:nrow(df1))
  df1<- merge(df1, df2, by = by, all = T)
  df1<- df1[order(df1$sequenceid),]
  df1$sequenceid<- NULL
  
  return(df1)
}