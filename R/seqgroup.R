#' Generates Sequential Groups Based on Values
#'
#' This function groups variable based on its value. The variable is not sorted, so make
#' sure to sort before running the function. Rows with the same value but not adjacent
#' are not in the same group.
#'
#' @param df a data frame object.
#' @param var the variable in \code{df} grouping is based on. Group number changes when
#' value becomes different with the last value regardless of what the actual value is.
#'
#' @return a numeric list of group numbers
#'
#' @examples
#' A<- rep(1:3, times = 2, each = 2)
#' A<- data.frame(A)
#' A$group<- seqgroup(A, var = "A")
#'
#' @importFrom DataCombine slide
#' @importFrom zoo na.locf
#'
#' @export

seqgroup<- function(df, var = NULL) {
  if (is.atomic(df)) {
    df <- data.frame(x = df)
  }

  if (is.null(var)) {
    group<- seq(1:nrow(df))
  } else {
    group<- rep(1, nrow(df))

    df2<- cbind(df[var], group)
    suppressMessages(
      df2 <- slide(df2, Var = var, slideBy = -1)
    )
    var2<- paste0(var,"-1")

    df2[1,var2]= df2[1,var]
    df2$group<- ifelse(df2[var]==df2[var2],0, cumsum(df2$group)+1)
    is.na(df2$group)<- df2$group==0
    df2[1,"group"]<- 1
    df2$group<- na.locf(df2$group,na.rm=FALSE)
    df2$group<- as.numeric(df2$group)

  }
  return(df2$group)
}
