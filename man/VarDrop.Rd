% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{VarDrop}
\alias{VarDrop}
\title{Drop one or more variables from a data frame.}
\usage{
VarDrop(data, Var)
}
\arguments{
\item{data}{a data frame.}

\item{Var}{character vector containing the names of the variables to drop.}
}
\description{
\code{VarDrop} drops one or more variables from a data frame.
}
\examples{
# Create dummy data
a <- c(1, 2, 3, 4, NA)
b <- c( 1, NA, 3, 4, 5)
c <- c(1:5)
d <- c(1:5)
ABCData <- data.frame(a, b, c, d)

# Drop a and b
DroppedData <- VarDrop(ABCData, c('b', 'c'))

}
