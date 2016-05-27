% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{getDec}
\alias{getDec}
\title{Return the number of decimal places.}
\usage{
getDec(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
Number of decimals
}
\description{
Return the number of decimal places.
}
\examples{
getDec(c(10,10.432, 11.01, .000001))
}
\author{
Based on http://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r, but improved
}
\seealso{
Other Utility functions: \code{\link{findAbsMin}},
  \code{\link{find_n_neighbors}}, \code{\link{getRange}},
  \code{\link{group_sort}}, \code{\link{list2str}},
  \code{\link{move_n_point}}, \code{\link{se}}
}
