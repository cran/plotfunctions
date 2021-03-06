% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{list2str}
\alias{list2str}
\title{Combine list values as string.}
\usage{
list2str(x, inputlist)
}
\arguments{
\item{x}{A vector with the names or numbers of list elements to be combined.}

\item{inputlist}{A (named) list with information, e.g., graphical parameter settings.}
}
\value{
String
}
\description{
Combine list values as string.
}
\examples{
test <- list(a=c(1,2,3), b='a', c=c(TRUE, FALSE), d='test')
list2str(c('a','c', 'd'), test) 
}
\seealso{
Other Utility functions: 
\code{\link{findAbsMin}()},
\code{\link{find_n_neighbors}()},
\code{\link{firstLetterCap}()},
\code{\link{getArrowPos}()},
\code{\link{getDec}()},
\code{\link{getRange}()},
\code{\link{getRatioCoords}()},
\code{\link{get_palette}()},
\code{\link{group_sort}()},
\code{\link{inch2coords}()},
\code{\link{isColor}()},
\code{\link{move_n_point}()},
\code{\link{orderBoxplot}()},
\code{\link{se}()},
\code{\link{sortGroups}()}
}
\concept{Utility functions}
