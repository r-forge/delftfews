% $Id: as.seconds.Rd 3 2010-07-07 13:42:11Z mariotomo $

\name{findLocalExtremes}
\alias{findLocalExtremes}
\alias{findLocalMin}
\alias{findLocalMax}
\title{whether a value in a sequence is a local extreme}
\description{
  the \code{findLocalMin} and \code{findLocalMax} functions take an
  object of type vector and return a boolean vector of the same length,
  that holds \code{TRUE} if the element is a local extreme.  if there is
  a plateau, you will get the index of the first element in the plateau.
}
\usage{
findLocalMin(x)
}
\arguments{
  \item{x}{a vector.}
}
\examples{
v <- c(1,2,3,4,3,2)
findLocalMax(v)
# returns c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
}
