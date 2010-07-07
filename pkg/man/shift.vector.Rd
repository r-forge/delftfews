% File src/R/man/shift.vector.Rd

\name{shift.vector}
\alias{shift.vector}
\title{shift values in a vector}
\description{
  returns a vector with the same length as the original one, all values
  are shifted by a specified amount of positions, at one end NA values
  are introduced.
}
\usage{
shift.vector(v, by)
}
\arguments{
  \item{v}{the vector that has to be shifted}
  \item{by}{the amount of positions.}
}
\examples{
shift.vector(c(1,2,3), 1)
shift.vector(c(1,2,3), -1)
}
