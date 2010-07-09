% $Id$

\name{contiguous.stretch}
\alias{contiguous.stretch}
\alias{contiguous.stretch}
\title{finds the adjacent locations equal to given one}
\description{
  given a vector of values and a position, it returns a boolean vector
  of the elements satisfying the condition.  the condition is either
  equality or disequality with the given value around the positon.

  
}
\usage{
contiguous.stretch(data, position, value, equality)
}
\arguments{
  \item{data}{a vector of whatever type.}
  \item{position}{numeric; the position within the input data.}
  \item{value}{same type as in `data`; optional, defaults to
    `data[position]`; the expected value at position.}
  \item{equality}{boolean; optional, defaults to TRUE; whether we are
    looking for the stretch of data that is equal to value or different.}
}
\examples{
data <- c(1,1,1,2,2,2,3,3,2,2,2,2,2,3,3,1,1,1,1,1)
which(contiguous.stretch(data, 1)) # gives 1, 2, 3
which(contiguous.stretch(data, 2)) # gives 1, 2, 3
which(contiguous.stretch(data, 5)) # gives 4, 5, 6
contiguous.stretch(data, 5, 1) # gives all FALSE
}
