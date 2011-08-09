% File src/R/man/rollingSum.Rd
% $Id$

\name{rollingSum}
\alias{rollingSum}
\title{apply aggregating functions to contiguous stretches of data}
\description{  

  `rollingSum` is a commodity function invoking `zoo::rollapply` with
  `FUN=sum`.

  `rollapply.delftfews` overrides `rollapply.zoo` defaults for `na.pad`
  (set to TRUE) and `align` (set to 'right').  this can also be done by
  changing global settings at the moment you load the library.

}
\usage{
rollingSum(data, width, na.action=na.zero)
}
\arguments{
  \item{data}{the input data vector with instantaneous values}
  \item{width}{the number of values of the sum.}
  \item{na.action}{what to do with NA values.}
}
\examples{
# on this input
input <- c(rep(1:3, 2), NA)
# [1]  1  2  3  1  2  3 NA

# this sums the two elements [i-1, i]
rollingSum(input, 2)
# [1] NA  3  5  4  3  5  3

# the three elements [i-2, i-1, i]
rollingSum(input, 3)
# [1] NA NA  6  6  6  6  5

rollingSum(input, 4)
# notice how the first `width - 1` elements in the result are NA
# [1] NA NA NA  7  8  9  6
}
