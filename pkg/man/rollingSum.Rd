% File src/R/man/rollingSum.Rd
% $Id$

\name{rollingSum}
\alias{rollapply.default}
\alias{rollingSum}
\alias{[.zoo}
\title{apply aggregating functions to contiguous stretches of data}
\description{  

  returns the rolling application of `FUN` to input (`n`th element in
  returned vector is `FUN` of `width` elements in input from `n - width`
  to `n`.)

  width must be positive.
  result is same length as input (starts with `width-1` NA).

  commodity function `rollingSum` invokes `rollapply` with `FUN=sum`.
  `na.action` defaults to `na.pass` for `rollapply` and to `na.zero` for
  `rollingSum`.

}
\usage{
rollingSum(data, width, na.action=na.zero)
rollapply.default(data, width, FUN, ...)
}
\arguments{
  \item{data}{the input data vector with instantaneous values}
  \item{width}{the number of values of the sum.}
  \item{FUN}{the function to apply to the elements in the stretch.}
  \item{na.action}{what to do with NA values.}
  \item{...}{required by the general version.}
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
