% File src/R/man/rollingSum.Rd
% $Id$

\name{rollingSum}
\alias{rollapply}
\alias{rollingSum}
\alias{[.zoo}
\title{apply aggregating functions to contiguous stretches of data}
\description{  

  returns the rolling application of `fun` to input (`n`th element in
  returned vector is `fun` of `count` elements in input from `n - count`
  to `n`.)

  count must be positive.
  result is same length as input (starts with `count-1` NA).

  commodity function `rollingSum` invokes `rollapply` with `fun=sum`.
  `na.action` defaults to `na.pass` for `rollapply` and to `na.zero` for
  `rollingSum`.

}
\usage{
rollapply(data, count, fun, na.action=na.pass)
rollingSum(data, count, na.action=na.zero)
}
\arguments{
  \item{data}{the input data vector with instantaneous values}
  \item{count}{the number of values of the sum.}
  \item{fun}{the function to apply to the elements in the stretch.}
  \item{na.action}{what to do with NA values.}
}
\examples{
# on this input
input <- c(rep(1:3, 2), NA)
# [1]  1  2  3  1  2  3 NA

# this sums the two elements [i-1, i]
rollingSum(input, 2)
# [1] NA  3  5  4  3  5  3

# as above, but average without filling in NA elements:
rollapply(input, 2, mean)
# [1] NA 1.5 2.5 2.0 1.5 2.5  NA

# the three elements [i-2, i-1, i]
rollingSum(input, 3)
# [1] NA NA  6  6  6  6  5

rollingSum(input, 4)
# notice how the first `count - 1` elements in the result are NA
# [1] NA NA NA  7  8  9  6

rollapply(input, 4, mean)
# [1]   NA   NA   NA 1.75 2.00 2.25   NA
rollapply(input, 4, mean, na.action=na.zero)
# [1]   NA   NA   NA 1.75 2.00 2.25 1.50
}
