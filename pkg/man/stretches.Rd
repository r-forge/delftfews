% $Id$

\name{stretches}
\alias{stretches}
\title{finds first non-zero stretch of elements}
\description{
  returns the position of the first non-zero stretch of elements.
  gaps shorter than `gap` do not interrupt a stretch.
}
\usage{
stretches(input, gap, what, zero.surrounded)
}
\arguments{
  \item{input}{a vector of numerical values.}
  \item{gap}{the smallest sequence of NA or zeroes that will interrupt a
    stretch.  defaults to 1 (sequences must be contiguous).}
  \item{what}{character; if '"start"', finds where stretches start. if
    '"end"', finds where stretches end. defaults to '"start"'.}
  \item{zero.surrounded}{logical; if `TRUE`, assumes infinite zeroes
    leading and trailing the input vector.}
}
\examples{
input <- rep(0, 90)
input[6:10] <- NA
input[12:19] <- 1       # contiguous data
input[50+(0:3)*2] <- 2  # data with 1-gaps
input[70+(0:3)*3] <- 3  # data with 2-gaps

stretches(input)
# [1] 12 50 52 54 56 70 73 76 79
stretches(input, 2)
# [1] 12 50 70 73 76 79
stretches(input, 3)
# [1] 12 50 70

}