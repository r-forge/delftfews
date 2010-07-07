% $Id$

\name{cumulate}
\alias{cumulate}
\alias{cumulate.timeseries}
\title{augments a time series set with net and gross sums of one data column}
\description{
  
  performs a discrete integration.  given the `input` timeseries set,
  uses its `minutes` data and the indicated `column` data to compute the
  integral value of the data.

  stretches are integrated "net" and "gross", where the gaps between
  "gross" stretches are at most `gap` wide.

  numeric values thus computed can be seen as events and they are
  located at the timestamp of the earliest number of the stretch.  all
  other entries are `NA`.

  it's best to specify `data.description$missVal = NULL` when passing
  the result of `cumulate.timeseries` to `write.PI`.  `NA` events (no
  event) will be omitted.
  
}
\usage{
augmented <- cumulate.timeseries(input, column, gap)
}
\arguments{
  \item{input}{data.frame; the time series set to be augmented.}
  \item{column}{character; the name of the data holding column.}
  \item{gap}{numeric; the size of the minimal gap that interrupts gross stretches.}
}
\examples{
input <- c(0, 0, 0, 5, 6, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0)
result <- timeseries(20580000, by=5, length.out=length(input), input=input)
cumulate.timeseries(result, gap=3)
}