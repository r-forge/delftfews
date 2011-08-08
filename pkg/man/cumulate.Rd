% $Id$

\name{cumulate}
\alias{cumulate}
\alias{cumulate.zoo}
\title{augments a time series with net and gross sums of one data column}
\description{
  
  given the univariate `input` timeseries, performs a discrete
  integration of its non-zero stretches of data.

}
\details{
  stretches are integrated "net" and "gross", where the gaps between
  "gross" stretches are at most `gap` wide.

  numeric values thus computed can be seen as events and they are
  located at the timestamp of the earliest non-zero entry of the
  stretch.  all other entries are `NA`.

  it's best to specify `data.description$missVal = NULL` when passing
  the result of `cumulate.zoo to `write.PI`.  `NA` events (no
  event) will be omitted.
}
\usage{

cumulate(input, gap = 1, integration.method = 3, units = "secs",
with.partials = FALSE, \dots)

}
\arguments{
  \item{input}{zoo; an univariate zoo series, indexed on POSIXct timestamps.}
  \item{gap}{numeric; the size of the minimal gap that interrupts gross stretches.}
  \item{integration.method}{numeric; refers to the overview presented
    in http://portal.acm.org/citation.cfm?id=578374, figure 7.2.  1:
    rectangular (top left), 2: rectangular (midpoint), 3: trapezoid,
    4: simpson's..}
  \item{units}{character; this is passed to as.double for converting
    differences in timestamps.  all units
    acceptable there can be used here.}
  \item{with.partials}{logic; whether the output should contain the
    partials used in the calculations.  in this case, this will be the
    fifth column in the output.}
  \item{\dots}{placeholder for future extensions}
}
\examples{
input <- c(0, 0, 0, 5, 6, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0)

inputAsDelftfews <- timeseries(20580000, by=5, length.out=length(input), input=input)
cumulate(inputAsDelftfews, gap=3)

inputAsZoo <- zoo(input, order.by = structure(seq(20580000, 20580075, 5), class = c("POSIXct","POSIXt")))
cumulate(inputAsZoo, gap=3)
}
