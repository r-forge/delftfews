% $Id$

\name{select.percentiles}
\alias{select.percentiles}
\title{selects percentile columns from a timeseries set}
\description{
  
  \code{select.percentiles} works on timeseries sets as defined in this
  package.

  given a timeseries set \code{input}, a set of \code{percentiles} and
  an optional \code{score.function} with any extra parameters this may
  need, \code{select.percentiles} selects the timeseries corresponding
  to the specified \code{percentiles}.

  \code{score.function} defaults to summing up all first 12 elements of
  the timeseries.

  the returned object is a timeseries set that has equal timestamps as
  the \code{input} and as many data column as the length of
  \code{percentiles}.
  
}
\usage{
select.percentiles(input, percentiles, score.function, ...)
}
\arguments{
  \item{input}{a timeseries set.  first column contains timestamps,
    remaining columns may be equally named and contain realizations of
    the same experiment.}
  \item{percentiles}{a vector with the percentiles to be selected.}
  \item{score.function}{a function that computes a score for each column of the
    timeseries set.  defaults to summing up the first 12 elements.}
  \item{...}{extra parameters are passed to the `score.function` function.}
}
\examples{
 l <- rep(1:100, each=22)
 dim(l) <- c(22, 100)
 l <- data.frame(l)
 colnames(l) <- rep('a', 100)
 pidata <- zoo(l, as.Date(seq(from=12600, by=300, length.out=22)))
 current <- select.percentiles(pidata, c(30, 80))
}
