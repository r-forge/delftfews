% $Id$

\name{timeseries}
\alias{timeseries}
\alias{[.delftfews}
\alias{[<-.delftfews}
\title{overriding zoo creator and accessors}
\description{
  
  using three of the four \code{from}, \code{to}, \code{by},
  \code{length.out} parameters, creates a data.frame that satisfies the
  requirements for a time series set.  any extra data is considered a
  measurement column and added in the standard R fashion.

  the returned data.frame has at least the column \code{timestamps}.
  the first timestamp is equal to \code{from}, the last is equal or less
  than \code{to}, the distance between two subsequent timestamps is
  \code{by}, there are \code{length.out} elements (\code{length.out - 1}
  steps of distance \code{by}).

  data may be specified as numeric, in which case it is considered
  indicating minutes, or as \code{POSIXct} objects.
  
}
\usage{
timeseries(from, to, by, length.out, order.by, ...)
}
\arguments{
  \item{from}{the starting instant of the resulting time series.
    either \code{POSIXct} or \code{double}.}
  \item{to}{the ending instant of the resulting time series.  either
    \code{POSIXct} or \code{double}.}
  \item{by}{the timestep in the timeseries.  either a \code{difftime} or a
    \code{numeric}}
  \item{length.out}{if specified, ensures the resulting timeseries
    contains \code{length.out} elements (\code{length.out - 1} steps).}
  \item{order.by}{if specified, the resulting timeseries uses these
    timestamps as index instead of building its own from the above
    described four parameters.}
  \item{...}{any other named field is added as field in the resulting
    timeseries.  the lenght must match the \code{length.out} parameter.}
}
\examples{
firstminutes <- timeseries(0, 1000, by=50, constant=355/113)
uptomidnight <- timeseries(to=Sys.Date(), by=90, length.out=5, countdown=4:0)
}
