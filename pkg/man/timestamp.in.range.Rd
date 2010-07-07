% File src/R/man/timeseries.timestamps.in.range.Rd
% $Id$

\name{timestamp.in.range}
\alias{timestamp.in.range}
\alias{timestamp.in.range.day}
\alias{timestamp.in.range.hour}
\alias{timestamp.in.range.weekday}
\alias{timestamp.in.weekend}
\alias{timestamp.in.range.calendar}
\title{returns boolean vector that indicates whether the timestamps are in a date period}
\description{
  these functions take a data.frame as returned by `read.PI` or
  `timeseries` and return a boolean vector that indicates whether the
  corresponding row of the data.frame matches the range period.

  the range is a repeating window and is specified by the length of the
  period in a chosen unit, the size of the unit in minutes (defaults to
  1) and its value at the unix `EPOCH` (1970-01-01 00:00:00).

  commodity functions are provided for the most common cases.

  do notice that in `timestamps.in.range.calendar` the type of the
  parameters `from` and `to` are different from the other commodity
  functions.

  `timestamps.in.weekend(data)` is equivalent to
  `timestamps.in.range.weekday(data, 6, 7)`
}
\usage{
timestamp.in.range(data, from, to, by, units, offset)

timestamp.in.range.hour(data, from, to)
timestamp.in.range.weekday(data, from, to)
timestamp.in.range.calendar(data, from, to)

timestamp.in.weekend(data)
}
\arguments{
  \item{data}{the data.frame holding your timeseries.}
  \item{from}{the start of the valid window within the period.}
  \item{to}{the end of the valid window within the period.}
  \item{by}{the length of the period in the chosen `unit`.}
  \item{units}{a string to be passed to `as.double.difftime`.}
  \item{offset}{the value of the time measure at the unix EPOCH.}
}
