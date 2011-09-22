% File src/R/man/timeseries.timestamps.in.range.Rd
% $Id$

\name{timestamp.in.range}
\alias{timestamp.in.range}
\alias{timestamp.in.range.day}
\alias{timestamp.in.range.hour}
\alias{timestamp.in.range.weekday}
\alias{timestamp.in.weekend}
\alias{timestamp.in.range.calendar}
\title{returns boolean vector that indicates whether the timestamps are
  in a repeating range}
\description{
  These functions take a data.frame as returned by `read.PI` or
  `timeseries` and return a boolean vector that indicates whether the
  corresponding row of the data.frame matches the range period.

  The range is a repeating window and is specified by the length of the
  period in a chosen unit, the size of the unit in minutes (defaults to
  1) and its value at the unix `EPOCH` (1970-01-01 00:00:00 UTC).

  In testing whether a timestamp falls in the repeating interval, `from`
  is included while `to` is excluded.

  Commodity functions are provided for the most common cases.

  Notice that timestamps are absolute instants in time, that may be
  expressed in a 'local' timezone but are not intrinsically linked to
  any specific one.  when you ask whether a timestamp falls in a
  repeating range, the question is well defined only if you specify the
  timezone in which you want the answer.  per default, you will get an
  answer in the CET timezone.

  Also notice that in `timestamps.in.range.calendar` the type of the
  parameters `from` and `to` are different from the other commodity
  functions.

  `timestamps.in.weekend(data)` is equivalent to
  `timestamps.in.range.weekday(data, 6, 8)` (8: up to next Monday, excluded)
}
\usage{
timestamp.in.range(data, from, to, by, units, offset, tz)

timestamp.in.range.hour(data, from, to, tz)
timestamp.in.range.weekday(data, from, to, tz)
timestamp.in.range.calendar(data, from, to, tz)

timestamp.in.weekend(data, tz)
}
\arguments{
  \item{data}{the data.frame holding your timeseries.}
  \item{tz}{a timezone specification to be used for the conversion.
          It is system-specific and defaults to "CET".}
  \item{from}{the start of the valid window within the period, inclusive.}
  \item{to}{the end of the valid window within the period, exclusive.}
  \item{by}{the length of the period in the chosen `unit`.}
  \item{units}{a string to be passed to `as.double.difftime`.}
  \item{offset}{the value of the time measure at the unix EPOCH.}
}
\examples{
  pidata <- timeseries(from=1263553200, by=15*60, length.out=289, H.gewogen=-1.5)

  in.workweek <- timestamp.in.range.weekday(pidata, tz="UTC", from=1, to=6)
  on.saturday <- timestamp.in.range.weekday(pidata, tz="UTC", from=6, to=7)
  in.weekend <- timestamp.in.weekend(pidata, tz="UTC")
  between.hours <- timestamp.in.range.hour(pidata, 17, 10, tz="UTC")

  pidata <- timeseries(from=1234567800, by=57600*60, length.out=18)
  in.summer <- timestamp.in.range.calendar(pidata, "0401", "1001", tz="UTC")
  in.winter <- timestamp.in.range.calendar(pidata, "1001", "0401", tz="UTC")
}