% File src/R/man/controller.thresholdOnOff.Rd

\name{double.threshold}
\alias{double.threshold}
\alias{double.threshold.default}
\alias{double.threshold.data.frame}
\alias{double.threshold.matrix}
\alias{multi.double.threshold}
\alias{multi.double.threshold.default}
\alias{multi.double.threshold.data.frame}
\alias{multi.double.threshold.matrix}
\title{which threshold was hit last (or how many of them).}
\description{
  
  returns a logical status vector.  at each position the status is TRUE
  if data exceeds the `threshold.true`, FALSE if falls below the
  `threshold.false`.  if the data lays between the thresholds, the last
  status is taken forward.  `threshold.true` must be higher than
  `threshold.false`.

  the `multi.` variants will count how many `thresholds` were exceeded.
  they expect the threshold.false and threshold.true as columns in the
  thresholds data.frame.
  
}
\usage{
double.threshold(data, threshold.false, threshold.true, initial.status, on.equality)
multi.double.threshold(data, thresholds, initial.status)
}
\arguments{
  \item{data}{the input data vector.  data frames are examined columnwise.}
  \item{threshold.false}{the lower threshold (off).}
  \item{threshold.true}{the upper threshold (on).}
  \item{thresholds}{a data.frame with two columns (threshold.false,
    threshold.true) and as many rows as the thresholds you want to count.}
  \item{initial.status}{initial status, used if initial value is between
    a false/true threshold pair.}
  \item{on.equality}{a boolean, indicates whether a threshold is to be
    considered reached also on equality or only on exceeding it.
    defaults to FALSE (equality is not enough).}
}
\examples{
# am I baking my merengues right?
temp <- c(50, 90, 110, 120, 117, 114, 124, 120, 118)
double.threshold(temp, 116, 119)
}
