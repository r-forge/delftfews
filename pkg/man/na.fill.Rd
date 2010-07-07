% $Id$

\name{na.fill}
\alias{na.fill}
\alias{na.interpolate}
\alias{na.zero}
\title{handle missing values in objects}
\description{

  use these functions as argument to functions that require you specify
  a `na.action`.
  
  \code{na.fill} returns the object, where all NA are replaced with
  observations from the object.  all observed values are carried forward
  and the first is carried backward.  cfr na.locf from zoo library.

  \code{na.zero} returns the object, where all NA are replaced with
  zeroes.

  \code{na.interpolate} fills in the gaps linearly interpolating between
  the last preceding and first following observations.  if the gaps are
  at the head or tail of the vector, they are filled in as constants.

  these are not generic functions: the object must be a vector (a
  data.frame is a vector, too).

}
\usage{
na.fill(object)
na.zero(object)
na.interpolate(object)
}
\arguments{
  \item{object}{the vector, possibly containing NA values.}
}
