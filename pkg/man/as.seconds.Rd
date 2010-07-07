% $Id$

\name{as.seconds}
\alias{as.seconds}
\alias{as.seconds.Date}
\alias{as.seconds.POSIXct}
\alias{as.seconds.default}
\alias{as.seconds.difftime}
\title{returns the amount of seconds in the object}
\description{
  the \code{as.seconds} functions take an object and depending on its
  type they convert it to the amount of seconds it contains.  Date and
  POSIXct objects are considered as difftime object from the unix epoch.
}
\usage{
as.seconds(value)
}
\arguments{
  \item{value}{the object that is to be converted into seconds.}
}
