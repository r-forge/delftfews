% $Id$

\name{get.step}
\alias{get.step}
\alias{get.step.default}
\alias{get.step.zoo}
\title{returns the timestep in the object}
\description{
  the \code{get.step} functions take an object and depending on its
  type they compute the most common (or the declared) timestep.
}
\usage{
get.step(L, require.constant)
}
\arguments{
  \item{L}{the object from which to extract the timestep.}
  \item{require.constant}{if the inferred timestep is not constant,
    return NA.}
}
\examples{
pidata <- timeseries(from=1234567890, to=1234587890, length.out=21)
get.step(pidata)
}