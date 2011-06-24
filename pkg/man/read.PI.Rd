% File src/R/man/read.PI.Rd

\name{read.PI}
\alias{read.PI}
\title{read PI file as data.frame}
\description{
  reads a PI file, check the most common time step, force the
  data into a data.frame.

  the input PI file may define a non zero `timeZone`.  this is
  acknowledged and the date/time instants in the `timestamps` column
  will appear to you as shifted: keep in mind that a time instant is
  independent of the time zone concept: only its string representation
  is affected.  the standard `format` function, will help you see the
  data in the same form as in the input, just specify the correct `tz`.

  the time series set read in the PI file is forced into a set of
  equally spaced time series.  the step chosen will be the most common
  step in the PI file.  there is no guarantee that this is what you
  really need nor meant.
  
}
\usage{
read.PI(filename, step.seconds = NA, na.action = na.fill)
}
\arguments{
  \item{filename}{the name of the xml file containing the timeseries}
  \item{step.seconds}{the timestep in the timeseries.  it is inferred
    from the data if left unspecified}
  \item{na.action}{what has to happen to NA values.  can be any function
    that operates on the NA values of a vector, as for example `na.pass`
    or `na.zero`.  defaults to `na.fill`.}
}
\examples{
\dontrun{pidata <- read.PI('pi.xml', step.seconds=900)}

\dontrun{pidata <- read.PI('pi.xml')}

\dontrun{pidata <- read.PI('pi.xml', na.action=na.pass)}
}
