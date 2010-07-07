% File src/R/man/read.PI.Rd

\name{read.PI}
\alias{read.PI}
\title{read PI file as data.frame}
\description{
  reads a PI file, check the most common time step, force the
  data into a data.frame
}
\usage{
pidata <- read.PI(filename[, step.minutes, na.action])
}
\arguments{
  \item{filename}{the name of the xml file containing the timeseries}
  \item{step.minutes}{the timestep in the timeseries.  it is inferred
    from the data if left unspecified}
  \item{na.action}{what has to happen to NA values.  can be either
    `na.fill` or `na.pass`.  defaults to `na.fill`.}
}
\examples{
\dontrun{pidata <- read.PI('pi.xml', step.minutes=15)}

\dontrun{pidata <- read.PI('pi.xml')}

\dontrun{pidata <- read.PI('pi.xml', na.action=na.pass)}
}
