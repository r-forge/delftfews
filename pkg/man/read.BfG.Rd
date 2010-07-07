% File src/R/man/read.BfG.Rd

\name{read.BfG}
\alias{read.BfG}
\title{read BfG file as data.frame}
\description{
  reads a BfG file into a data.frame with one column
}
\usage{
pidata <- read.BfG(filename[, column])
}
\arguments{
  \item{filename}{the name of the file containing the timeseries}
  \item{column}{the name of the data holding column.  defaults to "value".}
}
\examples{
\dontrun{pidata <- read.BfG('some.data.BfG')}
}
