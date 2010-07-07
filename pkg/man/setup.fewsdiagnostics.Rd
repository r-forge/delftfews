% File src/R/man/setup.fewsdiagnostics.Rd

\name{setup.fewsdiagnostics}
\alias{setup.fewsdiagnostics}
\title{initialises the logger for Lizard-FEWS with given filename}
\description{
  initialises the logging logger for writing the logs with a level to the Lizard-Fews diagnostics xml file

}
\usage{
  setup.fewsdiagnostics(filename)
}
\arguments{
  \item{filename}{the name of the xml file the diagnostics are written to}
}
\examples{
 setup.fewsdiagnostics('diagnostics.xml')
}
