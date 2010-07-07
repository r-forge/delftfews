% File src/R/man/teardown.fewsdiagnostics.Rd

\name{teardown.fewsdiagnostics}
\alias{teardown.fewsdiagnostics}
\title{closes the logger for Lizard-FEWS}
\description{
  closes the logging logger for the Lizard-Fews diagnostics xml file

}
\usage{
  teardown.fewsdiagnostics(filename)
}
\arguments{
  \item{filename}{the name of the xml file the diagnostics are written to}
}
\examples{
 teardown.fewsdiagnostics('diagnostics.xml')
}
