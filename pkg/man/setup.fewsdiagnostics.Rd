% File src/R/man/setup.fewsdiagnostics.Rd

\name{setup.fewsdiagnostics}
\alias{setup.fewsdiagnostics}
\alias{teardown.fewsdiagnostics}
\title{functions for the Lizard-FEWS diagnostics file}
\description{

  setup.fewsdiagnostics and teardown.fewsdiagnostics initialise and
  finalize the fews diagnostics file.  between the two calls a handler
  is attached to "fews.diagnostics" logger.  this handler will help you
  produce a correct fews diagnostics file holding the logging records
  that hit that logger.

  only one such file can be generated so keep that in mind when you use
  these two functions.

}
\usage{
  setup.fewsdiagnostics(filename)
  teardown.fewsdiagnostics(...)
}
\arguments{
  \item{filename}{the name of the xml file the diagnostics are written to.}
  \item{...}{further arguments might be used in the future.}
}
\examples{
 setup.fewsdiagnostics('diagnostics.xml')
 loginfo("some message to fews", logger="fews.diagnostics")
 teardown.fewsdiagnostics('diagnostics.xml')
}
