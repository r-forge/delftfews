% File src/R/man/write.PI.Rd

\name{write.PI}
\alias{write.PI}
\title{write parts of a data.frame into PI format}
\description{
  use this function to produce a PI file.

  each column of the data that has a describing row in
  data.description will be output to the output.file.

  you can specify global.data that will be added to each and every
  series.event element in the output.

  each row of the `data.description` data.frame describes a series.  the
  `column` field is matched with a column in data`.  other
  necessary columns are: type, locationId, parameterId, timeStep,
  startDate and endDate.  optional columns are missVal, longName,
  stationName, units.

  missVal specifies the way NA (missing) values must be handled.
  - NULL: skips the element.
  - NA: set the value to 0 and flag it as missing.
  - numeric: set the value to this missVal.
}
\usage{
write.PI(data, data.description, filename, global.data)
}
\arguments{
  \item{data}{the timeseries.  must contain the column `timestamps`}
  \item{data.description}{a data.frame.  each row describes a column in the
    `data` that goes in the PI file.}
  \item{filename}{the name of the output xml file}
  \item{global.data}{an optional data.frame containing one row, each field
    describes one element, to be added to the header of all series.  
  }
}