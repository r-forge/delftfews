% File src/R/man/read.dcf.parsed.Rd

\name{read.sheet}
\alias{read.sheet}
\title{read configuration files as (list of) data frames.}
\description{

  these functions are meant for reading various types of configuration
  files, returning the content as (lists of) data frames.
  
  `read.sheet` reads a file that contains named data.frames and returns
  the single requested sheet.
  
}
\details{

  the first block is assumed to hold only one textual record.

  all following blocks are assumed to hold named records formed of only
  numeric values.  the first field is the name of the data.frame in the
  resulting list, the value of the first field is the name of the record
  (the row in the data.frame).

  an example will help understand.  imagine you need defining the global
  properties \code{fileDescription} and \code{sourceSystem} and two
  'scenario' records, named 'nat' and 'droog'.  you put the information
  in a dcf file, looking something like this:
  
  \preformatted{
    fileDescription: overstortput, by LzScripter
    sourceSystem: Lizard G-A Functions

    scenario: nat
    aanslagpeil: -0.38
    afslagpeil: -0.40
    gemaalcapaciteit: 300.0

    scenario: droog
    aanslagpeil: -0.42
    afslagpeil: -0.44
    gemaalcapaciteit: 300.0
  }

  reading the above file using \code{read.dcf.parsed} will give you a
  list with two elements, named `"fileDescription"` and `"scenario"`.
  the two elements would look like this:
  
  \preformatted{
    $fileDescription
                  fileDescription         sourceSystem
    1 overstortput, by LzScripter Lizard G-A Functions

    $scenario
            aanslagpeil afslagpeil gemaalcapaciteit
      nat         -0.38      -0.40              300
      droog       -0.42      -0.44              300
    }

    \code{write.PI} uses this format for one of its input parameters.
    
}
\usage{
read.sheet(file, sheet, header, sep, fileEncoding, stringsAsFactors, strip.white, ...)
}
\arguments{
  \item{file}{the name of -or an open connection to- the configuration file.}
  \item{sheet}{the name of the sheet that is to be read}
  \item{header}{in read.sheet, sets default value to TRUE, as opposed to
    read.table, where it is FALSE.}
  \item{sep}{in read.sheet, sets default value to "\\t", as opposed to
    read.table, where it is " ".}
  \item{fileEncoding}{the encoding of the file to be read.}
  \item{stringsAsFactors}{in read.sheet, sets default value to FALSE, as opposed to
    read.table, where it is TRUE.}
  \item{strip.white}{in read.sheet, sets default value to TRUE, as opposed to
    read.table, where it is FALSE.}
  \item{...}{passed to included functions}
}
