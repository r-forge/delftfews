% File src/R/man/genericScripts.Rd

\name{gaCommonInitializationSteps}
\alias{gaCommonInitializationSteps}
\alias{blendInGlobals}
\title{gaCommonInitializationSteps}
\description{
  functions that initialize the global environment for GA-scripts.

  `gaCommonInitializationSteps` uses the content of the GA definition
  file to add some necessary variables to the global environment.  these
  include for example `inputFileName`, `outputFileName`, the names of
  the input and output columns, the logging object.

  `blendInGlobals` does a similar thing with the extra configuration
  file.  it reads the /root/globals/NAME elements and puts their textual
  value into the corresponding NAME global variable.  if a `type`
  attribute is present, the character value is converted as the
  specified type.
  
}
\usage{

gaCommonInitializationSteps(gaDefFile = commandArgs(trailingOnly =
TRUE)[[1]], gaExtraConfFile = ifelse(length(commandArgs(trailingOnly =
TRUE)) > 1, commandArgs(trailingOnly = TRUE)[[2]], NA))

}
\arguments{
  \item{gaDefFile}{character: the name of the file defining the general
    adapter.  it's optional and if missing, the first command line
    argument will be used.}
  
  \item{gaExtraConfFile}{character: the name of the file containing
    extra configuration items, that do not fit in the general adapter
    definition file.  it's optional, defaults to the second command line
    argument or is simply skipped.}

}
\details{

  the easiest way to use the function `gaCommonInitializationSteps` is
  by invoking it, without arguments, as the first action in an R script.

  for testing, debugging and interactive usage, you would invoke it with
  explicit arguments.
  
  invoked without arguments, `gaCommonInitializationSteps` will use the
  first two arguments on the command line which run the script.

  the first argument is interpreted as the location of the xml that
  defines the general adapter.  

  the information read is then placed in the global environment so your
  script can access it easily.  the names of the parameters being
  exported are hard coded and closely mimics the names of the xml
  elements containing the values, with the important difference that we
  look at the data from the receiving end.  the objects exported by FEWS
  are input to our script and what FEWS hopes to import is for our
  script the output.  this difference is reflected in the names of the
  globals.

  if there's a second argument, `gaCommonInitializationSteps` will use
  it to initialize an XmlDoc object and store it globally as
  `extraConf`.  no assumption is made on the structure of this xml file,
  except that it must be parseable.

  apart from data extracted from the FEWS GA configuration file,
  `gaCommonInitializationSteps` defines the global variable `gaConfDir`
  as the name of the directory directly containing the FEWS GA
  configuration file.  The rationale behind this is that you can use it
  to select (using XPath) elements in a common extraConf object.

}
\examples{
\dontrun{gaCommonInitializationSteps() ## gets arguments from command line} 
\dontrun{gaCommonInitializationSteps("ga_conf.xml", "metadata.xml")}
}