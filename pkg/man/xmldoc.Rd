% File man/XmlDoc.Rd

\name{XmlDoc}
\alias{XmlDoc}
\alias{\S4method{new}{XmlDoc}}
\alias{\S4method{getText}{XmlDoc}}
\alias{\S4method{getAttribute}{XmlDoc}}
\title{creates an 'XmlDoc' object represenging an xml file.}
\description{
  an 'XmlDoc' object enables access to the text and attributes of xml
  documents.  it is not much more than syntactical sugar on top of the
  XML library, but it allows you forget about closing the xml document
  and makes accessing groups of elements a lot easier.

  besides the constructor $new, there are two exposed methods,
  getAttribute and getText.  both return character values, if you want
  to convert data to numeric, you have to invoke as.numeric yourself.

  both getAttribute and getText first build a XMLNodeSet object based on
  the element and ... arguments.  getAttribute also expects an attr
  argument, which specifies the desired attribute or attributes.

  both getAttribute and getText return a character vector.  only
  getAttribute may return a data.frame, if more than one attribute is
  requested. 
  
}
\usage{
  \S4method{new}{XmlDoc}(fileName)
  \S4method{getText}{XmlDoc}(element, \dots, children)
  \S4method{getAttribute}{XmlDoc}(attr, element, \dots)
}
\arguments{
  \item{fileName}{the file containing the xml code.}
  \item{element}{the path to the element. may contain directives for
    filtering on attribute values.}
  \item{\dots}{the optional attribute values for the filtering directives.}
  \item{attr}{the attribute or attributes to select.}
  \item{children}{character vector: the relative paths of the names of
    the elements whose text to return.  if specified, the return value
    is a matrix instead of a vector.  it is mostly useful if the
    contained elements are unique: sequences are pasted without glue.}
  }
\examples{

  tempFileName <- paste(tempdir(), 'test.xml', sep='/')
  cat('<root><element><sub id="1" ktb="7" qlm="a">7</sub>
    <sub id="2" ktb="23" qlm="b">23</sub></element></root>',
    file=file.path(tempFileName))

  doc <- XmlDoc$new(tempFileName)
  doc$getAttribute("id", "/root/element/sub")
  doc$getText("/root/element/sub")

  doc$getText("/root/element/sub[@id=\%d]", 2)

  doc$getAttribute(c("id", "ktb", "qlm"), "/root/element/sub")

  doc$getText("/root/element", children="sub")

  file.remove(tempFileName)
}
