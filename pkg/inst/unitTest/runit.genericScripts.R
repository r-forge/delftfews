##
## $Id: runit.genericScripts.R 24939 2011-10-28 10:14:55Z mario.frasca $
##

require(svUnit)

test.fileName <- file.path(tempdir(), c('1', '2', '3', '4', '5'))

.setUp <- function() {
  cat('<root><globals><abc>7</abc><cde>17</cde></globals></root>',
      file=test.fileName[1])
  cat('<root><globals><abc type="integer">7</abc><cde type="real">17</cde></globals></root>',
      file=test.fileName[2])
  cat('<root><globals><abc type="integer">7</abc><abc>8</abc><abc>9</abc><cde type="real">17</cde></globals></root>',
      file=test.fileName[3])
  cat('<root><testscript id="1"><abc type="integer">7</abc><abc>8</abc><abc>9</abc><cde type="real">17</cde></testscript><testscript id="2"><abc type="integer">70</abc><abc>80</abc><abc>90</abc><cde type="real">170</cde></testscript></root>',
      file=test.fileName[4])
  cat('<root> <energycontrol> <starthighrate type="numeric">7</starthighrate> <startlowrate type="numeric">23</startlowrate> <reductionvalue type="numeric">-0.01</reductionvalue> </energycontrol> </root>',
      file=test.fileName[5])
}

.tearDown <- function() {
  file.remove(test.fileName)
}

test.blendInGlobals.untyped <- function() {
  ec <- XmlDoc$new(test.fileName[1])
  te <- new.env()
  blendInGlobals(xmldoc=ec, envir=te)
  checkEquals(c("abc", "cde"), ls(te))
  checkIdentical("7", get("abc", te))
  checkIdentical("17", get("cde", te))
}

test.blendInGlobals.typed.atomic <- function() {
  ec <- XmlDoc$new(test.fileName[2])
  te <- new.env()
  blendInGlobals(xmldoc=ec, envir=te)
  checkEquals(c("abc", "cde"), ls(te))
  checkIdentical(7L, get("abc", te))
  checkIdentical(17.0, get("cde", te))
}

test.blendInGlobals.typed.vector <- function() {
  ec <- XmlDoc$new(test.fileName[3])
  te <- new.env()
  blendInGlobals(xmldoc=ec, envir=te)
  checkEquals(c("abc", "cde"), ls(te))
  checkIdentical(c(7L, 8L, 9L), get("abc", te))
  checkIdentical(17.0, get("cde", te))
}

test.blendInGlobals.no.globals <- function() {
  ec <- XmlDoc$new(test.fileName[5])
  te <- new.env()
  previously <- ls(te)
  blendInGlobals(xmldoc=ec, envir=te)
  checkEquals(previously, ls(te))
}

test.blendInGlobals.named.globals <- function() {
  ec <- XmlDoc$new(test.fileName[5])
  te <- new.env()
  blendInGlobals(xmldoc=ec, envir=te, element="/root/energycontrol")
  target <- c("reductionvalue", "starthighrate", "startlowrate")
  checkEquals(target, ls(te))
  checkEquals(-0.01, get("reductionvalue", te))
  checkEquals(7, get("starthighrate", te))
  checkEquals(23, get("startlowrate", te))
}

test.blendInGlobals.named.globals.by.id <- function() {
  ec <- XmlDoc$new(test.fileName[4])
  te <- new.env()
  blendInGlobals(xmldoc=ec, envir=te, element="/root/testscript[@id='%s']", key=1)
  checkEquals(c("abc", "cde"), ls(te))
  checkIdentical(c(7L, 8L, 9L), get("abc", te))
  checkIdentical(17.0, get("cde", te))
  blendInGlobals(xmldoc=ec, envir=te, element="/root/testscript[@id='%s']", key=2)
  checkEquals(c("abc", "cde"), ls(te))
  checkIdentical(c(70L, 80L, 90L), get("abc", te))
  checkIdentical(170.0, get("cde", te))
}

## getContainerDir is not exported
getContainerDir <- delftfews:::getContainerDir

test.getContainerDir.backslash <- function() {
  current <- getContainerDir("/usr/bin/bash")
  target <- "bin"
  checkEquals(target, current)
}

test.getContainerDir.forwardslash <- function() {
  current <- getContainerDir("C:\\Program Files\\R\\R-2.12.2\\bin\\R.exe")
  target <- "bin"
  checkEquals(target, current)
}

test.getContainerDir.directory <- function() {
  current <- getContainerDir("/usr/local/bin/")
  target <- "local"
  checkEquals(target, current)
}
