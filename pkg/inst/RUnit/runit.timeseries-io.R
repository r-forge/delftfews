##***********************************************************************
## $Id$
##
## this file is part of the R library delftfews.  delftfews is free
## software: you can redistribute it and/or modify it under the terms
## of the GNU General Public License as published by the Free Software
## Foundation, either version 3 of the License, or (at your option)
## any later version.
##
## delftfews is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with the nens libraray.  If not, see
## <http://www.gnu.org/licenses/>.
##

require(svUnit)

## testing the following non-exported functions:
splitToNumeric <- delftfews:::splitToNumeric
parseSplitDcf <- delftfews:::parseSplitDcf

EPOCH <- delftfews:::EPOCH

test.read.PI.just.reading <- function() {
  pidata <- read.PI('data/decumulative.input.xml')
  checkEquals("delftfews", class(pidata)[1])
}

## test.computing.decumulative <- function() {
##   DEACTIVATED("this is not a unit test, it's a usage example.")
##   pidata <- read.PI('data/decumulative.input.xml')

##   current <- data.frame(timestamps=index(pidata)[-1]) # drop first timestamp
##   current$P1201 <- diff(pidata[, 'lp.600-P1201.WNS954'])
##   current$P1202 <- diff(pidata[, 'lp.600-P1202.WNS954'])
##   current$P1203 <- diff(pidata[, 'lp.600-P1203.WNS954'])

##   pidata.out <- read.PI('data/decumulative.output.xml')

##   checkEquals(index(pidata.out), index(current))
##   checkEquals(pidata.out[, 'lp.600-P1201.WNS954.omgezet'], current$P1201)
##   checkEquals(pidata.out[, 'lp.600-P1202.WNS954.omgezet'], current$P1202)
##   checkEquals(pidata.out[, 'lp.600-P1203.WNS954.omgezet'], current$P1203)
## }

test.read.PI.na.pass.base <- function() {
  ## value is not at all present for timestamp.
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  checkEquals(FALSE, is.na(pidata[2, 'lp.600-P1201.WNS954', drop=TRUE]))
  checkEquals(TRUE, is.na(pidata[3, 'lp.600-P1201.WNS954', drop=TRUE]))
}

test.read.PI.na.pass.missVal <- function() {
  ## value for timestamp is the fictive missing value.
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  checkEquals(FALSE, is.na(pidata[2, 'lp.600-P1203.WNS954', drop=TRUE]))
  checkEquals(TRUE, is.na(pidata[3, 'lp.600-P1202.WNS954', drop=TRUE]))
}

test.read.PI.na.pass.flag9 <- function() {
  ## value for timestamp must be discarded (flag is 9)
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  checkEquals(FALSE, is.na(pidata[2, 'lp.600-P1203.WNS954', drop=TRUE]))
  checkEquals(TRUE, is.na(pidata[3, 'lp.600-P1203.WNS954', drop=TRUE]))
}

test.read.PI.one.empty.series <- function() {
  ## reader does not crash on one eventless series

  read.PI('data/eventless-f0.xml')
}

test.read.PI.first.empty.series <- function() {
  ## reader does not crash on first series being eventless

  read.PI('data/eventless-0f.xml')
}

test.read.PI.all.empty.series <- function() {
  ## reader returns something even if no events are found at all

  read.PI('data/eventless-0.xml', step.seconds=1440*60)

  checkException(read.PI('data/eventless-0.xml'), ", crashes if you do not specify step.seconds")
}

test.read.PI.timezone.2 <- function() {
  ## input file has a non zero timeZone element.
  pidata <- read.PI('data/read.PI.timezone.2.xml', na.action=na.pass)

  target <- EPOCH + seq(from=1270245600, to=1271023200, by=86400)
  checkEqualsNumeric(target, index(pidata))
}

test.write.PI.na.missing.elements <- function() {
  ## if no missVal is given: skip the element
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  conf <- data.frame(column='P1201', type='instantaneous',
                     locationId='600-P1201', parameterId='WNS954-differences',
                     timeStep=1440, startDate=20910240, endDate=20931840)

  result <- timeseries(P1201=diff(pidata[, 'lp.600-P1201.WNS954']), order.by=index(pidata)[-1])

  conf$missVal <- NULL # removed column, elements will be missing
  write.PI(result, conf, 'data/write.PI.na.missing.elements.current')
  expect <- readLines('data/write.PI.na.missing.elements.target')
  current <- readLines('data/write.PI.na.missing.elements.current')
  checkEquals(current, expect)
}

test.write.PI.na.NULL.elements <- function() {
  ## if missVal is NULL (the string): skip the element
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  conf <- data.frame(column='P1201', type='instantaneous',
                     locationId='600-P1201', parameterId='WNS954-differences',
                     timeStep=1440, startDate=20910240, endDate=20931840)

  result <- timeseries(P1201=diff(pidata['lp.600-P1201.WNS954']), order.by=index(pidata)[-1])

  conf$missVal <- "NULL"
  write.PI(result, conf, 'data/test.write.PI.na.1.xml.current')
  expect <- readLines('data/test.write.PI.na.1.xml.target')
  current <- readLines('data/test.write.PI.na.1.xml.current')
  checkEquals(current, expect)
}

test.write.PI.na.value.default <- function() {
  ## if missVal is given and not NA: use it and flag it 8
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  conf <- data.frame(column='P1201', type='instantaneous',
                     locationId='600-P1201', parameterId='WNS954-differences',
                     timeStep=1440, startDate=20910240, endDate=20931840)

  result <- timeseries(order.by=index(pidata)[-1], P1201=diff(pidata[, 'lp.600-P1201.WNS954']))

  conf$missVal <- -999.0 # set missing value, expect this value
  write.PI(result, conf, 'data/test.write.PI.na.2.xml.current')
  expect <- readLines('data/test.write.PI.na.2.xml.target')
  current <- readLines('data/test.write.PI.na.2.xml.current')
  checkEquals(current, expect)
}

test.write.PI.na.value.data.frame <- function() {
  ## same test as above, but writing a data.frame instead of a zoo
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  conf <- data.frame(column='P1201', type='instantaneous',
                     locationId='600-P1201', parameterId='WNS954-differences',
                     timeStep=1440, startDate=20910240, endDate=20931840)

  result <- data.frame(timestamps=index(pidata)[-1]) # drop first timestamp
  result$P1201 <- diff(pidata[, 'lp.600-P1201.WNS954'])

  conf$missVal <- -999.0 # set missing value, expect this value
  write.PI(result, conf, 'data/test.write.PI.na.2.xml.current')
  expect <- readLines('data/test.write.PI.na.2.xml.target')
  current <- readLines('data/test.write.PI.na.2.xml.current')
  checkEquals(current, expect)
}

test.write.PI.na.flag.9 <- function() {
  ## if missVal is NA: use zero and flag it 9
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  conf <- data.frame(column='P1201', type='instantaneous',
                     locationId='600-P1201', parameterId='WNS954-differences',
                     timeStep=1440, startDate=20910240, endDate=20931840)

  result <- timeseries(order.by=index(pidata)[-1], P1201=diff(pidata[, 'lp.600-P1201.WNS954']))

  conf$missVal <- NA # causes flag="9"
  write.PI(result, conf, 'data/test.write.PI.na.3.xml.current')
  expect <- readLines('data/test.write.PI.na.3.xml.target')
  current <- readLines('data/test.write.PI.na.3.xml.current')
  checkEquals(current, expect)
}

test.write.PI.na.threecolumns <- function() {
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  conf <- data.frame(column=c('P1201', 'P1202', 'P1203'))
  conf$type <- 'instantaneous'
  conf$locationId <- c('600-P1201', '600-P1202', '600-P1203')
  conf$parameterId <- 'parameterId'
  conf$units <- '-'
  conf$missVal <- -999
  conf$stationName <- '-'
  conf$longName <- '-'

  result <- timeseries(order.by=index(pidata)[-1], P1201=NA, P1202=NA, P1203=NA)
  
  write.PI(result, conf, 'data/test.write.PI.na.4.xml.current')
  expect <- readLines('data/test.write.PI.na.4.xml.target')
  current <- readLines('data/test.write.PI.na.4.xml.current')
  checkEquals(current, expect)
}

test.write.PI.na.nan <- function() {
  ## treat the two special values Inf and NA differently
  column <- c(NA, 25, NA, NA, NA, NA, Inf, NA, NA, NA)
  pidata <- timeseries(20576130*60, by=5*60, length.out=10, column=column)

  conf <- data.frame(column='column', type='instantaneous',
                     locationId='600-P1201', parameterId='WNS954-differences',
                     timeStep=5*60, startDate=20576130*60, endDate=20576175*60)

  conf$missVal <- NULL # causes empty line
  conf$InfVal <- NA # causes a 9-flagged 0 at Inf

  write.PI(pidata, conf, 'data/test.write.PI.nan.na.xml.current')
  expect <- readLines('data/test.write.PI.nan.na.xml.target')
  current <- readLines('data/test.write.PI.nan.na.xml.current')
  checkEquals(current, expect)
}

test.write.PI.missing.columns <- function() {
  ## if column is configured but not there, please do not crash
  pidata <- read.PI('data/decumulative.input.NA.xml', na.action=na.pass)

  conf <- data.frame(column=c('lp.600-P1201.WNS954', 'nonce'), type='instantaneous',
                     locationId=c('600-P1201', '720-P1234'), parameterId='WNS954',
                     timeStep=1440*60, startDate=20910240*60, endDate=20931840*60)

  write.PI(pidata, conf, 'data/write.PI.missing.columns.current')
  expect <- readLines('data/write.PI.missing.columns.target')
  current <- readLines('data/write.PI.missing.columns.current')
  checkEquals(current, expect)
}

test.write.PI.no.events <- function() {
  ## timeseries set with two series, one totally empty.
  pidata <- timeseries(20576130*60, by=5*60, length.out=10, column1=1, column2=NA)

  conf <- data.frame(column=c('column1', 'column2'), type='instantaneous',
                     locationId=c('P1201', 'P1202'), parameterId='WNS954',
                     timeStep=5*60, startDate=20576130*60, endDate=20576175*60)

  conf$missVal <- NULL # causes empty line

  write.PI(pidata, conf, 'data/test.write.PI.no.events.current')
  expect <- readLines('data/test.write.PI.no.events.target')
  current <- readLines('data/test.write.PI.no.events.current')
  checkEquals(current, expect)
}

test.splitToNumeric <- function() {
  input.m <- matrix(c("nat", "-0.38", "-0.40", "300.0",
                      "droog", "-0.42", "-0.44", "300.0"),
                    ncol=4, byrow=TRUE)
  colnames(input.m) <- c("scenario", "aanslagpeil", "afslagpeil", "gemaalcapaciteit")

  target.m <- data.frame(aanslagpeil=c(-0.38, -0.42),
                         afslagpeil=c(-0.40, -0.44),
                         gemaalcapaciteit=c(300.0, 300.0))
  rownames(target.m) <- c('nat', 'droog')

  current.m <- splitToNumeric(input.m)
  
  checkEquals(target.m, current.m)
}

test.splitToNumeric.small <- function() {
  input.m <- matrix(c("nat", "-0.38", "-0.40", "300.0"),
                    ncol=4, byrow=TRUE)
  colnames(input.m) <- c("scenario", "aanslagpeil", "afslagpeil", "gemaalcapaciteit")

  target.m <- data.frame(aanslagpeil=c(-0.38),
                         afslagpeil=c(-0.40),
                         gemaalcapaciteit=c(300.0))
  rownames(target.m) <- c('nat')

  current.m <- splitToNumeric(input.m)
  
  checkEquals(target.m, current.m)
}

test.parseSplitDcf <- function() {
  input.m <- matrix(c("overstortput, by Lizard Scripter", "Lizard General Adapter Functions", NA, NA, NA, NA,
                    NA, NA, "nat", "-0.38", "-0.40", "300.0",
                    NA, NA, "droog", "-0.42", "-0.44", "300.0"
                    ),
                  ncol=6, byrow=TRUE)
  colnames(input.m) <- c("fileDescription", "sourceSystem", "scenario", "aanslagpeil", "afslagpeil", "gemaalcapaciteit")

  target.m <- list(data.frame(fileDescription="overstortput, by Lizard Scripter",
                              sourceSystem="Lizard General Adapter Functions", stringsAsFactors=FALSE),
                   data.frame(aanslagpeil=c(-0.38, -0.42),
                              afslagpeil=c(-0.40, -0.44),
                              gemaalcapaciteit=c(300.0, 300.0)))
  names(target.m) <- c("fileDescription", "scenario")
  rownames(target.m[[2]]) <- c("nat", "droog")
  
  current.m <- parseSplitDcf(input.m)
  checkEquals(length(target.m), length(current.m))
  checkEquals(target.m[[1]], current.m[[1]])
  checkEquals(target.m[[2]], current.m[[2]])
}
