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

EPOCH <- delftfews:::EPOCH

if(FALSE) {
  ## interactive testing...
  require(delftfews)
  setwd("~/Local/r-forge.r-project.org/delftfews/pkg/tests/")
  checkTrue <- function(x) all(x == TRUE)
  checkEquals <- function(x, y) all(x == y)
  checkEqualsNumeric <- function(x, y) all(as.numeric(x) == as.numeric(y))
  checkException <- function(x, ...) x
}

test.read.PI.just.reading <- function() {
  pidata <- read.PI('data/decumulative.input.xml')
  checkTrue(is.zoo(pidata))
}

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

  pidata <- read.PI('data/eventless-f0.xml')
  checkEquals(c(10, 2), dim(pidata))
  checkEquals(c("lp.600-P1201.WNS925", "lp.600-P2504.WNS925"), colnames(pidata))
}

test.read.PI.first.empty.series <- function() {
  ## reader does not crash on first series being eventless

  pidata <- read.PI('data/eventless-0f.xml')
  checkEquals(c(10, 2), dim(pidata))
  checkEquals(c("lp.600P2504.WNS925", "lp.600P1201.WNS925"), colnames(pidata))
}

test.read.PI.all.empty.series <- function() {
  ## reader returns something even if no events are found at all

  pidata <- read.PI('data/eventless-0.xml', step.seconds=1440*60)
  checkEquals(c(31, 1), dim(pidata))
  checkTrue(all(is.na(pidata)))

  checkException(read.PI('data/eventless-0.xml'), ", crashes if you do not specify step.seconds")
}

test.read.PI.timezone.2 <- function() {
  ## input file has a non zero timeZone element.
  pidata <- read.PI('data/read.PI.timezone.2.xml', na.action=na.pass)

  target <- EPOCH + seq(from=1270245600, to=1271023200, by=86400)
  checkEqualsNumeric(target, index(pidata))
}

test.read.PI.is.irregular.TRUE <- function() {
  pidata <- read.PI('data/peilschalen-3.xml', is.irregular=TRUE)

  target <- structure(c(1282644388, 1282645827, 1282719141,
                        1284544972, 1284556921, 1287647956,
                        1287654592, 1287656862, 1289923706,
                        1290006858, 1290007205, 1292332479,
                        1292332731, 1292334625, 1295262137,
                        1295264053, 1295270062, 1297845318,
                        1297856502, 1300283068, 1300285031,
                        1300286755, 1302783719, 1302852032,
                        1305552626, 1305552905, 1308582513,
                        1308583235, 1308734339 ), class =
                      c("POSIXct", "POSIXt"))
  current <- index(pidata)
  checkEqualsNumeric(target, index(pidata))

  target <- structure(c(1282645827, 1287647956, 1289923706,
                        1292334625, 1295264053, 1300285031,
                        1308734339), class = c("POSIXct", "POSIXt"))
  current <- index(pidata[!is.na(pidata[,1])])
  checkEqualsNumeric(target, current)

  target <- structure(c(1282644388, 1284544972, 1287654592,
                        1290006858, 1292332479, 1295262137,
                        1297845318, 1300283068, 1302783719,
                        1305552626, 1308582513), class =
                      c("POSIXct", "POSIXt"))
  current <- index(pidata[!is.na(pidata[,2])])
  checkEqualsNumeric(target, current)

  target <- structure(c(1282719141, 1284556921, 1287656862,
                        1290007205, 1292332731, 1295270062,
                        1297856502, 1300286755, 1302852032,
                        1305552905, 1308583235 ), class =
                      c("POSIXct", "POSIXt"))
  current <- index(pidata[!is.na(pidata[,3])])
  checkEqualsNumeric(target, current)

}

test.read.PI.is.irregular.TRUE.granularity.3600 <- function() {
  pidata <- read.PI('data/peilschalen-3.xml', is.irregular=TRUE, step.seconds=3600)

  target <- structure(c(1282644388, 1282647988, 1282719988, 1284545188,
                        1284559588, 1287648388, 1287655588, 1287659188,
                        1289927188, 1290009988, 1292335588, 1295262388,
                        1295265988, 1295273188, 1297847188, 1297857988,
                        1300284388, 1300287988, 1302786388, 1302854788,
                        1305554788, 1308585988, 1308737188), class = c("POSIXct", "POSIXt"))
  current <- index(pidata)
  dput(current)
  checkEqualsNumeric(target, current)

  checkEqualsNumeric(c(NA, NA, -1.67), pidata[8])
  checkEqualsNumeric(c(0.37, NA, NA), pidata[9])
  checkEqualsNumeric(c(NA, -1.7, -1.79), pidata[10])
}

test.read.PI.is.irregular.TRUE.granularity.3600.base.0 <- function() {
  pidata <- read.PI('data/peilschalen-3.xml', is.irregular=TRUE, base=0, step.seconds=3600)

  target <- structure(c(1282647600, 1282719600, 1284548400,
                        1284559200, 1287648000, 1287655200,
                        1287658800, 1289926800, 1290009600,
                        1292335200, 1295265600, 1295272800,
                        1297846800, 1297857600, 1300284000,
                        1300287600, 1302786000, 1302854400,
                        1305554400, 1308585600, 1308736800), class =
                        c("POSIXct", "POSIXt"))
  current <- index(pidata)
  checkEqualsNumeric(target, current)

  checkEqualsNumeric(c(0.37, NA, NA), pidata[8])
  checkEqualsNumeric(c(NA, -1.7, -1.79), pidata[9])
  checkEqualsNumeric(c(0.37, -1.75, -1.79), pidata[10])
}

test.read.PI.timeZone.4.is.irregular.TRUE <- function() {
  pidata <- read.PI('data/peilschalen-1-timezone-4.xml', is.irregular=TRUE)
  target <- c(1302847200, 1302933600, 1303020000, 1303106400, 1303192800, 1303279200)
  current <- as.seconds(index(pidata))
  checkEquals(target, current)
}

test.read.PI.timeZone.4.is.irregular.FALSE <- function() {
  pidata <- read.PI('data/peilschalen-1-timezone-4.xml')
  target <- c(1302847200, 1302933600, 1303020000, 1303106400, 1303192800, 1303279200)
  current <- as.seconds(index(pidata))
  checkEquals(target, current)
}

test.read.PI.select.on.parameterId <- function() {
  pidata <- read.PI('data/combined-3.xml', is.irregular=TRUE, step.seconds=3600)
  checkEquals(5, ncol(pidata))
  pidata <- read.PI('data/combined-3.xml', parameterId="WNSHDB38", is.irregular=TRUE, step.seconds=3600)
  checkEqualsNumeric(3, ncol(pidata))
}

test.read.PI.select.on.two.parameterId <- function() {
  pidata <- read.PI('data/combined-3.xml', is.irregular=TRUE, step.seconds=3600)
  checkEquals(5, ncol(pidata))
  pidata <- read.PI('data/combined-3.xml', parameterId=c("WNSHDB1", "WNSHDB3"), is.irregular=TRUE, step.seconds=3600)
  checkEqualsNumeric(2, ncol(pidata))
}

test.read.PI.filter.timestamp <- function() {

  peilschalen <- read.PI('data/combined-3.xml', parameterId="WNSHDB38", is.irregular=TRUE, step.seconds=60)

  timestamps <- index(peilschalen)
  passes <- function(candidate) {
    index <- c(which(candidate < timestamps), -1)[[1]]
    if(index == -1) return(FALSE)
    return(as.seconds(difftime(timestamps[[index]], candidate)) < 86400)
  }
  
  pidata <- read.PI('data/combined-3.xml', is.irregular=TRUE, parameterId=c("WNSHDB1", "WNSHDB3"), filter.timestamp=passes, step.seconds=60)

  checkEquals(42, nrow(pidata))
  checkEquals(c(1305459000, 1305477900, 1305481500, 1305482400, 1305485916, 
                1305486900, 1305487716, 1305492300, 1305496800, 1305497700, 1305500400, 
                1305500676, 1305517500, 1305525600, 1305526500, 1305535500, 1305540156, 
                1305549000, 1305549900, 1308496716, 1308504600, 1308505500, 1308507276, 
                1308508200, 1308512700, 1308519000, 1308519900, 1308521796, 1308522600, 
                1308523476, 1308524400, 1308524556, 1308528996, 1308536100, 1308544200, 
                1308545100, 1308550596, 1308559500, 1308564000, 1308564900, 1308572196, 
                1308579300), as.seconds(index(pidata)))
}

test.read.PI.skip.short.lived.60 <- function() {

  ## read twice the same data, select just the second column, then
  ## check the effect of the optional parameter skip.short.lived
  pidata <- read.PI('data/peilschalen-3-with-corrections.xml', is.irregular=TRUE, skip.short.lived=60)
  corrected.peilschaal <- pidata[!is.na(pidata[, 2]), 2]
  pidata <- read.PI('data/peilschalen-3-with-corrections.xml', is.irregular=TRUE)
  uncorrected.peilschaal <- pidata[!is.na(pidata[, 2]), 2]

  ## correction removes timestamps without altering any
  checkTrue(all(index(corrected.peilschaal) %in% index(uncorrected.peilschaal)))
  ## third timestamp gets removed
  checkEquals(c(TRUE, TRUE, FALSE), (index(uncorrected.peilschaal) %in% index(corrected.peilschaal))[1:3])
  ## only one timestamp is removed
  checkEquals(1, length(index(uncorrected.peilschaal)) - length(index(corrected.peilschaal)))
}

test.read.PI.filter.timestamp.small <- function() {

  peilschalen <- read.PI('data/combined-small-3.xml', parameterId="WNSHDB38", is.irregular=TRUE, step.seconds=60)

  timestamps <- index(peilschalen)
  passes <- function(candidate) {
    index <- c(which(candidate < timestamps), -1)[[1]]
    if(index == -1) return(FALSE)
    return(as.seconds(difftime(timestamps[[index]], candidate)) < 86400)
  }
  
  pidata <- read.PI('data/combined-small-3.xml', is.irregular=TRUE, parameterId="WNSHDB1", filter.timestamp=passes, step.seconds=60)

  checkEquals(6, nrow(pidata))
  checkEquals(c(1305459000, 1305492300, 1308521796, 1308523476, 1308544200, 1308579300), as.seconds(index(pidata)))
}

test.read.PI.univariate.as.matrix <- function() {
  pidata <- read.PI('data/peilschalen-1-timezone-4.xml')
  checkEqualsNumeric(2, length(dim(pidata)))
}

test.read.PI.multivariate.as.matrix <- function() {
  pidata <- read.PI('data/peilschalen-3-with-corrections.xml')
  checkEqualsNumeric(2, length(dim(pidata)))
}

test.read.PI.first.series.empty <- function() {
  pidata <- read.PI('data/first-series-empty.xml', is.irregular=TRUE)
  checkEqualsNumeric(c(11, 2), dim(pidata))
  checkTrue(all(is.na(pidata[, 1])))
  checkTrue(!all(is.na(pidata[, 2])))
}

test.read.PI.second.series.empty <- function() {
  pidata <- read.PI('data/second-series-empty.xml', is.irregular=TRUE)
  checkEqualsNumeric(c(11, 2), dim(pidata))
  checkTrue(!all(is.na(pidata[, 1])))
  checkTrue(all(is.na(pidata[, 2])))
}

test.read.PI.first.series.empty.equidistant <- function() {
  pidata <- read.PI('data/first-series-empty-equidistant.xml')
  checkEqualsNumeric(c(11, 3), dim(pidata))
  checkTrue(all(is.na(pidata[, 1])))
  checkTrue(!all(is.na(pidata[, 2])))
  checkTrue(!all(is.na(pidata[, 3])))
}

test.read.PI.first.series.empty.equidistant.with.holes <- function() {
  pidata <- read.PI('data/first-series-empty-equidistant-with-holes.xml', is.irregular=TRUE)
  checkEqualsNumeric(c(10, 3), dim(pidata))
  checkTrue(all(is.na(pidata[, 1])))
  checkEquals(c(2, 4, 9), which(is.na(pidata[, 2])))
  checkEquals(c(7), which(is.na(pidata[, 3])))
}

test.read.PI.empty.equidistant.with.holes <- function() {
  pidata <- read.PI('data/equidistant-with-holes.xml', is.irregular=TRUE)
  checkEqualsNumeric(c(10, 2), dim(pidata))
  checkEquals(c(2, 4, 9), which(is.na(pidata[, 1])))
  checkEquals(c(7), which(is.na(pidata[, 2])))
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

  result <- timeseries(P1201=diff(pidata[, 'lp.600-P1201.WNS954']), order.by=index(pidata)[-1])

  conf$missVal <- NULL
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
                     timeStep=5*60)

  conf$missVal <- NULL # causes empty line

  write.PI(pidata, conf, 'data/test.write.PI.no.events.current')
  expect <- readLines('data/test.write.PI.no.events.target')
  current <- readLines('data/test.write.PI.no.events.current')
  checkEquals(current, expect)
}

test.write.PI.one.event <- function() {
  ## timeseries set with two series, one timestamp only.
  pidata <- timeseries(20576130*60, by=5*60, length.out=1, column1=1.1, column2=1.2)

  conf <- data.frame(column=c('column1', 'column2'), type='instantaneous',
                     locationId=c('P1201', 'P1202'), parameterId='WNS954',
                     timeStep=5*60, startDate=20576130*60, endDate=20576175*60)

  conf$missVal <- NULL # causes empty line

  write.PI(pidata, conf, 'data/test.write.PI.one.event.current')
  expect <- readLines('data/test.write.PI.one.event.target')
  current <- readLines('data/test.write.PI.one.event.current')
  checkEquals(current, expect)
}

test.write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty <- function() {
  ## the data contains in this case, you want only one event
  pidata <- zoo(cbind(a=NA, b=NA), order.by=structure(seq(0,86400,21600), class = c("POSIXct","POSIXt")))
  ts <- index(pidata)

  pidata[ts[3], 'b'] <- Inf

  conf <- data.frame(column=c('a', 'b'), type='instantaneous',
                     locationId='600-P1201', parameterId=c('a', 'b'),
                     timeStep=1440, startDate=20910240, endDate=20931840)

  conf$missVal <- NULL  # removed column, elements will be missing
  conf$InfVal <- -999  # 
  write.PI(pidata, conf, 'data/write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty.current')
  expect <- readLines('data/write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty.target')
  current <- readLines('data/write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty.current')
  checkEquals(current, expect)
}

test.write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty.no.time.indication <- function() {
  ## the data contains in this case, you want only one event
  pidata <- zoo(cbind(a=NA, b=NA), order.by=structure(seq(0,86400,21600), class = c("POSIXct","POSIXt")))
  ts <- index(pidata)

  pidata[ts[3], 'b'] <- Inf

  conf <- data.frame(column=c('a', 'b'), type='instantaneous',
                     locationId='600-P1201', parameterId=c('a', 'b'),
                     timeStep=1440)

  conf$missVal <- NULL  # removed column, elements will be missing
  conf$InfVal <- -999  # 
  write.PI(pidata, conf, 'data/write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty.no.time.indication.current')
  expect <- readLines('data/write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty.no.time.indication.target')
  current <- readLines('data/write.PI.missVal.NA.InfVal.999.content.one.Inf.further.empty.no.time.indication.current')
  checkEquals(current, expect)
}

test.write.PI.no.data.description <- function() {
  ## the data contains in this case, you want only one event
  pidata <- zoo(cbind(lp.locA.par1=1:2, lp.locB.par2=101:102),
                order.by=structure(seq(0,21600,21600), class = c("POSIXct","POSIXt")))
  ts <- index(pidata)

  write.PI(pidata, filename='data/write.PI.no.data.description.current')
  expect <- readLines('data/write.PI.no.data.description.target')
  current <- readLines('data/write.PI.no.data.description.current')
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


test.read.PI.step.seconds.series.have.different.offset <- function() {
  pidata <- read.PI('data/different-offset.xml',
                    step.seconds=3600,
                    na.action=na.pass)

  target <- rep(NA, 10)
  target[4] <- -1.563
  target[5] <- -1.562
  current <- as.numeric(pidata[, 1])
  checkEquals(target, current)
}
