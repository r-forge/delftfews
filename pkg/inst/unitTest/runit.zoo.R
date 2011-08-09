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

EPOCH <- delftfews:::EPOCH

`test.$<-.zoo.respects.derived.classes` <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  orig.class <- class(FWS) <- c('some.other.class', class(FWS))
  FWS$a <- 4:7
  checkEquals(orig.class, class(FWS))
}

`test.$.zoo.respects.derived.classes` <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  orig.class <- class(FWS) <- c('some.other.class', class(FWS))
  checkEquals(class(FWS), class(FWS$a))
}

`test.$.zoo.does.not.drop.dimensions` <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  target.a <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, a=1)
  target.b <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, b=3)
  checkEquals(target.a, FWS$a)
  checkEquals(target.b, FWS$b)
}

`test.[.zoo.does.not.drop.dimensions` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=1:4)
  target.a <- zoo(cbind(a=1), order.by=1:4)
  target.b <- zoo(cbind(b=3), order.by=1:4)
  checkEquals(target.a, FWS[, 'a'])
  checkEquals(target.b, FWS[, 'b'])
}

`test.[.zoo.respects.derived.classes` <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  class(FWS) <- c('some.other.class', class(FWS))
  checkEquals(class(FWS), class(FWS[, 'a']))
}

test.Ops.zoo.keeps.class.numeric <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=4:7)
  class(FWS) <- c('some.other.class', class(FWS))
  FWSa <- FWS$a
  checkEquals(class(FWS), class(FWSa - 0))
  checkEquals(class(FWS), class(FWSa + 0))
  checkEquals(class(FWS), class(FWSa * 1))
  checkEquals(class(FWS), class(FWSa / 1))
}

test.Ops.zoo.keeps.class.logic <- function() {
  DEACTIVATED("waiting for the zoo group to fix a bug in Ops.zoo")
  FWS <- zoo(cbind(a=1, b=3), order.by=1:4)
  class(FWS) <- c('some.other.class', class(FWS))
  FWSa <- FWS$a
  checkEquals(class(FWS), class(FWSa > 0))
}

`test.[.zoo.first.parameter.univariate.logic` <- function() {
  FWS <- zoo(cbind(a=1, b=1:4), order.by=1:4)
  target <- FWS[as.vector(FWS[, 'b', drop=TRUE] < 3)]
  current <- FWS[FWS[, 'b', drop=TRUE] < 3]
  checkEquals(target, current)
}

`test.[.zoo.first.parameter.multivariate.logic.one.column` <- function() {
  DEACTIVATED("waiting for the zoo group to define what is the correct behaviour")
  FWS <- zoo(cbind(a=1, b=1:4), order.by=1:4)
  target <- FWS[as.vector(FWS[, 'b', drop=FALSE] < 3)]
  current <- FWS[FWS[, 'b', drop=FALSE] < 3]
  checkEquals(target, current)
}

test.rollapply <- function() {
  input <- c(rep(1:3, 2), NA, 4, 4, 4)
  ##  [1]  1  2  3  1  2  3 NA  4  4  4

  result <- rollapply(input, 2, sum)
  expect <- c(3, 5, 4, 3, 5, NA, NA, 8, 8)
  checkEquals(expect, result)

  result <- rollapply(input, 2, sum, fill=NA)
  expect <- c(3, 5, 4, 3, 5, NA, NA, 8, 8, NA)
  checkEquals(expect, result)

  result <- rollapply(input, 2, sum, fill=NA, align='right')
  expect <- c(NA, 3, 5, 4, 3, 5, NA, NA, 8, 8)
  checkEquals(expect, result)

  result <- rollapply(na.zero(input), 2, sum, fill=NA, align='right')
  expect <- c(NA, 3, 5, 4, 3, 5, 3, 4, 8, 8)
  checkEquals(expect, result)

  result <- rollapply(na.zero(input), 4, sum, fill=NA, align='right')
  expect <- c(NA, NA, NA, 7, 8, 9, 6, 9, 11, 12)
  checkEquals(expect, result)
  
  result <- rollapply(na.zero(input), 4, max, fill=NA, align='right')
  expect <- c(NA, NA, NA, 3, 3, 3, 3, 4, 4, 4)
  checkEquals(expect, result)
  
  result <- rollapply(na.zero(input), 4, mean, fill=NA, align='right')
  expect <- c(NA, NA, NA, 1.75, 2.00, 2.25, 1.50, 2.25, 2.75, 3.00)
  checkEquals(expect, result)
  
  result <- rollapply(na.zero(input), 2, min, fill=NA, align='right')
  expect <- c(NA, 1, 2, 1, 1, 2, 0, 0, 4, 4)
  checkEquals(expect, result)
  
}

test.rollapply.keeps.tzone.unidimensional <- function() {
  input <- zoo(1:9, order.by=structure(seq(0, by=60, length=9), class = c("POSIXct", "POSIXt"), tzone="UTC"))
  result <- rollapply(input, 5, sum)
  checkEquals("UTC", attr(index(result), 'tzone'))
}

test.rollapply.keeps.tzone.bidimensional <- function() {
  DEACTIVATED("waiting for the zoo group to fix a bug in rollapply")
  input <- zoo(cbind(a=1:9), order.by=structure(seq(0, by=60, length=9), class = c("POSIXct", "POSIXt"), tzone="UTC"))
  result <- rollapply(input, 5, sum)
  checkEquals("UTC", attr(index(result), 'tzone'))
}
