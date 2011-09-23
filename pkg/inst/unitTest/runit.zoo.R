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
  DEACTIVATED("reported to the zoo group, worked around.")
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
  DEACTIVATED("reported to the zoo group, worked around.")
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
  DEACTIVATED("reported to the zoo group, worked around.")
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
  DEACTIVATED("reported to the zoo group, worked around.")
  input <- zoo(cbind(a=1:9), order.by=structure(seq(0, by=60, length=9), class = c("POSIXct", "POSIXt"), tzone="UTC"))
  result <- rollapply(input, 5, sum)
  checkEquals("UTC", attr(index(result), 'tzone'))
}

test.as.matrix.6x0.equals.to.self <- function() {
  zoo6x0 <- zoo(order.by=0:5)
  checkEquals(zoo6x0, zoo6x0)
}

test.as.matrix.6x0.distinct.from.0x0 <- function() {
  zoo6x0 <- zoo(order.by=0:5)
  zoo0x0 <- zoo()
  checkEquals(zoo0x0, zoo0x0)
  checkEquals(FALSE, isTRUE(all.equal(zoo0x0, zoo6x0)))
}

test.as.matrix.zoo.zero.by.zero <- function() {
  DEACTIVATED("reported to the zoo group, worked around.")
  checkEquals(c(0, 0), dim(as.matrix(zoo())))
}

test.as.matrix.zoo.zero.columns <- function() {
  checkEquals(c(3, 0), dim(as.matrix(zoo(order.by=1:3))))
  checkEquals(c(2, 0), dim(as.matrix(zoo(order.by=1:2))))
  checkEquals(c(1, 0), dim(as.matrix(zoo(order.by=1))))
}

test.as.matrix.zoo.zero.rows <- function() {
  checkEquals(c(0, 1), dim(as.matrix(zoo(cbind(a=1),
order.by=numeric(0)))))
  checkEquals(c(0, 2), dim(as.matrix(zoo(cbind(a=1, b=2),
order.by=numeric(0)))))
  checkEquals(c(0, 3), dim(as.matrix(zoo(cbind(a=1, b=2, c=3),
order.by=numeric(0)))))
}

test.timeseries.zoo.equivalent <- function() {
  FWS1 <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  FWS2 <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH), frequency=frequency(FWS1))
  checkEquals(FWS1, FWS2)
}

## selecting complete rows and columns.

`test.[.zoo.by.column` <- function() {
  DEACTIVATED("selecting only one column retrieves an univariate series, unless you specify drop=FALSE.")
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  checkEquals(FWS[,'a', drop=FALSE], FWS[, 'a'])
  checkEquals(FWS[,'b', drop=FALSE], FWS[, 'b'])
  checkEquals(FWS[,c('a', 'b'), drop=FALSE], FWS[, c('a', 'b')])
}

`test.[.zoo.by.row.numeric` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  checkEquals(FWS[2], FWS[2.0])
  checkEquals(FWS[2], FWS[2L])
}

`test.[.zoo.by.row.timestamp` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  checkEquals(FWS[1, 1:2], FWS[as.POSIXct(1234567800, origin=EPOCH)])
  checkEquals(FWS[2, 1:2], FWS[as.POSIXct(1234567800 + 57600*60, origin=EPOCH)])
}

`test.$.zoo.non.existing` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  checkTrue(is.null(FWS$c))
}

`test.[.zoo.non.existing` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  checkTrue(is.null(FWS[, 'c']))
}

`test.[<-.zoo.by.column.redefine` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS[, 'a'] <- 5:8
  checkEqualsNumeric(5:8, FWS[, 'a'])
}

`test.[<-.zoo.by.column.new` <- function() {
  DEACTIVATED("waiting for the zoo group to fix a bug in [<-.zoo")
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS[, 'd'] <- 5:8
  checkEqualsNumeric(5:8, FWS[, 'd'])
}

`test.[<-.zoo.character.adding.named.column` <- function() {
  DEACTIVATED("waiting for the zoo group to correct a bug in [<-.zoo")
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS[, 'd'] <- FWS$a
  checkEqualsNumeric(rep(1, 4), FWS[, 'd'])
}

`test.$<-.zoo.redefining` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS$a <- 4:7
  checkEqualsNumeric(4:7, FWS[, 'a'])
}

`test.$<-.zoo.keeps.other.names` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  colnames(FWS) <- c('ab-c','d,e,f')
  FWS$a <- 4:7
  checkEquals(c('ab-c','d,e,f', 'a'), colnames(FWS))
}

`test.[<-.zoo.keeps.other.names` <- function() {
  DEACTIVATED("waiting for the zoo group to correct a bug in [<-.zoo")
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  colnames(FWS) <- c('ab-c','d,e,f')
  FWS[, 'a'] <- 4:7
  checkEquals(c('ab-c','d,e,f', 'a'), colnames(FWS))
}

`test.$<-.zoo.adding.to.existing` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS$d <- 4:7
  checkEqualsNumeric(4:7, FWS[, 'd'])
}

`test.$<-.zoo.adding.to.empty` <- function() {
  FWS <- zoo(order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS$d <- 4:7
  checkEqualsNumeric(4:7, FWS[, 'd'])
}

`test.$<-.zoo.adding.to.single.column` <- function() {
  FWS <- zoo(cbind(a=1), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS$d <- 4:7
  checkEqualsNumeric(4:7, FWS[, 'd'])
}

`test.$<-.zoo.character.adding.named.column` <- function() {
  FWS <- zoo(cbind(a=1, b=3), order.by=as.POSIXct(seq(1234567800, by=57600*60, length.out=4), origin=EPOCH))
  FWS$d <- FWS$a
  checkEqualsNumeric(rep(1, 4), FWS[, 'd'])
}
