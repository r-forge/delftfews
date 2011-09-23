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

test.timestamp.in.range <- function() {
  pidata <- timeseries(as.POSIXct(1263553200, origin=EPOCH), by=15*60, length.out=289, H.gewogen=-1.5)
  target <- c(55L, 151L, 247L)
  current <- which(timestamp.in.range(pidata, from=1263601800, to=1263610801, by=86400, offset=0, units='secs'))
  checkEquals(target, current)
  target <- c(7L, 55L, 103L, 151L, 199L, 247L)
  current <- which(timestamp.in.range(pidata, from=1263601800, to=1263610801, by=86400/2, offset=0, units='secs'))
  checkEquals(target, current)
}

test.timestamp.in.range.weekday.workday <- function() {
  pidata <- timeseries(as.POSIXct(1263553200, origin=EPOCH), by=15*60, length.out=289, H.gewogen=-1.5)

  in.workweek <- timestamp.in.range.weekday(pidata, tz="UTC", from=1, to=6)
  target <- rep(TRUE, 289)
  target[53:244] <- FALSE
  checkEquals(target, in.workweek)
}

test.timestamp.in.range.weekday.saturday <- function() {
  pidata <- timeseries(as.POSIXct(1263553200, origin=EPOCH), by=15*60, length.out=289, H.gewogen=-1.5)

  on.saturday <- timestamp.in.range.weekday(pidata, tz="UTC", from=6, to=7)
  target <- rep(FALSE, 289)
  target[53:148] <- TRUE
  checkEquals(target, on.saturday)
}

test.timestamp.in.weekend <- function() {
  pidata <- timeseries(as.POSIXct(1263553200, origin=EPOCH), by=15*60, length.out=289, H.gewogen=-1.5)
  ##              timestamps H.gewogen in.weekend
  ## 1   2010-01-15 11:00:00      -1.5      FALSE
  ## ...
  ## 52  2010-01-15 23:45:00      -1.5      FALSE
  ## 53  2010-01-16 00:00:00      -1.5       TRUE
  ## ...
  ## 244 2010-01-17 23:45:00      -1.5       TRUE
  ## 245 2010-01-18 00:00:00      -1.5      FALSE
  ## ...
  ## 289 2010-01-18 11:00:00      -1.5      FALSE

  in.weekend <- timestamp.in.weekend(pidata, tz="UTC")
  target <- rep(FALSE, 289)
  target[53:244] <- TRUE
  checkEquals(target, in.weekend)
}

test.timestamp.in.weekend.localtime.winter <- function() {
  pidata <- timeseries(as.POSIXct(1263553200, origin=EPOCH), by=15*60, length.out=289, H.gewogen=-1.5)
  ##              timestamps H.gewogen in.weekend
  ## 1   2010-01-15 11:00:00      -1.5      FALSE
  ## ...
  ## 48  2010-05-14 22:45:00      -1.5      FALSE
  ## 49  2010-05-14 23:00:00      -1.5       TRUE
  ## ...
  ## 240 2010-05-16 22:45:00      -1.5       TRUE
  ## 241 2010-05-16 23:00:00      -1.5      FALSE
  ## ...
  ## 289 2010-01-18 11:00:00      -1.5      FALSE

  in.weekend <- timestamp.in.weekend(pidata)
  target <- rep(FALSE, 289)
  target[49:240] <- TRUE
  checkEquals(target, in.weekend)
}

test.timestamp.in.weekend.localtime.summer <- function() {
  pidata <- timeseries(as.POSIXct(1273834800, origin=EPOCH), by=15*60, length.out=289, H.gewogen=-1.5)
  ##              timestamps H.gewogen in.weekend
  ## 1   2010-05-14 11:00:00      -1.5      FALSE
  ## ...
  ## 44  2010-05-14 21:45:00      -1.5      FALSE
  ## 45  2010-05-14 22:00:00      -1.5       TRUE
  ## ...
  ## 236 2010-05-17 21:45:00      -1.5       TRUE
  ## 237 2010-05-17 22:00:00      -1.5      FALSE
  ## ...
  ## 289 2010-05-17 11:00:00      -1.5      FALSE

  in.weekend <- timestamp.in.weekend(pidata)
  target <- rep(FALSE, 289)
  target[45:236] <- TRUE
  checkEquals(target, in.weekend)
}

test.timestamp.in.range.hour.a <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=210*60, length.out=14)
  ## 23:30 03:00 06:30 10:00 13:30 17:00 20:30 00:00 03:30 07:00 10:30 14:00 17:30 21:00

  in.period <- timestamp.in.range.hour(pidata, 10, 17, tz="UTC")
  expect <- c(F, F, F, T, T, F, F, F, F, F, T, T, F, F)
  checkEquals(expect, in.period)
}

test.timestamp.in.range.hour.b <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=210*60, length.out=14)
  ## 23:30 03:00 06:30 10:00 13:30 17:00 20:30 00:00 03:30 07:00 10:30 14:00 17:30 21:00

  in.period <- timestamp.in.range.hour(pidata, 17, 10, tz="UTC")
  expect <- c(T, T, T, F, F, T, T, T, T, T, F, F, T, T)
  checkEquals(expect, in.period)
}

test.reformat.date <- function() {
  ## testing internal function .reformat.date
  checkEquals("0101", delftfews:::.reformat.date("0101"))
  checkEquals("0101", delftfews:::.reformat.date("01/01"))
  checkEquals("0101", delftfews:::.reformat.date("01-01"))
}

test.timestamp.in.range.calendar.contiguous.1.a <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=720*60, length.out=14)
  ## 0213 0214 0214 0215 0215 0216 0216 0217 0217 0218 0218 0219 0219 0220

  in.period <- timestamp.in.range.calendar(pidata, "0214", "0217", tz="UTC")
  expect <- rep(F, 14)
  expect[2:7] <- TRUE
  checkEquals(expect, in.period)
}

test.timestamp.in.range.calendar.contiguous.1.b <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=720*60, length.out=14)
  ## 0213 0214 0214 0215 0215 0216 0216 0217 0217 0218 0218 0219 0219 0220

  in.period <- timestamp.in.range.calendar(pidata, "0215", "0216", tz="UTC")
  expect <- rep(F, 14)
  expect[4:5] <- TRUE
  checkEquals(expect, in.period)
}

test.timestamp.in.range.calendar.contiguous.1.c <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=720*60, length.out=14)
  ## 0213 0214 0214 0215 0215 0216 0216 0217 0217 0218 0218 0219 0219 0220
  
  in.period <- timestamp.in.range.calendar(pidata, "0210", "0216", tz="UTC")
  expect <- rep(F, 14)
  expect[1:5] <- TRUE
  checkEquals(expect, in.period)
}

test.timestamp.in.range.calendar.contiguous.2.a <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=18)
  ## 2009-02-13 2009-03-25 2009-05-04 2009-06-13 2009-07-23 2009-09-01
  ## 2009-10-11 2009-11-20 2009-12-30 2010-02-08 2010-03-20 2010-04-29
  ## 2010-06-08 2010-07-18 2010-08-27 2010-10-06 2010-11-15 2010-12-25

  in.period <- timestamp.in.range.calendar(pidata, "0301", "1001", tz="UTC")
  expect <- rep(F, 18)
  expect[2:6] <- TRUE
  expect[11:15] <- TRUE
  checkEquals(expect, in.period) 
}

test.timestamp.in.range.calendar.contiguous.2.b <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=18)
  ## 2009-02-13 2009-03-25 2009-05-04 2009-06-13 2009-07-23 2009-09-01
  ## 2009-10-11 2009-11-20 2009-12-30 2010-02-08 2010-03-20 2010-04-29
  ## 2010-06-08 2010-07-18 2010-08-27 2010-10-06 2010-11-15 2010-12-25

  in.period <- timestamp.in.range.calendar(pidata, "3-1", "10-1", tz="UTC")
  expect <- rep(F, 18)
  expect[2:6] <- TRUE
  expect[11:15] <- TRUE
  checkEquals(expect, in.period) 
}

test.timestamp.in.range.calendar.contiguous.3 <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=18)
  ## 2009-02-13 2009-03-25 2009-05-04 2009-06-13 2009-07-23 2009-09-01
  ## 2009-10-11 2009-11-20 2009-12-30 2010-02-08 2010-03-20 2010-04-29
  ## 2010-06-08 2010-07-18 2010-08-27 2010-10-06 2010-11-15 2010-12-25

  checkException(timestamp.in.range.calendar(pidata, "31", "101", tz="UTC"), ": should crash on ambiguous input")
  checkException(timestamp.in.range.calendar(pidata, "301", "1001", tz="UTC"), ": should crash on ambiguous input")
}

test.timestamp.in.range.calendar.split <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=18)
  ## 2009-02-13 2009-03-25 2009-05-04 2009-06-13 2009-07-23 2009-09-01
  ## 2009-10-11 2009-11-20 2009-12-30 2010-02-08 2010-03-20 2010-04-29
  ## 2010-06-08 2010-07-18 2010-08-27 2010-10-06 2010-11-15 2010-12-25

  in.period <- timestamp.in.range.calendar(pidata, "1001", "0401", tz="UTC")
  expect <- rep(F, 18)
  expect[1:2] <- TRUE
  expect[7:11] <- TRUE
  expect[16:18] <- TRUE
  checkEquals(expect, in.period)
}

test.select.percentiles.timeseries.30.80.10 <- function() {
  l <- rep(1:10, each=22)
  dim(l) <- c(22, 10)
  l[,sample(1:10)] <- l # shuffle columns
  colnames(l) <- rep('a', 10)
  pidata <- timeseries(21000000*60, by=5*60, length.out=22, data=l)
  current <- select.percentiles(pidata, c(30, 80))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.30=3L, a.80=8L)
  checkEquals(target, current)
}

test.select.percentiles.timeseries.10.20.90.100.10 <- function() {
  l <- rep(1:10, each=22)
  dim(l) <- c(22, 10)
  l[,sample(1:10)] <- l # shuffle columns
  colnames(l) <- rep('a', 10)
  pidata <- timeseries(21000000*60, by=5*60, length.out=22, data=l)
  current <- select.percentiles(pidata, c(10, 20, 90, 100))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.10=1L, a.20=2L, a.90=9L, a.100=10L)
  checkEquals(target, current)
}

test.select.percentiles.timeseries.30.80.100 <- function() {
  l <- rep(1:100, each=22)
  dim(l) <- c(22, 100)
  l[,sample(1:100)] <- l # shuffle columns
  colnames(l) <- rep('a', 100)
  pidata <- timeseries(21000000*60, by=5*60, length.out=22, data=l)
  current <- select.percentiles(pidata, c(30, 80))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.30=30L, a.80=80L)
  checkEquals(target, current)
}

test.select.percentiles.timeseries.with.four.NA.columns <- function() {
  ## NA columns are ignored
  l <- rep(1:100, each=22)
  dim(l) <- c(22, 100)
  l <- cbind(l, NA)
  l <- cbind(l, NA)
  l <- cbind(l, NA)
  l <- cbind(l, NA)
  l[,sample(1:104)] <- l # shuffle columns
  colnames(l) <- rep('a', 104)
  pidata <- timeseries(21000000*60, by=5*60, length.out=22, data=l)
  current <- select.percentiles(pidata, c(30, 80))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.30=30L, a.80=80L)
  checkEquals(target, current)
}

test.select.percentiles.timeseries.with.only.NA.columns <- function() {
  ## user provided us with all-NA data, except for the timestamps.  we
  ## return something similar.
  l <- rep(NA, 2200)
  dim(l) <- c(22, 100)
  colnames(l) <- rep('a', 100)
  pidata <- timeseries(21000000*60, by=5*60, length.out=22, data=l)
  current <- select.percentiles(pidata, c(30, 80))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.30=NA, a.80=NA)
  checkEquals(target, current)
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
