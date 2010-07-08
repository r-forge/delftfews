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

require(RUnit)

test.select.percentiles.timeseries.30.80.10 <- function() {
  pidata <- timeseries(21000000*60, by=5*60, length.out=22)
  for(i in 1:10) {
    pidata[i+1] <- i
  }
  names(pidata)[(1:10)+1] <- 'a'
  current <- select.percentiles(pidata, c(30, 80))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.30=3, a.80=8)
  checkEquals(target, current)
}

test.select.percentiles.timeseries.10.20.90.100.10 <- function() {
  pidata <- timeseries(21000000*60, by=5*60, length.out=22)
  for(i in 1:10) {
    pidata[i+1] <- i
  }
  names(pidata)[(1:10)+1] <- 'a'
  current <- select.percentiles(pidata, c(10, 20, 90, 100))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.10=1, a.20=2, a.90=9, a.100=10)
  checkEquals(target, current)
}

test.select.percentiles.timeseries.30.80.100 <- function() {
  pidata <- timeseries(21000000*60, by=5*60, length.out=22)
  for(i in 1:100) {
    pidata[i+1] <- i
  }
  names(pidata)[(1:100)+1] <- 'a'
  current <- select.percentiles(pidata, c(30, 80))
  target <- timeseries(21000000*60, by=5*60, length.out=22, a.30=30, a.80=80)
  checkEquals(target, current)
}

test.timestamp.in.range <- function() {
  DEACTIVATED("timestamp.in.range is not tested.")
}

test.timestamp.in.range.weekday <- function() {
  DEACTIVATED("timestamp.in.range.weekday is not tested.")
}

test.timestamp.in.weekend <- function(){
  pidata <- read.PI('data/timeseries.in.weekend.timeperiod.reference.xml')
  in.weekend <- timestamp.in.weekend(pidata)
  ##259-15 = 244 non-weekend timestamps
  ##304-258 = 46 weekend timestamps

  checkTrue(length(in.weekend) == 289)
  ##first are all non-weeekend
  checkTrue(all(!in.weekend[1:244]))
  #last are weekend (16-1-2010)
  checkTrue(all(in.weekend[245:289]))
}

test.timestamp.in.range.hour <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=210*60, length.out=14)
  ## 23:30 03:00 06:30 10:00 13:30 17:00 20:30 00:00 03:30 07:00 10:30 14:00 17:30 21:00

  in.period <- timestamp.in.range.hour(pidata, 10, 17)
  expect <- c(F, F, F, T, T, T, F, F, F, F, T, T, F, F)
  checkEquals(expect, in.period)

  in.period <- timestamp.in.range.hour(pidata, 17, 10)
  expect <- c(T, T, T, T, F, T, T, T, T, T, F, F, T, T)
  checkEquals(expect, in.period)
}

test.reformat.date <- function() {
  DEACTIVATED("reformat.date is not tested.")
}

test.timestamp.in.range.calendar.contiguous.1 <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=720*60, length.out=14)
  ## 0213 0214 0214 0215 0215 0216 0216 0217 0217 0218 0218 0219 0219 0220

  in.period <- timestamp.in.range.calendar(pidata, "0214", "0217")
  expect <- rep(F, 14)
  expect[2:7] <- TRUE
  checkEquals(expect, in.period)

  in.period <- timestamp.in.range.calendar(pidata, "0215", "0216")
  expect <- rep(F, 14)
  expect[4:5] <- TRUE
  checkEquals(expect, in.period)
  
  in.period <- timestamp.in.range.calendar(pidata, "0210", "0216")
  expect <- rep(F, 14)
  expect[1:5] <- TRUE
  checkEquals(expect, in.period)
}

test.timestamp.in.range.calendar.contiguous.2 <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=18)
  ## 2009-02-13 2009-03-25 2009-05-04 2009-06-13 2009-07-23 2009-09-01
  ## 2009-10-11 2009-11-20 2009-12-30 2010-02-08 2010-03-20 2010-04-29
  ## 2010-06-08 2010-07-18 2010-08-27 2010-10-06 2010-11-15 2010-12-25

  in.period <- timestamp.in.range.calendar(pidata, "0301", "1001")
  expect <- rep(F, 18)
  expect[2:6] <- TRUE
  expect[11:15] <- TRUE
  checkEquals(expect, in.period) 

  in.period <- timestamp.in.range.calendar(pidata, "3-1", "10-1")
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

  checkException(timestamp.in.range.calendar(pidata, "31", "101"), ": should crash on ambiguous input")
  checkException(timestamp.in.range.calendar(pidata, "301", "1001"), ": should crash on ambiguous input")

}

test.timestamp.in.range.calendar.split <- function() {
  pidata <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=18)
  ## 2009-02-13 2009-03-25 2009-05-04 2009-06-13 2009-07-23 2009-09-01
  ## 2009-10-11 2009-11-20 2009-12-30 2010-02-08 2010-03-20 2010-04-29
  ## 2010-06-08 2010-07-18 2010-08-27 2010-10-06 2010-11-15 2010-12-25

  in.period <- timestamp.in.range.calendar(pidata, "1001", "0401")
  expect <- rep(F, 18)
  expect[1:2] <- TRUE
  expect[7:11] <- TRUE
  expect[16:18] <- TRUE
  checkEquals(expect, in.period)
}
