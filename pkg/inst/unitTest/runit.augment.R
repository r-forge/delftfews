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

test.timeseries.base <- function() {
  ## test equality except dimnames
  minutes <- (0:5) * 720
  target <- zoo(data.frame(), as.POSIXct(minutes * 60, origin=EPOCH))
  class(target) <- c("delftfews", class(target))
  attr(target, 'timestep') <- 720*60
  current <- timeseries(from=0, by=720*60, length.out=6)
  checkEqualsNumeric(target, current)
}

test.timeseries.dimnames <- function() {
  ## only test dimnames
  minutes <- (0:5) * 720
  target <- list(NULL, NULL)
  current <- dimnames(timeseries(from=0, by=720*60, length.out=6))
  checkEquals(target, current)
}

test.timeseries.with.one.column <- function() {
  minutes <- (0:5) * 720
  target <- zoo(data.frame(a=1), as.POSIXct(minutes * 60, origin=EPOCH))
  dimnames(target) <- list(NULL, dimnames(target)[[2]])
  class(target) <- c("delftfews", class(target))
  attr(target, 'timestep') <- 720*60
  current <- timeseries(from=0, by=720*60, length.out=6, a=1)
  checkEquals(target, current)
}

test.timeseries.with.more.columns <- function() {
  minutes <- (0:5) * 720
  target <- zoo(data.frame(a=1, b=1:6), as.POSIXct(minutes * 60, origin=EPOCH))
  dimnames(target) <- list(NULL, dimnames(target)[[2]])
  class(target) <- c("delftfews", class(target))
  attr(target, 'timestep') <- 720*60
  current <- timeseries(from=0, by=720*60, length.out=6, a=1, b=1:6)
  checkEquals(target, current)
}

test.timeseries.with.data.frame <- function() {
  full <- data.frame(a=1, b=1:6, c=(0:5)*4)
  minutes <- (0:5) * 720
  target <- zoo(full, as.POSIXct(minutes * 60, origin=EPOCH))
  dimnames(target) <- list(NULL, dimnames(target)[[2]])
  class(target) <- c("delftfews", class(target))
  attr(target, 'timestep') <- 720*60
  current <- timeseries(from=0, by=720*60, length.out=6, data=full)
  checkEquals(target, current)
}

test.timeseries.with.order.by <- function() {
  full <- data.frame(a=1, b=1:6, c=(0:5)*4)
  minutes <- (0:5) * 720
  template <- timeseries(from=0, by=720*60, length.out=6)
  target <- zoo(full, as.POSIXct(minutes * 60, origin=EPOCH))
  dimnames(target) <- list(NULL, dimnames(target)[[2]])
  class(target) <- c("delftfews", class(target))
  ## TODO: decide about this one!
  ## attr(target, 'timestep') <- 720*60

  current <- timeseries(order.by=index(template), data=full)
  checkEquals(target, current)
}

test.cumulate.timeseries.one.net.stretch <- function() {
  data <- rep(NA, 10)
  data[4:6] <- 5
  ## 0 0 0 5 5 5 0 0 0 0
  pidata <- timeseries(1234567800, by=5*60, length.out=10, input=data)

  result <- cumulate.timeseries(pidata, integration.method=1)

  target <- rep(NA, 10)
  target[4:6] <- 25*60
  checkEquals(target, coredata(result$input.gross.partials))
  checkEquals(target, coredata(result$input.net.partials))

  target <- rep(NA, 10)
  target[4] <- 75*60
  checkEquals(target, coredata(result$input.gross.totals))
  checkEquals(target, coredata(result$input.net.totals))

  target <- rep(NA, 10)
  target[4] <- 15 * 60
  checkEquals(target, coredata(result$input.gross.duration))
  checkEquals(target, coredata(result$input.net.duration))
}

test.cumulate.timeseries.one.net.stretch.trapezoid <- function() {
  data <- rep(NA, 10)
  data[4:6] <- 2
  data[5] <- 4
  ## 0 0 0 2 4 2 0 0 0 0
  pidata <- timeseries(1234567800, by=5*60, length.out=10, input=data)

  result <- cumulate.timeseries(pidata, integration.method=3)
  ## input duration partials totals
  ##     0       NA       NA     NA
  ##     0       NA       NA     NA
  ##     0       NA       NA     NA
  ##     2 ??TODO??        5     40
  ##     4       NA       15     NA
  ##     2       NA       15     NA
  ##     0       NA        5     NA
  ##     0       NA       NA     NA
  ##     0       NA       NA     NA
  ##     0       NA       NA     NA

  target <- rep(NA, 10)
  target[c(4,7)] <- 5 * 60
  target[c(5,6)] <- 15 * 60
  checkEquals(target, coredata(result$input.gross.partials))
  checkEquals(target, coredata(result$input.net.partials))

  target <- rep(NA, 10)
  target[4] <- 40 * 60
  checkEquals(target, coredata(result$input.gross.totals))
  checkEquals(target, coredata(result$input.net.totals))

  ## TODO - now same duration as integration.method 1
  target <- rep(NA, 10)
  target[4] <- 15 * 60
  checkEquals(target, coredata(result$input.gross.duration))
  checkEquals(target, coredata(result$input.net.duration))
}

test.cumulate.timeseries.two.net.stretches <- function() {
  data <- rep(NA, 15)
  data[4:6] <- 5
  data[9] <- 5
  pidata <- timeseries(1234567800, by=5*60, length.out=15, input=data)
  ##             timestamps input
  ## 1  2009-02-13 23:30:00     0
  ## 2  2009-02-13 23:35:00     0
  ## 3  2009-02-13 23:40:00     0
  ## 4  2009-02-13 23:45:00     5
  ## 5  2009-02-13 23:50:00     5
  ## 6  2009-02-13 23:55:00     5
  ## 7  2009-02-14 00:00:00     0
  ## 8  2009-02-14 00:05:00     0
  ## 9  2009-02-14 00:10:00     5
  ## 10 2009-02-14 00:15:00     0
  ## 11 2009-02-14 00:20:00     0
  ## 12 2009-02-14 00:25:00     0
  ## 13 2009-02-14 00:30:00     0
  ## 14 2009-02-14 00:35:00     0
  ## 15 2009-02-14 00:40:00     0

  result <- cumulate.timeseries(pidata, gap=3, integration.method=1)

  ##    input input.gross.totals input.net.totals
  ## 1      0                  0                0
  ## 2      0                  0                0
  ## 3      0                  0                0
  ## 4      5                100|              75|
  ## 5      5                  0|               0|
  ## 6      5                  0|               0|
  ## 7      0                  0|               0
  ## 8      0                  0|               0
  ## 9      5                  0|              25|
  ## 10     0                  0                0
  ## 11     0                  0                0
  ## 12     0                  0                0
  ## 13     0                  0                0
  ## 14     0                  0                0
  ## 15     0                  0                0
  target <- rep(NA, 15)
  target[4] <- 100 * 60
  checkEquals(target, coredata(result$input.gross.totals))
  target[4] <- 75 * 60
  target[9] <- 25 * 60
  checkEquals(target, coredata(result$input.net.totals))

  ##    input input.gross.duration input.net.duration 
  ## 1      0                    0                  0 
  ## 2      0                    0                  0 
  ## 3      0                    0                  0 
  ## 4      5                   30|                15|
  ## 5      5                    0|                 0|
  ## 6      5                    0|                 0|
  ## 7      0                    0|                 0 
  ## 8      0                    0|                 0 
  ## 9      5                    0|                 5|
  ## 10     0                    0                  0 
  ## 11     0                    0                  0 
  ## 12     0                    0                  0 
  ## 13     0                    0                  0 
  ## 14     0                    0                  0 
  ## 15     0                    0                  0 
  target <- rep(NA, 15)
  target[4] <- 30 * 60
  checkEquals(target, coredata(result$input.gross.duration))
  target[4] <- 15 * 60
  target[9] <- 5 * 60
  checkEquals(target, coredata(result$input.net.duration))
}

test.cumulate.timeseries.two.net.stretches.trapezoid <- function() {
  data <- rep(NA, 15)
  data[4:6] <- 5
  data[9] <- 5
  pidata <- timeseries(1234567800, by=5*60, length.out=15, input=data)
  ##             timestamps input
  ## 1  2009-02-13 23:30:00     0
  ## 2  2009-02-13 23:35:00     0
  ## 3  2009-02-13 23:40:00     0
  ## 4  2009-02-13 23:45:00     5
  ## 5  2009-02-13 23:50:00     5
  ## 6  2009-02-13 23:55:00     5
  ## 7  2009-02-14 00:00:00     0
  ## 8  2009-02-14 00:05:00     0
  ## 9  2009-02-14 00:10:00     5
  ## 10 2009-02-14 00:15:00     0
  ## 11 2009-02-14 00:20:00     0
  ## 12 2009-02-14 00:25:00     0
  ## 13 2009-02-14 00:30:00     0
  ## 14 2009-02-14 00:35:00     0
  ## 15 2009-02-14 00:40:00     0

  result <- cumulate.timeseries(pidata, gap=3, integration.method=3)

  ##    input gross.partials net.partials
  ## 1      0             NA           NA
  ## 2      0             NA           NA
  ## 3      0             NA           NA
  ## 4      5           12.5|        12.5|
  ## 5      5           25.0|        25.0|
  ## 6      5           25.0|        25.0|
  ## 7      0           12.5|        12.5|
  ## 8      0              0|          NA
  ## 9      5           12.5|        12.5|
  ## 10     0           12.5|        12.5|
  ## 11     0             NA           NA
  ## 12     0             NA           NA
  ## 13     0             NA           NA
  ## 14     0             NA           NA
  ## 15     0             NA           NA
  target <- rep(NA, 15)
  target[4:10] <- 12.5 * 60
  target[5:6] <- 25 * 60
  target[8] <- NA
  checkEquals(target, coredata(result$input.net.partials))
  target[8] <- 0
  checkEquals(target, coredata(result$input.gross.partials))
}

test.cumulate.timeseries.two.net.stretches.near.borders <- function() {
  data <- rep(NA, 10)
  data[2:6] <- 5
  data[9:10] <- 5
  pidata <- timeseries(1234567800, by=5*60, length.out=10, input=data)
  ##             timestamps input
  ## 1  2009-02-13 23:30:00     0
  ## 2  2009-02-13 23:35:00     5
  ## 3  2009-02-13 23:40:00     5
  ## 4  2009-02-13 23:45:00     5
  ## 5  2009-02-13 23:50:00     5
  ## 6  2009-02-13 23:55:00     5
  ## 7  2009-02-14 00:00:00     0
  ## 8  2009-02-14 00:05:00     0
  ## 9  2009-02-14 00:10:00     5
  ## 10 2009-02-14 00:15:00     5

  result <- cumulate.timeseries(pidata, gap=3, integration.method=1)

  ##    input input.gross.totals input.net.totals
  ## 1      0                  0                0
  ## 2      5                175|             125|
  ## 3      5                  0|               0|
  ## 4      5                  0|               0|
  ## 5      5                  0|               0|
  ## 6      5                  0|               0|
  ## 7      0                  0|               0
  ## 8      0                  0|               0
  ## 9      5                  0|              50|
  ## 10     5                  0|               0|
  target <- rep(NA, 10)
  target[2] <- 175 * 60
  checkEquals(target, coredata(result$input.gross.totals))
  target[2] <- 125 * 60
  target[9] <- 50 * 60
  checkEquals(target, coredata(result$input.net.totals))

  ##    input input.gross.duration input.net.duration 
  ## 1      0                    0                  0 
  ## 2      5                   45|                25|
  ## 3      5                    0|                 0|
  ## 4      5                    0|                 0|
  ## 5      5                    0|                 0|
  ## 6      5                    0|                 0|
  ## 7      0                    0|                 0 
  ## 8      0                    0|                 0 
  ## 9      5                    0|                10|
  ## 10     5                    0|                 0|
  target <- rep(NA, 10)
  target[2] <- 45 * 60
  checkEquals(target, coredata(result$input.gross.duration))
  target[2] <- 25 * 60
  target[9] <- 10 * 60
  checkEquals(target, coredata(result$input.net.duration))
}

test.cumulate.timeseries.all.NA <- function() {
  pidata <- timeseries(21000000*60, by=5*60, length.out=10)
  pidata$input <- NA

  result <- cumulate.timeseries(pidata, integration.method=1)

  target <- rep(NA, 10)
  checkEquals(target, coredata(result$input.gross.totals))
  checkEquals(target, coredata(result$input.net.totals))
  checkEquals(target, coredata(result$input.gross.duration))
  checkEquals(target, coredata(result$input.net.duration))
}
