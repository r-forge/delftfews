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

test.na.fill <- function() {
  checkEquals(c(1, 1, 1, 2, 3, 3, 4), na.fill(c(1, NA, NA, 2, 3, NA, 4)))
  checkEquals(c(1, 2, 3, 4), na.fill(c(1, 2, 3, 4)))
  checkEquals(c(2, 2, 2, 3, 3, 4), na.fill(c(NA, NA, 2, 3, NA, 4)))
}

test.na.zero <- function() {
  checkEquals(c(1, 0, 0, 2, 3, 0, 4), na.zero(c(1, NA, NA, 2, 3, NA, 4)))
  checkEquals(c(1, 2, 3, 4), na.zero(c(1, 2, 3, 4)))
}

test.na.interpolate <- function() {
  DEACTIVATED("this function is not yet needed.")
  checkEquals(c(1, 2, 3, 4), na.interpolate(c(1, 2, 3, 4)), ", itempotent")
  checkEquals(c(1, 3, 5, 7, 3, 3.5, 4), na.interpolate(c(1, NA, NA, 7, 3, NA, 4)))
  checkEquals(c(2, 2, 2, 3, 3.5, 4), na.interpolate(c(NA, NA, 2, 3, NA, 4)))
}

test.stretches.no.gap <- function() {
  result <- stretches(c(0, 0, 1, 0, 0))
  target <- c(3)
  checkEquals(target, result)
  result <- stretches(c(0, 0, 1, 0, 0), what="end")
  target <- c(3)
  checkEquals(target, result)
  result <- stretches(c(0, 0, 1, 1, 1, 1, 0, 0), what="end")
  target <- c(6)
  checkEquals(target, result)
}

test.stretches.NA.no.gap <- function() {
  result <- stretches(c(NA, NA, 1, NA, NA))
  target <- c(3)
  checkEquals(target, result)
  result <- stretches(c(NA, NA, 1, NA, NA), what="end")
  target <- c(3)
  checkEquals(target, result)
  result <- stretches(c(NA, NA, 1, 1, 1, 1, NA, NA), what="end")
  target <- c(6)
  checkEquals(target, result)
}

test.stretches.short.gap.gross <- function() {
  result <- stretches(c(0, 0, 1, 0, 0))
  target <- c(3)
  result <- stretches(c(0, 0, 1, 0, 1, 0, 0), gap=2)
  target <- c(3)
  checkEquals(target, result)
  result <- stretches(c(0, 0, 1, 1, 0, 1, 1, 0, 0), gap=2)
  target <- c(3)
  checkEquals(target, result)
}

test.stretches.NA.short.gap.gross <- function() {
  result <- stretches(c(NA, NA, 1, NA, NA))
  target <- c(3)
  result <- stretches(c(NA, NA, 1, NA, 1, NA, NA), gap=2)
  target <- c(3)
  checkEquals(target, result)
  result <- stretches(c(NA, NA, 1, 1, NA, 1, 1, NA, NA), gap=2)
  target <- c(3)
  checkEquals(target, result)
}

test.stretches.short.gap.net <- function() {
  result <- stretches(c(0, 0, 1, 0, 1, 0, 0))
  target <- c(3, 5)
  checkEquals(target, result)
  result <- stretches(c(0, 0, 1, 0, 1, 1, 1, 0, 0))
  target <- c(3, 5)
  checkEquals(target, result)
}

test.stretches.NA.short.gap.net <- function() {
  result <- stretches(c(NA, NA, 1, NA, 1, NA, NA))
  target <- c(3, 5)
  checkEquals(target, result)
  result <- stretches(c(NA, NA, 1, NA, 1, 1, 1, NA, NA))
  target <- c(3, 5)
  checkEquals(target, result)
}

test.stretches.long.gap.gross <- function() {
  input <- c(0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  result <- stretches(input, gap=2)
  checkEquals(c(5, 9), result)
  result <- stretches(input, gap=3)
  checkEquals(c(5, 9), result)
  result <- stretches(input, gap=4)
  checkEquals(c(5), result)
}

test.stretches.NA.long.gap.gross <- function() {
  input <- c(NA, NA, NA, NA, 1, NA, NA, NA, 1, NA, NA, NA, NA)
  result <- stretches(input, gap=2)
  checkEquals(c(5, 9), result)
  result <- stretches(input, gap=3)
  checkEquals(c(5, 9), result)
  result <- stretches(input, gap=4)
  checkEquals(c(5), result)
}

test.stretches.short.gap.net.surrounded <- function() {
  result <- stretches(c(1, 0, 1), zero.surrounded=TRUE)
  target <- c(1, 3)
  checkEquals(target, result)
}

test.stretches.NA.short.gap.net.surrounded <- function() {
  result <- stretches(c(1, NA, 1), zero.surrounded=TRUE)
  target <- c(1, 3)
  checkEquals(target, result)
}

test.stretches.long.gap.gross.not.surrounded <- function() {
  input <- c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
  checkEquals(c(6),    stretches(input, gap=2))
  checkEquals(c(2, 6), stretches(input, gap=2, zero.surrounded=TRUE))
  checkEquals(c(6),    stretches(input, gap=3))
  checkEquals(c(2, 6), stretches(input, gap=3, zero.surrounded=TRUE))
  checkEquals(numeric(0),  stretches(input, gap=4))
  checkEquals(c(2),        stretches(input, gap=4, zero.surrounded=TRUE))

  input <- c(0, 1, 0, 0, 0, 1)
  checkEquals(c(6),       stretches(input, gap=2))
  checkEquals(c(2, 6),    stretches(input, gap=2, zero.surrounded=TRUE))
  checkEquals(c(6),       stretches(input, gap=3))
  checkEquals(c(2, 6),    stretches(input, gap=3, zero.surrounded=TRUE))
  checkEquals(numeric(0), stretches(input, gap=4))
  checkEquals(c(2),       stretches(input, gap=4, zero.surrounded=TRUE))
}

test.stretches.NA.long.gap.gross.not.surrounded <- function() {
  input <- c(NA, 1, NA, NA, NA, 1, NA, NA, NA, NA)
  checkEquals(c(6),    stretches(input, gap=2))
  checkEquals(c(2, 6), stretches(input, gap=2, zero.surrounded=TRUE))
  checkEquals(c(6),    stretches(input, gap=3))
  checkEquals(c(2, 6), stretches(input, gap=3, zero.surrounded=TRUE))
  checkEquals(numeric(0),  stretches(input, gap=4))
  checkEquals(c(2),        stretches(input, gap=4, zero.surrounded=TRUE))

  input <- c(NA, 1, NA, NA, NA, 1)
  checkEquals(c(6),       stretches(input, gap=2))
  checkEquals(c(2, 6),    stretches(input, gap=2, zero.surrounded=TRUE))
  checkEquals(c(6),       stretches(input, gap=3))
  checkEquals(c(2, 6),    stretches(input, gap=3, zero.surrounded=TRUE))
  checkEquals(numeric(0), stretches(input, gap=4))
  checkEquals(c(2),       stretches(input, gap=4, zero.surrounded=TRUE))
}

test.rollingSum.base <- function() {
  input <- (1:20)
  result <- rollingSum(input, 10)
  expect <- c(NA, NA, NA, NA, NA,  NA,  NA,  NA,  NA,  55,  65,  75,  85,  95, 105, 115, 125, 135, 145, 155)
  checkEquals(length(expect), length(result))
  checkEquals(expect, result)
}

test.rollingSum.with.na <- function() {  
  input <- c(1, 1, 1, NA, 2, 2, 1, NA, 1, 2, 1)
  result <- rollingSum(input, 3)
  expect <- c(NA, NA, 3, 2, 3, 4, 5, 3, 2, 3, 4)
  checkEquals(length(expect), length(result))
  checkEquals(expect, result)
}

test.rollingSum.delftfews <- function() {
  input <- timeseries(from=0, by=720*60, length.out=20, a=1, b=1:20)
  result <- rollingSum(input$b, 10)
  target.numeric <- c(NA, NA, NA, NA, NA,  NA,  NA,  NA,  NA,  55,  65,  75,  85,  95, 105, 115, 125, 135, 145, 155)
  checkEquals(length(target.numeric), length(result))
  checkEqualsNumeric(target.numeric, result)
  target <- timeseries(from=0, by=720*60, length.out=20, b=target.numeric)
  checkEquals(target, result)
}

test.rollapply <- function() {
  input <- c(rep(1:3, 2), NA, 4, 4, 4)
  ##  [1]  1  2  3  1  2  3 NA  4  4  4

  result <- rollapply(input, 2, sum)
  expect <- c(NA, 3, 5, 4, 3, 5, NA, NA, 8, 8)
  checkEquals(expect, result)

  result <- rollapply(input, 2, sum, na.action=na.zero)
  expect <- c(NA, 3, 5, 4, 3, 5, 3, 4, 8, 8)
  checkEquals(expect, result)

  result <- rollapply(input, 4, sum, na.action=na.zero)
  expect <- c(NA, NA, NA, 7, 8, 9, 6, 9, 11, 12)
  checkEquals(expect, result)
  
  result <- rollapply(input, 4, max, na.action=na.zero)
  expect <- c(NA, NA, NA, 3, 3, 3, 3, 4, 4, 4)
  checkEquals(expect, result)
  
  result <- rollapply(input, 4, mean, na.action=na.zero)
  expect <- c(NA, NA, NA, 1.75, 2.00, 2.25, 1.50, 2.25, 2.75, 3.00)
  checkEquals(expect, result)
  
  result <- rollapply(input, 2, min, na.action=na.zero)
  expect <- c(NA, 1, 2, 1, 1, 2, 0, 0, 4, 4)
  checkEquals(expect, result)
  
}

test.shift.vector <- function() {
  checkEquals(c(NA, NA, 1), shift.vector(c(1, 2, 3), 2))
  checkEquals(c(2, 3, NA), shift.vector(c(1, 2, 3), -1))
  checkEquals(c(1, 2, 3), shift.vector(c(1, 2, 3), 0))
}

test.contiguous.stretch.1 <- function() {
  ## all adjacent values
  input <- c(1,1,1,2,2,2,3,3,2,2)
  current <- contiguous.stretch(input, 1)
  target <- rep(FALSE, length(input))
  target[1:3] <- TRUE
  checkEquals(target, current)
}

test.contiguous.stretch.2 <- function() {
  ## starting at different position, but finding the same value as test.1
  input <- c(1,1,1,2,2,2,3,3,2,2)
  current <- contiguous.stretch(input, 3)
  target <- rep(FALSE, length(input))
  target[1:3] <- TRUE
  checkEquals(target, current)
}

test.contiguous.stretch.3 <- function() {
  ## starting at different position, finding other value than test.1
  ## and 2.  notice this will not return the last two positions, which
  ## are not adjacent to the stretch.
  input <- c(1,1,1,2,2,2,3,3,2,2)
  current <- contiguous.stretch(input, 5)
  target <- rep(FALSE, length(input))
  target[4:6] <- TRUE
  checkEquals(target, current)
}

test.contiguous.stretch.4 <- function() {
  ## the value is not found at position, so the result is all FALSE
  input <- c(1,1,1,2,2,2,3,3,2,2)
  current <- contiguous.stretch(input, 1, 2)
  target <- rep(FALSE, length(input))
  checkEquals(target, current)
}

test.contiguous.stretch.5 <- function() {
  ## stretch of values different from 2, starting at positon 1
  input <- c(1,1,1,2,2,2,3,3,2,2)
  current <- contiguous.stretch(input, 1, 2, FALSE)
  target <- rep(FALSE, length(input))
  target[1:3] <- TRUE
  checkEquals(target, current)
}

test.contiguous.stretch.6 <- function() {
  ## stretch of values different from 3, starting at positon 1
  input <- c(1,1,1,2,2,2,3,3,2,2)
  current <- contiguous.stretch(input, 1, 3, FALSE)
  target <- rep(FALSE, length(input))
  target[1:6] <- TRUE
  checkEquals(target, current)
}

test.contiguous.stretch.7 <- function() {
  ## stretch of values different from 4, starting at positon 1
  input <- c(1,1,1,2,2,2,3,3,2,2)
  current <- contiguous.stretch(input, 1, 4, FALSE)
  target <- rep(TRUE, length(input))
  checkEquals(target, current)
}

test.get.step <- function() {
  ## testing unexported function
  get.step <- delftfews:::get.step
  
  L <- list(a=cumsum(c(1,3,3,2,3,3,4,3,2,3,3)), b=cumsum(c(2,3,2,3)))
  checkEquals(3, get.step(L))
  L <- data.frame(a=c(2,4,6,8,12,14,18), b=c(12,14,16,18,22,24,28))
  checkEquals(2, get.step(L))
  L <- matrix(c(2,4,6,8,12,14,18, 12,14,16,18,22,24,28), 7, 2)
  checkEquals(2, get.step(L))
}

test.double.threshold.0 <- function() {
  ## start below lower (false) threshold
  values <- c(0, 2, 3, 2, 1, 0)
  target <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  current <- double.threshold(values, 1, 2)
  checkEquals(target, current)
}

test.double.threshold.1 <- function() {
  ## start between thresholds, default initial value FALSE
  values <- c(1, 2, 3, 2, 1, 0)
  target <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  current <- double.threshold(values, 1, 2)
  checkEquals(target, current)
}

test.double.threshold.2 <- function() {
  ## start between thresholds, explicit initial value TRUE
  values <- c(1, 2, 3, 2, 1, 0)
  target <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  current <- double.threshold(values, 1, 2, TRUE)
  checkEquals(target, current)
}

test.double.threshold.3 <- function() {
  ## start below false threshold, ignores initial value TRUE
  values <- c(0, 2, 3, 2, 1, 0)
  target <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  current <- double.threshold(values, 1, 2, TRUE)
  checkEquals(target, current)
}

test.double.threshold.with.equal <- function() {
  ## start below false threshold, ignores initial value TRUE
  values <- c(0, 2, 3, 2, 1, 0)
  current <- double.threshold(values, 1, 2, on.equality=TRUE)
  target <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  checkEquals(target, current)
}

test.double.threshold.summarise.1 <- function() {
  values <- c(0, 0.5, 0.75, 1, 1.25, 1.5, 1.65, 1.45, 1.25, 1.4, 1.6, 1.2, 1.1, 0.9, 1.2, 1, 1.3, 1.5, 1.6, 1.3, 1.1, 0.8, 1, 1.4, 1.5, 1.3, 1.2, 1.1, 0.5, 0.2, 0)
  target <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  current <- as.numeric(double.threshold(values, 1.1, 1.5))
  checkEquals(target, current)
}

test.double.threshold.summarise.2 <- function() {
  values <- c(0, 0.5, 0.75, 1, 1.25, 1.5, 1.65, 1.45, 1.25, 1.4, 1.6, 1.2, 1.1, 0.9, 1.2, 1, 1.3, 1.5, 1.6, 1.3, 1.1, 0.8, 1, 1.4, 1.5, 1.3, 1.2, 1.1, 0.5, 0.2, 0)
  target <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0)
  current <- as.numeric(double.threshold(values, 1.1, 1.5, on.equality=TRUE))
  checkEquals(target, current)
}

test.double.threshold.data.frame.1 <- function() {
  values <- data.frame(a=c(0, 2, 3, 2, 1, 0), b=c(0, 2, 3, 2, 1, 0))
  target <- data.frame(a=c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE), b=c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
  current <- double.threshold(values, 1, 2, TRUE)
  checkEquals(target, current)
}

test.multi.double.threshold.vector.1 <- function() {
  ## vector, four thresholds, initial TRUE.

  values <- c(1, 2, 3, 4, 5, 4, 3, 2, 1)
  thresholds <- data.frame(false=1:4-0.01, true=1:4+0.01)
  current <- multi.double.threshold(values, thresholds, TRUE)
  target <- c(1, 1, 2, 3, 4, 4, 3, 2, 1)
  checkEquals(target, current)
}

test.multi.double.threshold.data.frame.0 <- function() {
  ## data.frame, one column, four thresholds, initial default.

  values <- data.frame(a=c(1, 2, 3, 4, 5, 4, 3, 2, 1))
  thresholds <- data.frame(false=1:4-0.01, true=1:4+0.01)
  current <- multi.double.threshold(values, thresholds)
  target <- data.frame(a=c(0, 1, 2, 3, 4, 4, 3, 2, 1))
  checkEquals(target, current)
}

test.multi.double.threshold.data.frame.1 <- function() {
  ## data.frame(one column), four thresholds, initial TRUE.

  values <- data.frame(a=c(1, 2, 3, 4, 5, 4, 3, 2, 1))
  thresholds <- data.frame(false=1:4-0.01, true=1:4+0.01)
  current <- multi.double.threshold(values, thresholds, TRUE)
  target <- data.frame(a=c(1, 1, 2, 3, 4, 4, 3, 2, 1))
  checkEquals(target, current)
}

test.multi.double.threshold.data.frame.2 <- function() {
  ## data.frame(two columns), four thresholds, initial TRUE.

  values <- data.frame(a=c(1, 2, 3, 4, 5, 4, 3, 2, 1), b=c(1, 2, 3, 4, 5, 4, 3, 2, 1))
  thresholds <- data.frame(false=1:4-0.01, true=1:4+0.01)
  current <- multi.double.threshold(values, thresholds, TRUE)
  target <- data.frame(a=c(1, 1, 2, 3, 4, 4, 3, 2, 1), b=c(1, 1, 2, 3, 4, 4, 3, 2, 1))
  checkEquals(target, current)
}

## testing unexported extremes(x, count)
extremes <- delftfews:::extremes

test.extremes.too.short <- function() {
  ## x shorter than count, does not complain
  values <- c(1,2,3)
  target <- c(2, 2)
  current <- extremes(values, 8)
  checkEquals(target, current)
}

test.extremes.emtpy <- function() {
  ## x empty: returns pair of NA
  values <- c()
  target <- c(NA_real_, NA_real_)
  current <- extremes(values, 8)
  checkEquals(target, current)
}

test.extremes.correct.1 <- function() {
  ## normal usage on sorted data
  values <- c(1,2,3,4,5,6,7,8,9)
  target <- c(2, 8)
  current <- extremes(values, 3)
  checkEquals(target, current)
}

test.extremes.correct.2 <- function() {
  ## normal usage on same but unsorted data
  values <- c(7,2,8,3,1,9,6,5,4)
  target <- c(2, 8)
  current <- extremes(values, 3)
  checkEquals(target, current)
}

