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

test.rollingSum <- function(){
  input <- (1:20)
  result <- rollingSum(input, 10)
  expect <- c(NA, NA, NA, NA, NA,  NA,  NA,  NA,  NA,  55,  65,  75,  85,  95, 105, 115, 125, 135, 145, 155)
  checkEquals(length(expect), length(result))
  checkEquals(expect, result)
  
  input <- c(1, 1, 1, NA, 2, 2, 1, NA, 1, 2, 1)
  result <- rollingSum(input, 3)
  expect <- c(NA, NA, 3, 2, 3, 4, 5, 3, 2, 3, 4)
  checkEquals(length(expect), length(result))
  checkEquals(expect, result)
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

test.contiguous.stretch <- function() {
  DEACTIVATED("contiguous.stretch is not tested.")
}

test.get.step <- function() {
  DEACTIVATED("get.step is not tested.")
}

test.sum.first <- function() {
  DEACTIVATED("sum.first is not tested.")
}
