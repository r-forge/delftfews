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
## Purpose    : base functions
##
## initial programmer :  Mario Frasca
##
## initial date       :  20091120
##

as.seconds <- function(value)
  ## generic function, computes the amount of seconds since EPOCH
  UseMethod('as.seconds')

as.seconds.POSIXct <- function(value) {
  as.numeric(difftime(value, EPOCH, tz="UTC"), units="secs")
}

as.seconds.Date <- function(value) {
  as.numeric(difftime(value, EPOCH, tz="UTC"), units="secs")
}

as.seconds.default <- function(value) {
  value
}

as.seconds.difftime <- function(value) {
  as.numeric(value, units="secs")
}


as.POSIXct.seconds <- function(z, ...) {
  as.POSIXct(as.numeric(z), ...)
}

na.fill <- function(object) {
  ## NA in object are replaced with observed values

  ## accepts a vector possibly holding NA values and returns a vector
  ## where all observed values are carried forward and the first is
  ## carried backward.  cfr na.locf from zoo library.
  L <- !is.na(object)
  c(object[L][1], object[L])[1 + cumsum(L)]
}

na.zero <- function(object) {
  ## NA in object are replaced with zeroes
  result <- object
  result[is.na(result)] <- 0
  result
}

na.interpolate <- function(object) 
  ## TODO
  object

stretches <- function(input, gap=1, what="start", zero.surrounded=FALSE) {
  ## returns the position of the first non-zero stretch of elements.
  ## gaps shorter than `gap` do not interrupt a stretch.
  if(zero.surrounded)
    input <- c(rep(0, gap), input, rep(0, gap))
  input <- (input != 0)
  input[is.na(input)] <- 0
  if(gap > 1)
    input <- c(rep(0, gap - 1), apply(embed(input, gap), 1, sum))
  if(what == "start")
    lookfor <- 1
  else
    lookfor <- -1
  result <- which(diff(input != 0) == lookfor) + 1
  if(length(result) == 0)
    return(result)
  if(result[1] == gap)
    result <- result[-1]
  if(what == "end")
    result <- result - gap
  if(zero.surrounded)
    result <- result - gap
  return(result)
}

rollapply <- function(data, count, fun, na.action=na.pass) {
  ## returns the rolling application of `fun` to data (nth element in
  ## returned vector is `fun` of count elements in data from n-count
  ## to n.)

  ## count must be positive.
  ## result is same length as data (starts with `count-1` NA).

  data <- na.action(data)
  len <- length(data)
  if(count < 1) {
    ## Only positive count allowed
    return(rep(NA, len))  
  }
  if(count > len) {
    rep(NA, len)
  } else {
    c( rep(NA, count - 1) , apply(embed(data, count), 1, fun) )
  }
}

rollingSum <- function(data, count, na.action=na.zero) {
  ## commodity function
  ## na.zero specifies that NA will be summed as zero.
  rollapply(data, count, fun=sum, na.action=na.action)
}

shift.vector <- function(v, by) {
  ## shifts vector by desired positions.

  ## produces a vector of same size as v, holding the same values but
  ## shifted by some integer number.  values are dropped at one end
  ## while on the other end NA are introduced.

  if (by == 0)
    v
  else if (by >= 0)
    c(NA + 1:by, v[1:(length(v) - by)])
  else
    c(v[(-by + 1):(length(v) - by)])
}

contiguous.stretch <- function(data, position, value=NULL, equality=TRUE) {
  ## zie http://stackoverflow.com/questions/2643719
  if(is.null(value))
    value <- data[position]
  if (position < 1 || position > length(data))
    return(rep(FALSE, length(data)))
  if(equality)
    data <- (data == value)
  else
    data <- (data != value)
  id <- cumsum(c(1, as.numeric(diff(data) != 0)))
  if(!data[position])
    return(rep(FALSE, length(data)))
  return(id == id[position])
}

get.step <- function(L, require.constant=FALSE) {
  ## not exported, tested.
  
  ## returns the value of the most common difference between
  ## subsequent elements.

  ## takes into account only forward steps, all negative steps are
  ## discarded.  works with list, data.frame, matrix.  if the given
  ## list contains timestamps, they are converted to seconds (since
  ## EPOCH).
  input <- unlist(sapply(as.list(L), as.numeric))
  L <- diff(as.seconds(input))
  result <- as.numeric(quantile(L[L>0], 0.5))
  if(require.constant && any(result != L))
    return(NA)
  return(result)
}

sum.first <- function(input, count=12) {
  ## not exported, not tested.
  
  ## accepts a data.frame and returns the vector of the sum of the
  ## first 12 rows of each column
  sapply(input[1:count,], sum)
}
