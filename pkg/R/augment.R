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
## Purpose    : creating and augmenting timeseries sets
##
## initial programmer :  Mario Frasca
## contributors: Mario Frasca, Michèl van Leeuwen, 
##
## initial date       :  20091120
##

timeseries <- function(from=NULL, to=NULL, by=NULL, length.out=NULL, order.by=NULL, ...) {
  ## builds a minimal time-series-set zoo object

  if(!is.null(order.by)) {
    timestamps <- order.by
    by <- get.step(timestamps)
  } else {

    from <- as.seconds(from)
    to <- as.seconds(to)
    by <- as.seconds(by)

    if(is.null(to))
      to <- from + by * (length.out - 1)
    else if(is.null(by))
      by <- (to - from) / (length.out - 1)
    else if(is.null(from))
      from <- to - by * (length.out - 1)

    timestamps <- as.POSIXct.seconds(seq(from=from, to=to, by=by), origin=EPOCH)
  }

  result <- zoo(data.frame(...), order.by=timestamps, frequency=1.0/by)
  ## we make no use of rowname information, they only confuse our tests.
  rownames(result) <- NULL
  ## following trick allows us override specific methods
  class(result) <- c("delftfews", class(result))

  params <- list(...)
  if(length(params) == 1) {
    input <- params[[1]]
    if(is.matrix(input) || is.data.frame(input)){
      if(dim(input)[2] == 1)
        names(result) <- names(params)
      else
        names(result) <- colnames(input)
    }
  }

  return(result)
}

cumulate <- function(input, gap=1, integration.method=3, with.partials=FALSE, ...)
  UseMethod('cumulate')

cumulate.default <- function(input, ...) NULL

cumulate.zoo <- function(input, gap=1, integration.method=3, with.partials=FALSE, units="secs", ...) {
  ## given an univariate series, indexed on POSIXct time stamps,
  ## return a series set containing four columns, the netto and gross
  ## cumulative summarization of input.  the 'integration.method'
  ## refers to the overview presented in
  ## http://portal.acm.org/citation.cfm?id=578374, figure 7.2.  1:
  ## rectangular (top left), 2: rectangular (midpoint), 3: trapezoid,
  ## 4: simpson's.  the methods are implemented taking into account
  ## the two 0 measurements outside the stretch under examination.

  keys <- index(input)
  result <- zoo(cbind(gross=NA, gross.duration=NA,
                      net=NA, net.duration=NA), order.by=keys)
  if(with.partials)
    result <- cbind(result, partials=NA)

  augment <- function(start, end, type) {
    start.ts <- keys[start]
    if(end == length(input)) {
      end.ts <- keys[end] + (keys[end] - keys[end - 1])
      timestamps <- c(keys[start:end], end.ts)
    } else {
      end.ts <- keys[end+1]
      timestamps <- keys[start:(end+1)]
    }

    ## prepare local data that is used more than once
    intervals <- as.double(diff(timestamps), units=units)
    if (integration.method == 1) {
      ## rectangular, top left
      values <- input[start:end]
    } else if (integration.method == 3) {
      ## trapezoid
      values <- rollapply(c(0, input[start:end], 0), 2, mean, na.action=na.zero)[-1]
      intervals[length(intervals) + 1] <- intervals[length(intervals)]
      end <- end + 1
    } else {
      stop("integration method ", integration.method, " not implemented (yet).")
    }
    partials <- values * intervals
    
    ## '<<-' modifies surrounding environment
    result[keys[start], type] <<- sum(partials, na.rm=TRUE)
    result[keys[start], paste(type, 'duration', sep=".")] <<- as.double(end.ts - start.ts, units=units)
    if(with.partials)
      result[keys[start:end], 'partials'] <<- partials
    return(invisible())
  }

  mapply(augment,
         stretches(input, what="start", zero.surrounded=TRUE),
         stretches(input, what="end", zero.surrounded=TRUE),
         MoreArgs=list(type="net"))
  mapply(augment,
         stretches(input, gap, what="start", zero.surrounded=TRUE),
         stretches(input, gap, what="end", zero.surrounded=TRUE),
         MoreArgs=list(type="gross"))

  return(result)
}
