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

timeseries <- function(from=NULL, to=NULL, by=NULL, length.out=NULL, ...) {
  ## builds a minimal time series data.frame

  from <- as.seconds(from)
  to <- as.seconds(to)
  by <- as.seconds(by)

  if(is.null(to))
    to <- from + by * (length.out - 1)
  else if(is.null(by))
    by <- (to - from) / (length.out - 1)
  else if(is.null(from))
    from <- to - by * (length.out - 1)

  seconds <- seq(from=from, to=to, by=by)

  timestamps <- as.POSIXct.seconds(seconds, origin=EPOCH)

  data.frame(timestamps=timestamps, ...)
}

cumulate.timeseries <- function(input, column="input", gap=1, integration.method=3, units="secs") {
  ## given a timeseries set with an '"input"' column, augment it with
  ## four columns holding netto and gross cumulative summarization of
  ## input.  the 'integration.method' refers to the overview presented
  ## in http://portal.acm.org/citation.cfm?id=578374, figure 7.2.  1:
  ## rectangular (top left), 2: rectangular (midpoint), 3: trapezoid,
  ## 4: simpson's.  the methods are implemented taking into account
  ## the two 0 measurements outside the stretch under examination.

  result <- input
  result[[paste(column, 'gross', 'partials', sep='.')]] <- NA
  result[[paste(column, 'gross', 'totals', sep='.')]] <- NA
  result[[paste(column, 'gross', 'duration', sep='.')]] <- NA

  result[[paste(column, 'net', 'partials', sep='.')]] <- NA
  result[[paste(column, 'net', 'totals', sep='.')]] <- NA
  result[[paste(column, 'net', 'duration', sep='.')]] <- NA

  augment <- function(start, end, type) {
    start.ts <- input$timestamps[start]
    if(end >= nrow(input)) {
      end.ts <- input$timestamps[end] + (input$timestamps[end] - input$timestamps[end - 1])
      timestamps <- input$timestamps[start:end]
      timestamps <- c(timestamps, end.ts)
    } else {
      end.ts <- input$timestamps[end+1]
      timestamps <- input$timestamps[start:(end+1)]
    }

    ## prepare local data that is used more than once
    intervals <- as.double(diff(timestamps), units=units)
    if (integration.method == 1) {
      ## rectangular, top left
      values <- result[[column]][start:end]
    } else if (integration.method == 3) {
      ## trapezoid
      values <- rollapply(c(0, result[[column]][start:end], 0), 2, mean, na.action=na.zero)[-1]
      intervals[length(intervals) + 1] <- intervals[length(intervals)]
      end <- end + 1
    } else {
      stop("integration method ", integration.method, " not implemented (yet).")
    }
    partials <- values * intervals
    
    ## '<<-' modifies surrounding environment
    result[[paste(column, type, 'partials', sep=".")]][start:end] <<- partials
    result[[paste(column, type, 'totals', sep=".")]][start] <<- sum(partials, na.rm=TRUE)
    result[[paste(column, type, 'duration', sep=".")]][start] <<- as.double(end.ts - start.ts, units='secs')
  }

  mapply(augment,
         stretches(result[[column]], what="start", zero.surrounded=TRUE),
         stretches(result[[column]], what="end", zero.surrounded=TRUE),
         MoreArgs=list(type="net"))
  mapply(augment,
         stretches(result[[column]], gap, what="start", zero.surrounded=TRUE),
         stretches(result[[column]], gap, what="end", zero.surrounded=TRUE),
         MoreArgs=list(type="gross"))

  result
}

select.percentiles <- function(input, percentiles, score.function=sum.first, ...) {
  ## assuming 'input' contains some sort of monte carlo realizations
  ## of the same experiment in timeseries format, this function
  ## chooses the percentiles indicated, after the `score.function` function
  ## has applied to each column.

  ## first column contains timestamps, don't count it
  N <- length(input) - 1
  ## call the score.function, passing it any extra parameters
  tempdata <- score.function(input[(1:N)+1], ...)
  ## set names numerically so we can find back the columns
  names(tempdata) <- 1:N
  ## these are the columns.  don't forget skipping the timestamps
  ## column.
  columns <- as.numeric(names(sort(tempdata)[N * percentiles / 100])) + 1

  ## result has the same timestamps as input, plus the columns just
  ## chosen.
  result <- data.frame(timestamps=input$timestamps)
  result[paste(names(input)[columns], percentiles, sep='.')] <- input[columns]

  ## done
  return(result)
}
