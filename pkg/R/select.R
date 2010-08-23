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
## Purpose    : selecting rows or columns from a timeseries set
##
## initial programmer :  Mario Frasca
## contributors: Mario Frasca, Michèl van Leeuwen, 
##
## initial date       :  20091120
##

timestamp.in.range <- function(data, from, to, by, units, offset, tz="UTC") {
  ## returns array of booleans identifying rows falling in repeating
  ## intervals.  timestamps are first converted to the given units and
  ## then tested.

  ## `data` is a data.frame with a `timestamps` column holding POSIXct values.
  ## `from` and `to` have the obvious meanings.  (`from` is included, `to` is excluded.)
  ## `by` is the length of the repeating interval.
  ## `units` specifies the unit used for `from`, `to`, `by`.
  ## `offset` is the value at EPOCH.

  if (to < from)
    to <- to + by
  window.width <- to - from
  
  ## we want to check the 'numerical' parts only, when the timestamp
  ## is expressed in the timezone specified.  to do so we print the
  ## timestamps in the timezone specified, remove the timezone
  ## information and do all calculations as if we were living in UTC.
  ## it's a cheap trick, you don't need tell me.
  timestamps.as.string <- format(index(data), tz=tz)
  fictive.timestamps <- as.POSIXct(timestamps.as.string, tz="UTC")
  values.from.from <- (as.double(difftime(fictive.timestamps, EPOCH), units=units) + offset - from) %% by
  return (values.from.from < window.width)
}


timestamp.in.range.weekday <- function(data, from, to, tz="CET") {
  timestamp.in.range(data, from, to, 7, 'days', 4, tz=tz)
}


timestamp.in.weekend <- function(data, tz="CET") {
  ## all equivalent:
  ## timestamp.in.range.weekday(data, 6, 8, tz=tz) # Sat->Mon(next week)
  ## timestamp.in.range.weekday(data, 6, 1, tz=tz) # Sat->Mon
  ## !timestamp.in.range.weekday(data, 1, 6, tz=tz) # not Mon->Sat
  ## we use "not from monday (included) to saturday (excluded)"
  !timestamp.in.range.weekday(data, 1, 6, tz=tz) 
}


timestamp.in.range.hour <- function(data, from, to, tz="CET") {
  timestamp.in.range(data, from, to, 24, 'hours', 0, tz=tz)
}


reformat.date <- function(datestring) {
  ## transforms date with some separator to MMDD
  ##

  parts <- strsplit(datestring, '[/-]')[[1]]
  if(length(parts) == 1) {
    if(nchar(datestring) != 4)
      stop("invalid date string")
    return(datestring)
  }
  return(paste(sprintf("%02d", as.numeric(parts)), collapse=''))
}


timestamp.in.range.calendar <- function(data, from, to, tz="CET") {
  ## returns whether the timestamps of a timeseries are between start and end date

  dates <- format.Date(index(data), format="%02m%02d")

  from <- reformat.date(from)
  to <- reformat.date(to)

  if(from < to)
    result <- (dates >= from) & (dates < to)
  else
    result <- (dates >= from) | (dates < to)

  return(result)
}


select.percentiles <- function(input, percentiles, score.function=sum.first, ...) {
  ## assuming 'input' contains some sort of monte carlo realizations
  ## of the same experiment in timeseries format, this function
  ## chooses the percentiles indicated, after the `score.function` function
  ## has applied to each column.

  ## call the score.function, passing it any extra parameters
  tempdata <- score.function(input, ...)

  ## if no valid data was provided, return no valid data!
  if(all(is.na(tempdata))) {
    ## return the desired amount of columns, filled with NA and
    ## timestamped as the input.
    columns <- seq_len(length(percentiles))
    result <- input[, columns]
    result[] <- NA
  } else {
    ## skip all columns where the score is NA
    input <- input[, !is.na(tempdata)]
    tempdata <- tempdata[!is.na(tempdata)]
    ## and now count the surviving columns
    N <- length(tempdata)
    ## set unique names so we can find back each individual column after
    ## ordering by score
    names(tempdata) <- seq_len(N)
    ## these are the columns.  
    columns <- as.numeric(names(sort(tempdata)[N * percentiles / 100]))
    ## result has same timestamps as input, but only the chosen
    ## columns
    result <- input[, columns]
  }
  ## force result to have the same class as the input
  class(result) <- class(input)
  ## rename columns adding a trailing .percentile
  colnames(result) <- paste(names(input)[columns], percentiles, sep='.')

  ## done
  return(result)
}

"[.delftfews" <- function(x, i, j, drop = FALSE, ...) {
  class.x <- class(x)
  try({
    result <- (if (missing(i))
               NextMethod(drop=drop)
    else if (missing(j) && is.character(i))
               NextMethod(i=seq_len(NROW(x)), drop=drop)
    else
               NextMethod(drop=drop))
    class(result) <- class.x
    return(result)
  }, silent=TRUE)
  return(NULL)
}

"[<-.delftfews" <- function(x, i, j, value) {
  class.x <- class(x)
  result <- (if (missing(i))
             NextMethod()
  else if (missing(j) && is.character(i))
             do.call("$<-.zoo", list(object=x, x=i, value=value))
  else
             NextMethod())
  class(result) <- class.x
  return(result)
}

'$.delftfews' <- function(object, ...) {
  class.object <- class(object)
  result <- NextMethod()
  if(!is.null(result)) class(result) <- class.object
  return(result)
}

'$<-.delftfews' <- function(object, ..., value) {
  class.object <- class(object)
  result <- NextMethod()
  class(result) <- class.object
  return(result)
}
