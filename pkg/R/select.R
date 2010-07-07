##***********************************************************************
## this program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## this program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with the nens libraray.  If not, see
## <http://www.gnu.org/licenses/>.
##
## Library    : the delftfews R library
##
## Purpose    : selecting rows or columns from a timeseries set
##
## $Id$
##
## initial programmer :  Mario Frasca
## contributors: Mario Frasca, Michèl van Leeuwen, 
##
## initial date       :  20091120
##

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


timestamp.in.range <- function(data, from, to, by, units, offset) {
  ## returns array of booleans identifying rows falling in repeating
  ## intervals.  timestamps are first converted to the given units and
  ## then tested.

  ## `data` is a data.frame with a `timestamps` column holding POSIXct values.
  ## `from` and `to` have the obvious meanings.
  ## `by` is the length of the repeating interval.
  ## `units` specifies the unit used for `from`, `to`, `by`.
  ## `offset` is the value at EPOCH.

  if (to < from)
    to <- to + by
  window.width <- to - from
  values.from.from <- (as.double(difftime(data$timestamps, EPOCH, tz="UTC"), units=units) + offset - from) %% by
  return (values.from.from <= window.width)
}


timestamp.in.range.weekday <- function(data, from, to) {
  timestamp.in.range(data, from, to, 7, 'days', 4)
}


timestamp.in.weekend <- function(data) {
  timestamp.in.range.weekday(data, 6, 7)
}


timestamp.in.range.hour <- function(data, from, to) {
  timestamp.in.range(data, from, to, 24, 'hours', 0)
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


timestamp.in.range.calendar <- function(data, from, to) {
  ## returns whether the timestamps of a timeseries are between start and end date

  dates <- format.Date(data$timestamps, format="%02m%02d")

  from <- reformat.date(from)
  to <- reformat.date(to)

  if(from < to)
    result <- (dates >= from) & (dates < to)
  else
    result <- (dates >= from) | (dates < to)

  return(result)
}
