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

  ## function looks a bit clumsy because in case the input object has
  ## labels for entries, we want to replace only the values, keeping
  ## all labels in place.
  values <- as.numeric(object)
  result <- object
  L <- !is.na(values)
  result[1:length(values)] <- c(values[L][1], values[L])[1 + cumsum(L)]
  return(result)
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

rollingSum <- function(data, width, na.action=na.zero) {
  ## commodity function
  ## na.zero specifies that NA will be summed as zero.
  rollapply(na.action(data), width, FUN=sum)
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

get.step <- function(L, require.constant=FALSE)
  UseMethod('get.step')

get.step.default <- function(L, require.constant=FALSE) {
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

get.step.zoo <- function(L, require.constant=FALSE) {
  if ('frequency' %in% names(attributes(L)))
    return (1.0 / frequency(L))
  return(get.step(index(L, require.constant)))
}

sum.first <- function(input, count=12) {
  ## not exported, not tested.

  ## accepts a data.frame and returns the vector of the sum of the
  ## first 12 rows of each column
  colSums(input[1:count,])
}

double.threshold <- function(data, threshold.false, threshold.true, initial.status=FALSE, on.equality=FALSE)
  UseMethod('double.threshold')

double.threshold.default <- function(data, threshold.false, threshold.true, initial.status=FALSE, on.equality=FALSE) {
  ## double threshold test.

  ## returns a logical status vector.  at each position the status is
  ## TRUE if data exceeds the `threshold.true`, FALSE if falls below
  ## the `threshold.false`.  if the data lays between the thresholds,
  ## the last status is taken forward.  `threshold.true` must be
  ## higher than `threshold.false`.

  s <- rep(NA, length(data))
  s[1] <- initial.status
  s[data > threshold.true] <- TRUE
  s[data < threshold.false] <- FALSE
  if(on.equality==TRUE) {
    s[data == threshold.true] <- TRUE
    s[data == threshold.false] <- FALSE
  }
  L <- !is.na(s)
  s[L][cumsum(L)]
}

double.threshold.data.frame <- function(data, ...) {
  ## applies double.threshold.default to each column of the input
  ## data.frame

  data.frame(apply(data, 2, double.threshold, ...))
}

double.threshold.matrix <- function(data, ...) {
  ## applies double.threshold.default to each column of the input
  ## matrix

  apply(data, 2, double.threshold, ...)
}

multi.double.threshold <- function(data, thresholds, initial.status)
  UseMethod('multi.double.threshold')

multi.double.threshold.default <- function(data, thresholds, initial.status=FALSE) {
  ## multiple double threshold test.  similar to above
  ## double.threshold, but this one counts the amount of thresholds
  ## being exceeded.

  ## `thresholds` is a data.frame with two columns named
  ## "threshold.false" and "threshold.true".

  apply.threshold.row <- function(threshold.row, data, ...) {
    threshold.false <- threshold.row[1]
    threshold.true <- threshold.row[2]
    double.threshold(data, threshold.false, threshold.true, ...)
  }

  apply(apply(thresholds, 1, apply.threshold.row, data=data, initial.status=initial.status), 1, sum)
}

multi.double.threshold.data.frame <- function(data, ...) {
  ## applies multi.double.threshold.default to each column of the input
  ## data.frame

  data.frame(apply(data, 2, multi.double.threshold, ...))
}

multi.double.threshold.matrix <- function(data, ...) {
  ## applies multi.double.threshold.default to each column of the input
  ## matrix

  apply(data, 2, multi.double.threshold, ...)
}


extremes <- function(x, count) {
  ## returns the averages of the `count` highest and lowest values of
  ## vector `x`.

  sorted <- sort(x)
  N <- length(x)

  c(mean(sorted[1:3]), mean(sorted[(N-2):N]))
}

## not really necessary, but possibly useful
edit.zoo <- function(x) {
  cd <- coredata(x)
  rownames(cd) <- index(x)
  zoo(edit(cd), order.by=index(x))
}

findLocalMax <- function(x) c(FALSE, diff(diff(x) > 0) < 0, FALSE)

findLocalMin <- function(x) c(FALSE, diff(diff(x) > 0) > 0, FALSE)

read.sheet <- function(file, sheet=NULL, header=TRUE, sep="\t",
                       fileEncoding="", stringsAsFactors=FALSE, strip.white=TRUE, ...) {

  if(is.null(sheet))
    ## sheets may have different structures.  this function returns
    ## just one of them and you must decide which one.
    stop("you must specify the desired sheet.")

  if (is.character(file)) {
    file <- if (nzchar(fileEncoding)) 
      file(file, "rt", encoding = fileEncoding)
    else file(file, "rt")
    on.exit(close(file), add=TRUE)
  }
  if (!inherits(file, "connection")) 
    stop("'file' must be a character string or connection")
  if (!isOpen(file, "rt")) {
    open(file, "rt")
    on.exit(close(file), add=TRUE)
  }

  LinesRaw <- readLines(file)
  StartOfRequestedSheet <- grep(paste("--", sheet), LinesRaw, fixed=TRUE)
  if(length(StartOfRequestedSheet) != 1)
    stop("you cannot have more than one sheet with the same name.")
  
  SheetStartLines <- grepl("^-- ", LinesRaw)
  SheetContent <- contiguous.stretch(SheetStartLines, StartOfRequestedSheet + 1, FALSE)

  connection <- textConnection(LinesRaw[SheetContent])
  on.exit(close(connection), add=TRUE)

  read.table(connection, header=header, sep=sep,
             strip.white=strip.white, stringsAsFactors=stringsAsFactors, ...)
}

diff.zoo <- function(x, ...) {
  diff(coredata(x))
}
