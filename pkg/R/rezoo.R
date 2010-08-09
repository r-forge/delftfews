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
## initial programmer :  the zoo group
## contributors: Mario Frasca
##
## initial date       :  20100806
##

"[.zoo" <- function(x, i, j, drop = TRUE, ...)
{
  if(!is.zoo(x)) stop("method is only for zoo objects")
  x.index <- index(x)
  rval <- coredata(x)
  if(missing(i)) i <- 1:NROW(rval)

  ## also support that i can be index:
  ## if i is not numeric/integer/logical, it is interpreted to be the index
  if (all(class(i) == "logical"))
    i <- which(i)
  else if (inherits(i, "zoo") && all(class(coredata(i)) == "logical")) {
    i <- which(coredata(merge(zoo(,time(x)), i)))
  } else if(!((all(class(i) == "numeric") || all(class(i) == "integer")))) 
    i <- which(MATCH(x.index, i, nomatch = 0L) > 0L)
  
  if(length(dim(rval)) == 2) {
	drop. <- if (length(i) == 1) FALSE else drop
    rval <- if (missing(j)) rval[i, , drop = drop.] # EDITED
            else rval[i, j, drop = drop.]           # EDITED
	if (drop && length(rval) == 1) rval <- c(rval)
	rval <- zoo(rval, x.index[i])
  } else
	rval <- zoo(rval[i], x.index[i])
  class(rval) <- class(x)

  attr(rval, "oclass") <- attr(x, "oclass")
  attr(rval, "levels") <- attr(x, "levels")
  attr(rval, "frequency") <- attr(x, "frequency")
  if(!is.null(attr(rval, "frequency"))) class(rval) <- c("zooreg", class(rval))

  return(rval)
}

"[<-.zoo" <- function (x, i, j, value) 
{
  ## x[,j] <- value and x[] <- value can be handled by default method
  if(missing(i)) return(NextMethod("[<-"))

  ## otherwise do the necessary processing on i
  x.index <- index(x)
  n <- NROW(coredata(x))
  value2 <- NULL
  
  if (all(class(i) == "logical")) {
    i <- which(i)
  } else if (inherits(i, "zoo") && all(class(coredata(i)) == "logical")) {
    i <- which(coredata(merge(zoo(,time(x)), i)))
  } else if(!((all(class(i) == "numeric") || all(class(i) == "integer")))) {
    ## all time indexes in x.index?
    i.ok <- MATCH(i, x.index, nomatch = 0L) > 0L
    if(any(!i.ok)) {
      if(is.null(dim(value))) {
        value2 <- value[!i.ok]
        value <- value[i.ok]
      } else {
        value2 <- value[!i.ok,, drop = FALSE]
        value <- value[i.ok,, drop = FALSE]      
      }
      i2 <- i[!i.ok]
      i <- i[i.ok]
    }
    i <- which(MATCH(x.index, i, nomatch = 0L) > 0L)
  }
  if(any(i > n) | any(i < 1)) stop("Out-of-range assignment not possible.")
  ## taking shortcut: ([.zoo, x, , , <altered coredata>)
  coredata(x)[i, j] <- value
  ## remainder became superfluous
  return(x)
}

"$<-.zoo" <- function(object, x, value) {
  if(length(dim(object)) != 2) stop("not possible for univariate zoo series")
  if(NCOL(object) > 0 & is.null(colnames(object))) stop("only possible for zoo series with column names")
  wi <- match(x, colnames(object))
  if(is.na(wi)) {
    object <- cbind(object, value)
    if(is.null(dim(object))) dim(object) <- c(length(object), 1)
    colnames(object)[NCOL(object)] <- x  
  } else {
    if(is.null(value)) {
      object <- object[, -wi, drop = FALSE]
    } else {   
      object[, wi] <- value
    }
  }
  object
}

Ops.zoo <- function (e1, e2) 
{
  e <- if (missing(e2)) {
    NextMethod(.Generic)
  }
  else if (any(nchar(.Method) == 0)) {
    NextMethod(.Generic)
  }
  else {
    merge(e1, e2, all = FALSE, retclass = NULL)
    NextMethod(.Generic)
  }
  out <- (if (is.null(attr(e, "index"))) 
          zoo(e, index(e1), attr(e1, "frequency"))
  else
          e)
  ## the next statement is a workaround for a bug in R
  structure(out, class = class(e1))
}

"$.zoo" <- function(object, x) {
  if(length(dim(object)) != 2) stop("not possible for univariate zoo series")
  if(is.null(colnames(object))) stop("only possible for zoo series with column names")
  wi <- pmatch(x, colnames(object))
  if(is.na(wi)) NULL else object[, wi]
}
