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

`[.zoo` <- function(x, i, j, drop = TRUE, ...)
{
  if(!is.zoo(x)) stop("method is only for zoo objects")
  rval <- coredata(x)
  n <- NROW(rval)
  n2 <- if(nargs() == 1) length(as.vector(rval)) else n
  if(missing(i)) i <- seq_len(n)

  if (inherits(i, "zoo"))
    i <- coredata(i)
  if (inherits(i, "matrix") && (ncol(i) == 1 || nrow(i) == 1))
    i <- c(i)
  ## also support that i can be index:
  ## if i is not numeric/integer/logical, it is interpreted to be the index
  if (all(class(i) == "logical"))
    i <- which(i)
  else if(!((all(class(i) == "numeric") || all(class(i) == "integer")))) 
    i <- which(MATCH(index(x), i, nomatch = 0L) > 0L)
  
  if(length(dim(rval)) == 2) {
	drop. <- if (length(i) == 1) FALSE else drop
        rval <- if (missing(j)) rval[i, , drop = drop., ...]
		else rval[i, j, drop = drop., ...]
	if (drop && length(rval) == 1) rval <- c(rval)
	rval <- zoo(rval, index(x)[i])
  } else
	rval <- zoo(rval[i], index(x)[i])

  attr(rval, "oclass") <- attr(x, "oclass")
  attr(rval, "levels") <- attr(x, "levels")
  attr(rval, "frequency") <- attr(x, "frequency")
  class(rval) <- class(x)

  return(rval)
}

`[<-.zoo` <- function (x, i, j, value) 
{
  ## x[,j] <- value and x[] <- value can be handled by default method
  if(missing(i)) return(NextMethod(`[<-`))

  ## otherwise do the necessary processing on i
  n <- NROW(coredata(x))
  n2 <- if(nargs() == 1) length(as.vector(coredata(x))) else n
  n.ok <- TRUE
  value2 <- NULL
  
  if (all(class(i) == "matrix")) i <- as.vector(i)
  if (all(class(i) == "logical")) {
    i <- which(rep(i, length.out = n2))
    n.ok <- all(i <= n2)
  } else if (inherits(i, "zoo") && all(class(coredata(i)) == "logical")) {
    i <- which(coredata(merge(zoo(,time(x)), i)))
    n.ok <- all(i <= n2)
  } else if(!((all(class(i) == "numeric") || all(class(i) == "integer")))) {
    ## all time indexes in index(x)?
    i.ok <- MATCH(i, index(x), nomatch = 0L) > 0L
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
    i <- which(MATCH(index(x), i, nomatch = 0L) > 0L)
    n.ok <- all(i <= n)
  }
  if(!n.ok | any(i < 1)) stop("Out-of-range assignment not possible.")
  rval <- NextMethod(`[<-`)

  if(!is.null(value2)) {
    rval2 <- if(missing(j)) zoo(value2, i2) else {
      value2a <- matrix(NA, nrow = length(i2), ncol = NCOL(rval))
      colnames(value2a) <- colnames(rval)
      value2a[, j] <- value2
      zoo(value2a, i2)
    }
    rval <- c(rval, rval2)
  }
  return(rval)
}

`$.zoo` <- function(object, x) {
  if(length(dim(object)) != 2) stop("not possible for univariate zoo series")
  if(is.null(colnames(object))) stop("only possible for zoo series with column names")
  wi <- pmatch(x, colnames(object))
  if(is.na(wi)) NULL else object[, wi]
}

`$<-.zoo` <- function(object, x, value) {
  if(length(dim(object)) != 2) stop("not possible for univariate zoo series")
  if(NCOL(object) > 0 & is.null(colnames(object))) stop("only possible for zoo series with column names")
  wi <- match(x, colnames(object))
  if(is.na(wi)) {
    colnames.object <- colnames(object)
    object <- zoo(cbind(coredata(object), as.vector(value)), index(object), frequency(object))
    if(is.null(dim(object))) dim(object) <- c(length(object), 1)
    colnames(object) <- c(colnames.object, x)
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
  class.e1 <- class(e1)
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
  structure(out, class = class.e1)
}
