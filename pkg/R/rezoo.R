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

`[.zoo` <- function(x, ..., drop = TRUE)
  tryCatch({
    rval <- zoo:::`[.zoo`(x, ..., drop=drop)
    class(rval) <- class(x)
    return(rval)
  }, error=function(e) NULL)

`$.zoo` <- function(object, x)
{
  if(x %in% colnames(object)) {
    rval <- zoo:::`[.zoo`(object, , x, drop=FALSE)
    class(rval) <- class(object)
    return(rval)
  } else {
    return(NULL)
  }
}

`$<-.zoo` <- function(object, x, value)
{
  if(length(object) == 0) {
    rval <- zoo(cbind(x=value), order.by=index(object))
    colnames(rval) <- x
    return(rval)
  }
  if(!(x %in% colnames(object))) {
    object <- cbind(object, x=value)
    colnames(object)[length(colnames(object))] <- x
    return(zoo:::`$<-.zoo`(object, x, value))
  }
  return(zoo:::`$<-.zoo`(object, x, value))
}
