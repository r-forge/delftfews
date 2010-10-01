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

require(svUnit)

EPOCH <- delftfews:::EPOCH

'test.$<-.zoo.respects.derived.classes' <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  checkTrue("delftfews" %in% class(FWS))
  FWS$a <- 4:7
  checkTrue("delftfews" %in% class(FWS))
}

'test.$.zoo.respects.derived.classes' <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  checkTrue("delftfews" %in% class(FWS))
  checkTrue("delftfews" %in% class(FWS$a))
}

'test.$.zoo.does.not.drop.dimensions' <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  target.a <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, a=1)
  target.b <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, b=3)
  checkEquals(target.a, FWS$a)
  checkEquals(target.b, FWS$b)
}

'test.[.zoo.does.not.drop.dimensions' <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  target.a <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, a=1)
  target.b <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, b=3)
  checkEquals(target.a, FWS['a'])
  checkEquals(target.b, FWS['b'])
}

'test.[.zoo.respects.derived.classes' <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  checkTrue("delftfews" %in% class(FWS))
  checkTrue("delftfews" %in% class(FWS['a']))
}

test.Ops.delftfews.keeps.class <- function() {
  FWS <- timeseries(as.POSIXct(1234567800, origin=EPOCH), by=57600*60, length.out=4, l=cbind(a=1, b=3))
  FWSa <- FWS$a
  class(FWSa) <- class(FWS)
  checkTrue("delftfews" %in% class(FWSa > 0))
  checkTrue("delftfews" %in% class(FWSa - 0))
  checkTrue("delftfews" %in% class(FWSa + 0))
  checkTrue("delftfews" %in% class(FWSa * 1))
  checkTrue("delftfews" %in% class(FWSa / 1))
}
