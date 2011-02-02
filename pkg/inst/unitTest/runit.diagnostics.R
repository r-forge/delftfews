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

logged <- NULL
mockAction <- function(msg, handler) {
  ## replace the timestamp with a constant string!
  parts <- unlist(strsplit(msg, " :: ", fixed=TRUE))
  parts[2] <- "TS"
  ## append the formatted message to the global 'logged' object
  logged <<- c(logged, paste(parts, collapse=' :: '))
}

test.formatter.fewsdiagnostics.simple <- function() {
  logReset()
  addHandler(mockAction, level='DEBUG', logger='', formatter=delftfews:::formatter.fewsdiagnostics)
  logged <<- NULL
  loginfo("a simple string")
  checkEquals('  <line level="3" description="LizardScripter :: TS :: a simple string"/>\n', logged)
}

test.formatter.fewsdiagnostics.entities <- function() {
  logReset()
  addHandler(mockAction, level='DEBUG', logger='', formatter=delftfews:::formatter.fewsdiagnostics)
  logged <<- NULL
  loginfo("a nasty string\"\'&<>")
  checkEquals('  <line level="3" description="LizardScripter :: TS :: a nasty string&quot;&apos;&amp;&lt;&gt;"/>\n', logged)
}

test.setup.fewsdiagnostics <- function() {
  DEACTIVATED("setup.fewsdiagnostics is not tested.")
}

test.teardown.fewsdiagnostics <- function() {
  DEACTIVATED("teardown.fewsdiagnostics is not tested.")
}
