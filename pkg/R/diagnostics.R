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
## Purpose    : helps the logging package create fews diagnostics files.
##
## initial programmer :  Mario Frasca
## contributors: Mario Frasca, Michèl van Leeuwen, 
##
## initial date       :  20091120
##

formatter.fewsdiagnostics <- function(record) {
  if(record$level <= loglevels[['INFO']])
    level <- 3
  else if(record$level <= loglevels[['WARNING']])
    level <- 2
  else if(record$level <= loglevels[['ERROR']])
    level <- 1
  else
    level <- 0

  sprintf('  <line level="%d" description="LizardScripter :: %s :: %s"/>\n', level, record$timestamp, record$msg)
}

setup.fewsdiagnostics <- function(filename) {
  cat('<?xml version="1.0" encoding="UTF-8" standalone="yes"?> \n<Diag version="1.2" xmlns="http://www.wldelft.nl/fews/PI" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.wldelft.nl/fews/PI HTTP://fews.wldelft.nl/schemas/version1.0/pi-schemas/pi_diag.xsd">\n', file=filename, append=FALSE)
  addHandler('diagnostics',
             writeToFile, file=filename,
             logger='fews.diagnostics',
             formatter=formatter.fewsdiagnostics)
}

teardown.fewsdiagnostics <- function(filename) {
  cat('</Diag>\n', file=filename, append=TRUE)
  removeHandler('diagnostics', logger='fews.diagnostics')
}

