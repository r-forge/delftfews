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
## $Id: settingsParser.R 22727 2011-07-20 10:34:46Z mario.frasca $
##
## initial programmer :  Mario Frasca
## initial date       :  20110718
##

XmlDoc <- setRefClass("XmlDoc",
                      fields = list(fileName='character', xmlDoc='XMLInternalDocument'),
                      methods = list(
                        .getNodeSet = function(element, ...) {
                          if(length(list(...)) > 0)
                            element <- sprintf(element, ...)
                          if(length(getDefaultNamespace(xmlDoc))) {
                            parts <- strsplit(element, '/', fixed=TRUE)
                            element <- paste(parts[[1]], collapse='/r:')
                          }
                          getNodeSet(xmlDoc, element, c(r = getDefaultNamespace(xmlDoc)))
                        },
                        getText = function(element, ..., children) {
                          if(missing(children))
                             sapply(.getNodeSet(element, ...), xmlValue)
                          else {
                            sapply(children,
                                   function(name) sapply(.getNodeSet(element, ...),
                                                         function(el) xmlValue(xmlElementsByTagName(el, name)[[1]])))
                          }
                        },
                        getAttribute = function(attr, element, ...) {
                          if(length(attr) == 1)
                            sapply(.getNodeSet(element, ...), xmlGetAttr, attr)
                          else {
                            sapply(attr,
                                   function(a)
                                     sapply(.getNodeSet(element, ...), xmlGetAttr, a))
                          }
                        },
                        getDoc = function() xmlDoc
                        ))

XmlDoc$methods(
               initialize = function(fileName, ...) {
                 fileName <<- fileName
                 xmlDoc <<- xmlTreeParse(fileName, useInternalNodes = TRUE)
                 callSuper()
               },
               finalize = function(...) {
                 if(!is.null(xmlDoc))
                   free(xmlDoc)
               })

