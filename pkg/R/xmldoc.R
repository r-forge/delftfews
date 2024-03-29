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
                          if(missing(children)) {
                             sapply(.getNodeSet(element, ...), xmlValue)
                          } else {
                            if(length(children) == 1 && children == TRUE) {
                              children <- unique(names(xmlChildren(.getNodeSet(element, ...)[[1]])))
                            }
                            sapply(children, simplify=FALSE,
                                   function(name) sapply(.getNodeSet(paste(element, name, sep="/"), ...),
                                                         xmlValue))
                          }
                        },
                        getAttribute = function(attr, element, ..., drop=TRUE) {
                          nodeSet <- .getNodeSet(element, ...)
                          if(is.character(attr) && length(attr) == 1) {
                            result <- sapply(nodeSet, xmlGetAttr, attr)
                          } else {
                            if(length(attr) == 1 && attr == TRUE) {
                              attr <- Reduce(union, sapply(nodeSet, function(x) names(xmlAttrs(x))))
                            }
                            result <- sapply(attr,
                                             function(a)
                                             sapply(nodeSet, xmlGetAttr, a))
                          }
                          if(!isTRUE(drop) && !is.matrix(result)) {
                            resultColNames <- names(result)
                            result <- rbind(result)
                            colnames(result) <- resultColNames
                            rownames(result) <- NULL
                          }
                          return(result)
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

