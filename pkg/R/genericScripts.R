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
## along with the delftfews libraray.  If not, see
## <http://www.gnu.org/licenses/>.
##
## the functions in this file are useful for common scripts
## initialization.


getContainerDir <- function(fileName) {
  ## return the name of the container directory

  ## the initial substitution \\/ can be handled better in 2.13, using
  ## the new winslash parameter to normalizePath.  anyway there will
  ## be warnings in case the file does not exist.

  fileName <- gsub("\\", "/", fileName, fixed=TRUE)
  normalized <- withCallingHandlers(normalizePath(fileName),
                                    warning=function(e) {})
  parts <- strsplit(normalized, .Platform$file.sep, fixed=TRUE)[[1]]
  parts[length(parts) - 1]
}


gaCommonInitializationSteps <- function(gaDefFile =
  commandArgs(trailingOnly=TRUE)[[1]], gaExtraConfFile =
  ifelse(length(commandArgs(trailingOnly = TRUE)) > 1,
  commandArgs(trailingOnly = TRUE)[[2]], NA)) {
  ## this function alters the global environment based on the content
  ## of the FEWS GA definition file.  check the manual page for more
  ## detail and please maintain that information.
  stopifnot(require(logging),
            require(delftfews))
  log <- getLogger("fews.diagnostics")
  log$debug("gaCommonInitializationSteps starting.")

  ## gaConfDir identifies a group of general adapters.  this can be
  ## used to select among repeated entities in the optional extraConf
  ## configuration document.
  gaConfDir <- getContainerDir(gaDefFile)
  
  gar <- XmlDoc$new(gaDefFile)
  exportDir <- gsub("%WORK_DIR%", ".", gar$getText('/generalAdapterRun/general/exportDir'))
  importDir <- gsub("%WORK_DIR%", ".", gar$getText('/generalAdapterRun/general/importDir'))

  diagnosticsFileName <- gsub("%WORK_DIR%", ".", gar$getText('/generalAdapterRun/general/diagnosticFile'))
  setup.fewsdiagnostics(diagnosticsFileName)
  log$debug("gaCommonInitializationSteps created the fews diagnostics file '%s'.", diagnosticsFileName)

  inputFileName <- file.path(exportDir, gar$getText('/generalAdapterRun/activities/exportActivities/exportTimeSeriesActivity/exportFile'))
  outputFileName <- file.path(importDir, gar$getText('/generalAdapterRun/activities/importActivities/importTimeSeriesActivity/importFile'))

  inputLocationId <- gar$getText('/generalAdapterRun/activities/exportActivities/exportTimeSeriesActivity/timeSeriesSets/timeSeriesSet/locationId')
  inputParameterId <- gar$getText('/generalAdapterRun/activities/exportActivities/exportTimeSeriesActivity/timeSeriesSets/timeSeriesSet/parameterId')
  inputColumnName <- paste("lp", inputLocationId, inputParameterId, sep=".")
  
  outputLocationId <- gar$getText('/generalAdapterRun/activities/importActivities/importTimeSeriesActivity/timeSeriesSets/timeSeriesSet/locationId')
  outputParameterId <- gar$getText('/generalAdapterRun/activities/importActivities/importTimeSeriesActivity/timeSeriesSets/timeSeriesSet/parameterId')
  outputColumnName <- paste("lp", outputLocationId, outputParameterId, sep=".")

  extraConf <- NA
  if(is.na(gaExtraConfFile) && length(commandArgs(trailingOnly=TRUE)) > 2) {
    gaExtraConfFile <- commandArgs(trailingOnly=TRUE)[[2]]
  }
  if(!is.na(gaExtraConfFile)) {
    extraConf <- XmlDoc$new(gaExtraConfFile)
  }

  ## exporting information to the global environment, to make it
  ## available to the caller.
  log$debug("gaCommonInitializationSteps defining global variables.")
  assign("inputFileName", inputFileName, envir = .GlobalEnv)
  assign("outputFileName", outputFileName, envir = .GlobalEnv)
  assign("inputColumnName", inputColumnName, envir = .GlobalEnv)
  assign("outputColumnName", outputColumnName, envir = .GlobalEnv)
  assign("outputLocationId", outputLocationId, envir = .GlobalEnv)
  assign("outputParameterId", outputParameterId, envir = .GlobalEnv)
  assign("extraConf", extraConf, envir = .GlobalEnv)
  assign("log", log, envir = .GlobalEnv)
  assign("gaConfDir", gaConfDir, envir = .GlobalEnv)

  log$debug("gaCommonInitializationSteps ended successfully.")
}

blendInGlobals <- function(xmldoc=ifelse(all(exists('extraConf')), extraConf, NA), envir=.GlobalEnv, key=NA) {
  ## get all attributes in a single structure
  if (is.na(key))
    globals <- xmldoc$getText("/root/globals", children=TRUE)
  else
    globals <- xmldoc$getText("/root/globals[@id=%d]", key, children=TRUE)
  for(name in names(globals)) {
    value <- globals[[name]]
    type <- xmldoc$getAttribute(paste("/root/globals", name, sep="/"), attr="type")[[1]]
    if(is.null(type))
      type <- "character"
    value <- do.call(paste("as", type, sep='.'), list(value))
    assign(name, value, envir=envir)
  }
}
