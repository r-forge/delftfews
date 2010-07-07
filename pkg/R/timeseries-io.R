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
## Library    : the delftfews R library
##
## Purpose    : input-output functions relative to timeseries
##
## $Id$
##
## initial programmer :  Mario Frasca
## contributors: Mario Frasca, Michèl van Leeuwen, 
##
## initial date       :  20091120
##

EPOCH <- as.POSIXct("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")
require("XML")
require("logging")

read.PI <- function(filename, step.seconds=NA, na.action=na.fill) {
  ## creates a data.frame containing all data in matrix form.

  groupByStep <- function(seconds, values, step.seconds, flags=NA, missVal=NA) {
    ## groups the values by seconds, one each step.
    ##
    ## a value may be NA, or be flagged as NA (flag 9), or be equivalent
    ## to NA (equal to missVal)
    
    ## if more values are given in one step, the last is returned
    
    if(!any(is.na(missVal)))  # missing value was specified
      values[values == missVal] <- NA
    if(!any(is.na(flags)))  # a flags array was provided
      values[flags == 9] <- NA

    if(length(!is.na(values)) > 0) {
      result <- aggregate(values, by=list(ceiling(seconds/step.seconds)*step.seconds), function(x) tail(x, n=1))
      colnames(result) <- c('s', "v")
    } else {
      result <- data.frame(v=FALSE)
      result <- subset(result, v)
    }

    return (result)
  }

  ## first scans document to find all necessary timestamps, then
  ## produces empty data.frame with all timestamps found, then adds
  ## columns corresponding to each series nodes.

  ## Read XML file
  doc <- xmlTreeParse(filename)
  TimeSeriesNode <- xmlRoot(doc)

  ## we only operate on the "series" nodes.

  seriesNodes <- xmlElementsByTagName(TimeSeriesNode, "series")

  ## reset the name of the elements in seriesNodes from the general
  ## "series" to "lp".location.parameter
  getSeriesName <- function(node) {
    headerValues <- xmlSApply(node[["header"]], xmlValue)
    paste("lp", headerValues$locationId, headerValues$parameterId, sep=".")
  }
  names(seriesNodes) <- mapply(getSeriesName, seriesNodes)

  ## scan the document to find the first and the last (step-valid)
  ## timestamps.  use them to initialize the result data.frame.

  getElementSeconds <- function(node, tagname) {
    ## returns the timestamps in seconds since EPOCH for the given node
    eventNodeList <- xmlElementsByTagName(node, tagname)
    dates <- sapply(eventNodeList, xmlGetAttr, name = "date")
    times <- sapply(eventNodeList, xmlGetAttr, name = "time")
    timestamps <- as.POSIXct(paste(dates, times), "%Y-%m-%d %H:%M:%S", tz="UTC")
    as.numeric(difftime(timestamps, EPOCH, tz="UTC"), units="secs")
  }

  seconds <- sapply(seriesNodes, getElementSeconds, tagname='event')

  if(max(sapply(seconds, length)) == 0) {
    ## we have only series without events, so we make a fictive
    ## `seconds` list from startDate, endDate and `step.seconds`.
    if (is.na(step.seconds))
      stop("no events, we really need `step.seconds`.")
    getHeaderSeconds <- function(node, tagname) {
      headerNodeList <- xmlElementsByTagName(node, 'header')
      startDate <- sapply(headerNodeList, getElementSeconds, tagname='startDate')
      endDate <- sapply(headerNodeList, getElementSeconds, tagname='endDate')
      seq(from=startDate, to=endDate, by=step.seconds)
    }
    seconds <- sapply(seriesNodes, getHeaderSeconds)
  }

  if (is.na(step.seconds))
    step.seconds <- get.step(seconds)

  startsAndEnds <- range(seconds)
  first <- floor(startsAndEnds[1]/step.seconds)*step.seconds
  last <- ceiling(startsAndEnds[2]/step.seconds)*step.seconds

  ## result data.frame holds one $timestamps column, meant for both
  ## computation and human readability of the data.
  result <- data.frame(timestamps=EPOCH + seq(from=first, to=last, by=step.seconds))

  ## finally extract all the timestamped values and fill in the blanks

  getValues <- function(node) {
    ## get the values as a named column
    header <- xmlElementsByTagName(node, "header")[[1]]
    missVal <- xmlElementsByTagName(header, 'missVal')[[1]]
    missVal <- as.numeric(xmlValue(missVal))

    eventNodeList <- xmlElementsByTagName(node, "event")
    dates <- sapply(eventNodeList, xmlGetAttr, name = "date")
    times <- sapply(eventNodeList, xmlGetAttr, name = "time")
    values <- as.numeric(sapply(eventNodeList, xmlGetAttr, name = "value"))
    flags <- as.numeric(sapply(eventNodeList, xmlGetAttr, name = "flag"))

    timestamps <- as.POSIXct(paste(dates, times), "%Y-%m-%d %H:%M:%S", tz="UTC")
    seconds <- as.numeric(difftime(timestamps, EPOCH, tz="UTC"), units="secs")

    grouped <- groupByStep(seconds, values, step.seconds, flags, missVal)
    column <- rep(NA, length(result$timestamps))
    column[as.seconds(result$timestamps) %in% grouped$s] <- grouped$v
    na.action(column)
  }

  ## column-bind the timestamps to the collected values
  cbind(result, mapply(getValues, seriesNodes))
}

write.PI <- function(data, data.description, filename, global.data=NA) {
  ## exports parts of the data data.frame to the XML filename

  ## data.description is a data.frame, looking like this:
  ##-----------------------------------------------------------
  ## type locationId  parameterId missVal units column
  ##1 instantaneous 155 Ai2 -999.0 m^3/min result
  ##2 instantaneous 156 Ai3 -999.0 mmHg check

  timeStep <- get.step(data$timestamps)

  looksLikeNULL <- function(object, name) {
    if(!(name %in% names(object)))
      return(TRUE)
    if(is.na(object[[name]]))
      return(FALSE)
    if(is.null(object[[name]]))
      return(TRUE)
    if(object[[name]] == 'NULL')
      return(TRUE)
    return(FALSE)
  }

  CreateSeriesNode <- function(item) {
    ## gets a configuration item (item is a row of data.description)
    ## and returns the corresponding 'series' node, filled with
    ## 'event' nodes

    ## cut uninteresting columns
    actualdata <- data.frame(seconds = as.seconds(data[[1]]))
    actualdata$column <- data[[ item[['column']] ]]
    ## cut rows that generate no 'event' node.
    if(looksLikeNULL(item, 'missVal')) {
      actualdata <- subset(actualdata, !is.na(column))
    }
    if(looksLikeNULL(item, 'InfVal')) {
      actualdata <- subset(actualdata, !is.infinite(column))
    }

    if(length(actualdata$column) == 0)
      return(invisible())

    ## what are the true start and end instants of this series?
    start <- EPOCH + min(actualdata$seconds)
    end <- EPOCH + max(actualdata$seconds)

    tsDate <- function(x) { format.POSIXct(x, format="%Y-%m-%d") }
    tsTime <- function(x) { format.POSIXct(x, format="%H:%M:%S") }

    CreateSeriesEventNode <- function(i) {
      ## gets a timestamped value and returns an xmlNode
      ## it is called by an `apply` by row, so gets arguments as strings.
      ts <- EPOCH + as.numeric(i[1])
      value <- as.numeric(i[2])

      missingValueNode <- function(name) {
        if(any(is.na(data.description[[name]])))
          ## write 0 and flag the value to "9"
          xmlNode(name = 'event', attrs=c(date=tsDate(ts), time=tsTime(ts), value=0, flag=9))
        else
          ## use the given special missing value
          xmlNode(name = 'event', attrs=c(date=tsDate(ts), time=tsTime(ts), value=item[[name]], flag=8))
      }

      if(is.infinite(value))
        missingValueNode('InfVal')
      else if(is.na(value))
        missingValueNode('missVal')
      else 
        xmlNode(name = 'event', attrs=c(date=tsDate(ts), time=tsTime(ts), value=value, flag=0))
    }

    if(length(data[[1]]) != length(actualdata[[1]])) # we removed rows
      timeStepNode <- xmlNode('timeStep', attrs=c(unit="nonequidistant"))
    else
      timeStepNode <- xmlNode('timeStep', attrs=c(unit="seconds", multiplier=timeStep))

    headerNode <- xmlNode('header',
                          xmlNode('type', item[['type']]),
                          xmlNode('locationId', item[['locationId']]),
                          xmlNode('parameterId', item[['parameterId']]),
                          timeStepNode,
                          xmlNode('startDate', attrs=c(date=tsDate(start), time=tsTime(start))),
                          xmlNode('endDate', attrs=c(date=tsDate(end), time=tsTime(end)))
                          )

    for (name in c('missVal', 'longName','stationName', 'units'))
      if (!looksLikeNULL(item, name) & !is.na(item[name]))
        headerNode <- addChildren(headerNode, kids=list(xmlNode(name, item[[name]])))

    for (name in colnames(global.data))
      if (!is.na(global.data[1, name]) & !sum(grep('\\.', name)))
          headerNode <- addChildren(headerNode, kids=list(xmlNode(name, global.data[1, name])))

    seriesNode <- xmlNode('series', headerNode)
    seriesNode <- addChildren(seriesNode, kids=apply(actualdata, 1, CreateSeriesEventNode))
    return(seriesNode)
  }

  TimeSeriesNode <- xmlNode('TimeSeries',
                            attrs = c(
                              'xmlns' = "http://www.wldelft.nl/fews/PI",
                              'xmlns:xsi' = "http://www.w3.org/2001/XMLSchema-instance",
                              'xsi:schemaLocation' = "http://www.wldelft.nl/fews/PI http://fews.wldelft.nl/schemas/version1.0/pi-schemas/pi_timeseries.xsd",
                              'version' = "1.2"),
                            xmlNode('timeZone', list("0.0")))

  if(any(!(data.description$column %in% names(data)))) {
    logwarn("removing data.descriptions rows not available in the data")
    data.description <- subset(data.description, column %in% names(data))
  }
  TimeSeriesNode <- addChildren(TimeSeriesNode, kids=apply(data.description, 1, CreateSeriesNode))

  saveXML(TimeSeriesNode, file=filename)
}

read.BfG <- function(filename, column="value") {
  ## creates a data.frame containing all data in matrix form.

  ## Read data file
  Q <- read.table(filename, skip = 4)
  ## assign names to columns
  names(Q) <- c("year", "month", "day", "hour", column)

  dates <- paste(Q$year, sprintf("%02.0f",Q$month), sprintf("%02.0f",Q$day), sep = "-")
  times <- paste(sprintf("%02.0f", Q$hour), "00", "00", sep = ":")

  ## create the columns expected in our format
  Q$timestamps <- as.POSIXct(paste(dates, times), "%Y-%m-%d %H:%M:%S", tz="UTC")

  ## remove the original first four columns, set 'timestamps' as the first column.
  return(Q[c('timestamps', column)])
}

splitToNumeric <- function(x) {
  ## translates a string configuration submatrix to numeric
  
  ## not exported

  ow <- options("warn")
  options(warn=-1)
  result <- apply(x[,2:ncol(x), drop=FALSE], 1:2, as.numeric)
  options(ow)

  if (any(is.na(result)))
    return(as.data.frame(x, stringsAsFactors=FALSE))

  rownames(result) <- x[,1]
  result <- as.data.frame(result, stringsAsFactors=FALSE)
  names(attributes(result)$row.names) <- NULL
  return(result)
}

splitDcfMatrix <- function(x) {
  ## splits a debian configuration file matrix in components.

  ## returns a list of named string matrices, still to be parsed.

  ## not exported

  result <- list()
  while(sum(dim(x)) != 0) {
    cols <- which(!is.na(x[1,]))
    rows <- which(!is.na(x[,1]))
    result[[length(result) + 1]] <- rbind(x[rows, cols, drop=FALSE])
    x <- x[-rows, -cols, drop=FALSE]
  }

  names(result) <- unlist(lapply(X=result, FUN=function(x) colnames(x)[[1]]))
  result
}

parseSplitDcf <- function(x) {
  ## parses (forces to numeric) and splits a dcf matrix
  ##
  ## not exported
  lapply(splitDcfMatrix(x), splitToNumeric)
}

read.dcf.parsed <- function(filename) {
  ## reads a debian configuration file and splits it in components
  parseSplitDcf(read.dcf(filename))
}