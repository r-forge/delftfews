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
## Purpose    : input-output functions relative to timeseries
##
## initial programmer :  Mario Frasca
## contributors: Mario Frasca, Michèl van Leeuwen,
##
## initial date       :  20091120
##

EPOCH <- as.POSIXct("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")
require("XML")
require("logging")

read.PI <- function(filename, step.seconds=NA, na.action=na.fill, parameterId, is.irregular=FALSE, filter.timestamp, skip.short.lived=NA) {
  ## creates a data.frame containing all data in matrix form.
  isToBeFiltered <- !missing(filter.timestamp)

  groupByStep <- function(seconds, values, step.seconds, flags=NA, missVal=NA, keepThese=TRUE) {
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
      result <- aggregate(values[keepThese], by=list(ceiling(seconds[keepThese]/step.seconds)*step.seconds), function(x) tail(x, n=1))
      colnames(result) <- c('s', "v")
    } else {
      result <- subset(data.frame(s='', v=FALSE), c(FALSE))
    }

    return (result)
  }

  ## first scans document to find all necessary timestamps, then
  ## produces empty data.frame with all timestamps found, then adds
  ## columns corresponding to each series nodes.

  ## Read XML file
  doc <- XmlDoc$new(filename)

  ## time offset in seconds from the timeZone element (which is in hours)
  timeOffset <- as.double(doc$getText("/TimeSeries/timeZone")) * 60 * 60

  ## we only operate on the "series" nodes.
  if(missing(parameterId)) {
    seriesNodes <- doc$.getNodeSet("/TimeSeries/series")
    headerNodes <- doc$.getNodeSet("/TimeSeries/series/header")
  } else {
    seriesNodes <- doc$.getNodeSet(paste("/TimeSeries/series[", paste(sprintf("./header/parameterId='%s'", parameterId), collapse=" or "), "]", sep=""))
    headerNodes <- doc$.getNodeSet(paste("/TimeSeries/series/header[", paste(sprintf("./parameterId='%s'", parameterId), collapse=" or "), "]", sep=""))
  }

  ## reset the name of the elements in seriesNodes from the general
  ## "series" to "lp".location.parameter
  lp <- sapply(c("locationId", "parameterId"),
               function(name) sapply(headerNodes, function(el) xmlValue(xmlElementsByTagName(el, name)[[1]])),
               simplify=FALSE)
  names(seriesNodes) <- paste("lp", lp$locationId, lp$parameterId, sep=".")

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

  ## finally extract all the timestamped values and fill in the blanks
  getValues <- function(node, as.zoo=FALSE) {
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

    if(!is.na(skip.short.lived)) {
      grouped <- groupByStep(seconds, values, step.seconds, flags, missVal, c(diff(seconds) > skip.short.lived, TRUE))
    } else {
      grouped <- groupByStep(seconds, values, step.seconds, flags, missVal)
    }

    ## 3106 - this is the place for filtering the data.  we just read
    ## it, modified according to the settings, but we have not yet
    ## stored it in the partial result.

    if(isToBeFiltered) {
      old.length <- length(grouped$v)
      grouped <- grouped[sapply(EPOCH + grouped$s - timeOffset, filter.timestamp), ]
      logdebug("filtering, %d/%d values survive", length(grouped$v), old.length)
    }

    if(as.zoo) {
      result <- zoo(cbind(grouped$v), order.by=EPOCH + grouped$s - timeOffset)
    } else {
      column <- rep(NA, length(result.index))
      column[as.seconds(result.index) %in% (grouped$s - timeOffset)] <- grouped$v
      result <- na.action(column)
    }
    return(result)
  }

  seconds <- sapply(seriesNodes, getElementSeconds, tagname='event')

  if(is.irregular) {
    if (is.na(step.seconds)) {  # here step.seconds means granularity and must be set.
      ## no granularity means keep data precise to the second.
      step.seconds <- 1
    }

    result <- zoo(order.by=structure(numeric(0), class=c("POSIXct", "POSIXt")))

    for(name in names(seriesNodes)) {
      logdebug("doing column '%s'", name)
      item <- getValues(seriesNodes[[name]], as.zoo=TRUE)
      if(nrow(item) == 0) {
        item <- zoo(cbind(v=NA), order.by=index(result))
      }
      colnames(item) <- name
      result <- cbind(result, item)
    }

  } else {

    if(all(lapply(seconds, length) == 0)) {
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

    if (is.na(step.seconds)) {
      step.seconds <- get.step(seconds)
    }
    startsAndEnds <- range(seconds)
    first <- startsAndEnds[1]
    last <- first + ceiling(diff(startsAndEnds) / step.seconds) * step.seconds

    ## result zoo is indexed on timestamps.  you can retrieve them using the `index` function.
    result.index <- EPOCH + seq(from=first, to=last, by=step.seconds) - timeOffset
    if(isToBeFiltered)
      result.index <- result.index[sapply(result.index, filter.timestamp)]

    ## column-bind the timestamps to the collected values
    result <- zoo(cbind(mapply(getValues, seriesNodes)), order.by=result.index, frequency=1.0/step.seconds)
  }
  
  return(result)
}

write.PI <- function(data, data.description, filename, global.data)
  ## generic function, saves a timeseriesset to a file
  UseMethod('write.PI')

write.PI.data.frame <- function(data, data.description, filename, global.data=NA) {
  data <- zoo(data[-1], order.by=data$timestamps)
  return(write.PI.zoo(data, data.description, filename, global.data=global.data))
}

write.PI.zoo <- function(data, data.description, filename, global.data=NA) {
  ## exports parts of the data data.frame to the XML filename

  ## data.description is a data.frame, looking like this:
  ##-----------------------------------------------------------
  ## type locationId  parameterId missVal units column
  ##1 instantaneous 155 Ai2 -999.0 m^3/min result
  ##2 instantaneous 156 Ai3 -999.0 mmHg check

  timeStep <- get.step(data)

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
    actualdata <- data.frame(seconds = as.seconds(index(data)))
    actualdata$column <- as.vector(data[, item[['column']]])
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

    ## timeStep
    ## Time step for typical profile if variable to be defined for the historical event.
    ## Attributes:
    ## -unit: enumeration of: second, minute, hour, day, week, nonequidistant
    ## -multiplier: defines the number of units given above in a time step (not relevant for nonequidistant time steps)
    ## -divider: same function as the multiplier, but defines fraction of units in time step.
    if(nrow(data) != nrow(actualdata)) # we removed rows
      timeStepNode <- xmlNode('timeStep', attrs=c(unit="nonequidistant"))
    else
      timeStepNode <- xmlNode('timeStep', attrs=c(unit="second", multiplier=timeStep))

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

  saveXML(TimeSeriesNode, file=filename, prefix = '<?xml version="1.0" encoding="UTF-8"?>\n')
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
  return(Q[column], Q$timestamps)
}

splitToNumeric <- function(x) {
  ## translates a string configuration submatrix to numeric

  ## not exported, tested

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
