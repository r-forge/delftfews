require(svUnit)

test.fileName <- file.path(tempdir(), c('1', '2', '3', 'irregular'))

.setUp <- function() {
  cat('<root><element><sub id="1">7</sub><sub id="2">23</sub><ter id="1">17</ter><ter id="2">21</ter></element></root>',
      file=test.fileName[2])
  cat('<root><sub id="1" ktb="7" qlm="a"/><sub id="2" ktb="23" qlm="b"/></root>',
      file=test.fileName[1])
  cat('<root><e><id>1</id><name>text</name></e><e><id>2</id><name>due</name></e></root>',
      file=test.fileName[3])
  cat('<root><sub id="1" ktb="7" qlm="a"/><sub id="2" ktb="23" hqb="0" qlm="b"/><sub id="3" ktb="7" hqb="a"/><sub id="4" hqb="7" qlm="a"/></root>',
      file=test.fileName[4])
}

.tearDown <- function() {
  file.remove(test.fileName)
}

test.xmldoc.new <- function() {
  doc <- XmlDoc$new(test.fileName[1])

  checkTrue("XmlDoc" == class(doc))
  checkTrue("XMLInternalDocument" == class(doc$xmlDoc)[[1]])
}

test.xmldoc.getAllTextValues <- function() {
  doc <- XmlDoc$new(test.fileName[2])

  checkEquals(c("7", "23"), doc$getText("/root/element/sub"))
  checkEquals(c("17", "21"), doc$getText("/root/element/ter"))
}

test.xmldoc.getSingleAttributeValues <- function() {
  doc <- XmlDoc$new(test.fileName[1])

  checkEquals(c("7", "23"), doc$getAttribute("ktb", "/root/sub"))
  checkEquals(c("a", "b"), doc$getAttribute("qlm", "/root/sub"))
}

test.xmldoc.getSingleAttributeValuesFilter <- function() {
  doc <- XmlDoc$new(test.fileName[1])

  checkEquals("7", doc$getAttribute("ktb", "/root/sub[@id=%d]", 1))
  checkEquals("23", doc$getAttribute("ktb", "/root/sub[@id=%d]", 2))
}

test.xmldoc.getMultipleAttributeValuesSpecific <- function() {
  doc <- XmlDoc$new(test.fileName[1])

  target <- cbind(id=c("1", "2"), ktb=c("7", "23"), qlm=c("a", "b"))
  current <- doc$getAttribute(c("id", "ktb", "qlm"), "/root/sub")
  checkEquals(target, current)
}

test.xmldoc.getMultipleAttributeValuesAllUniform <- function() {
  doc <- XmlDoc$new(test.fileName[1])

  target <- cbind(id=c("1", "2"), ktb=c("7", "23"), qlm=c("a", "b"))
  current <- doc$getAttribute(TRUE, "/root/sub")
  checkEquals(target, current)
}

test.xmldoc.getMultipleAttributeValuesAllIrregular <- function() {
  doc <- XmlDoc$new(test.fileName[4])

  target <- cbind(id=c("1", "2", "3", "4"), ktb=c("7", "23", "7", NA), qlm=c("a", "b", NA, 'a'), hqb=c(NA, '0', 'a', '7'))
  target <- apply(target, c(1,2), function(x) if(!is.na(x)) x)
  current <- doc$getAttribute(TRUE, "/root/sub")
  checkEquals(target, current)
}

test.xmldoc.getMultipleChildrenValuesSpecific <- function() {
  doc <- XmlDoc$new(test.fileName[3])

  target <- cbind(id=c("1", "2"), name=c("text", "due"))
  current <- doc$getText("/root/e", children=c("id", "name"))
  checkEquals(target, current)
}

test.xmldoc.getMultipleChildrenValuesAll <- function() {
  doc <- XmlDoc$new(test.fileName[3])

  target <- cbind(id=c("1", "2"), name=c("text", "due"))
  current <- doc$getText("/root/e", children=TRUE)
  checkEquals(target, current)
}

