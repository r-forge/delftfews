#! /usr/bin/Rscript
require(svUnit)  # Needed if run from R CMD BATCH

pkg <- "delftfews"
require(delftfews)

unlink("report.xml")  # Make sure we generate a new report
mypkgSuite <- svSuiteList(pkg, dirs="../../pkg/inst/unitTest")  # List all our test suites
runTest(mypkgSuite, name = pkg)  # Run them...
## makeTestListFromExamples is in svUnit 0.7.8 or more
doRunExamples <- TRUE
svUnitVersion = as.integer(strsplit(installed.packages()[which(installed.packages()[, 'Package'] == "svUnit"), "Version"], "[\\.-]")[[1]])
if (svUnitVersion[1] == 0) {
  if (svUnitVersion[2] < 7) {
    doRunExamples <- FALSE
  } else {
    if (svUnitVersion[2] == 7)
      doRunExamples <- svUnitVersion[3] >= 8
  }
}
if(doRunExamples)
  runTest(tryCatch(makeTestListFromExamples(pkg, "../../pkg/man/"), error=function(e) NULL))
protocol(Log(), type = "junit", file = "report.xml")  # ... and write report
