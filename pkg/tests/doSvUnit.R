#! /usr/bin/Rscript
require(svUnit)  # Needed if run from R CMD BATCH
require(delftfews)

unlink("report.xml")  # Make sure we generate a new report
mypkgSuite <- svSuiteList("delftfews", dirs="../../pkg/inst/unitTest")  # List all our test suites
runTest(mypkgSuite, name = "delftfews")  # Run them...
runTest(makeTestListFromExamples("delftfews", "../../pkg/man/"))
protocol(Log(), type = "junit", file = "report.xml")  # ... and write report
