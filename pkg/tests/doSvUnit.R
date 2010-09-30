#! /usr/bin/Rscript
require(svUnit)  # Needed if run from R CMD BATCH
require(delftfews)
unlink("../inst/RUnit/report.xml")  # Make sure we generate a new report
mypkgSuite <- svSuiteList("delftfews", dirs="../inst/RUnit")  # List all our test suites
runTest(mypkgSuite, name = "delftfews")  # Run them...
protocol(Log(), type = "junit", file = "../inst/RUnit/report.xml")  # ... and write report
