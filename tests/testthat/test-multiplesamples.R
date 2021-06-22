context("Circular Multiple Samples")

options <- jaspTools::analysisOptions("CircularStatisticsMultipleSampleTests")
options$.meta <- list(dependent = list(shouldEncode = TRUE), fixedFactors = list(
  shouldEncode = TRUE))
options$dependent <- "WindDirection"
options$fixedFactors <- "Month"
set.seed(1)
results <- jaspTools::runAnalysis("CircularStatisticsMultipleSampleTests", "ElNino.csv", options)


test_that("One-way ANOVA table results match", {
  table <- results[["results"]][["oneWayAnovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, "Month", 4.78970330421411e-08, "Watson-Wheeler", 39.7873158803935
                                 ))
})