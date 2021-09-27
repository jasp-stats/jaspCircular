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
                                 list(4, "Month", 4.78970330421411e-08, "Watson-Wheeler", 39.7873158803935)
                                 )
})

test_that("Analysis works with period of 1", {
  options <- jaspTools::analysisOptions("CircularStatisticsMultipleSampleTests")
  options$.meta <- list(dependent = list(shouldEncode = TRUE), fixedFactors = list(
    shouldEncode = TRUE))
  options$dependent <- "dependent"
  options$fixedFactors <- "factor"
  options$period <- 1
  options$watsonWilliams <- TRUE
  set.seed(1)
  data <- data.frame(dependent = rnorm(100), factor = sample(1:2, 100, TRUE))
  results <- jaspTools::runAnalysis("CircularStatisticsMultipleSampleTests", data, options)
  table <- results[["results"]][["oneWayAnovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 98, 5.17743323509441, "factor", 0.0250599641727777, "Watson-Williams",
                                      2, "factor", 0.540347848517574, "Watson-Wheeler", 1.23108436577381)
                                 )
})

test_that("Analysis throws an error for too concentrated data", {
  options <- jaspTools::analysisOptions("CircularStatisticsMultipleSampleTests")
  options$.meta <- list(dependent = list(shouldEncode = TRUE), fixedFactors = list(
    shouldEncode = TRUE))
  options$dependent <- "dependent"
  options$fixedFactors <- "factor"
  options$period <- 1
  options$watsonWilliams <- TRUE
  set.seed(1)
  data <- data.frame(dependent = 1:100, factor = sample(1:2, 100, TRUE))
  results <- jaspTools::runAnalysis("CircularStatisticsMultipleSampleTests", data, options)
  testthat::expect(results[["results"]][["error"]],
                   "Analysis failed to check for too concentrated data")
})