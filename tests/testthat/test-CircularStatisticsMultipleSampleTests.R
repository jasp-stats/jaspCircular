context("Circular Multiple Samples")

options <- jaspTools::analysisOptions("CircularStatisticsMultipleSampleTests")
options$.meta <- list(dependent = list(shouldEncode = TRUE), fixedFactors = list(
  shouldEncode = TRUE))
options$dependent <- "WindDirection"
options$fixedFactors <- "Month"
options$customPeriod <- 360
options$period <- "custom"
set.seed(1)
results <- jaspTools::runAnalysis("CircularStatisticsMultipleSampleTests", "ElNino.csv", options)


test_that("One-way ANOVA table results match", {
  table <- results[["results"]][["oneWayAnovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, "Month", 4.78970330421411e-08, "Watson-Wheeler", 39.7873158803935)
                                 )
})

test_that("Analysis works with period of 1", {
  skip("This test needs to be verified")
  options <- jaspTools::analysisOptions("CircularStatisticsMultipleSampleTests")
  options$dependent <- "dependent"
  options$fixedFactors <- "factor"
  options$customPeriod <- 1
  options$period <- "custom"
  options$watsonWilliams <- TRUE
  set.seed(1)
  data <- data.frame(dependent = rnorm(100), factor = factor(sample(1:2, 100, TRUE)))
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
  options$customPeriod <- 1
  options$period <- "custom"
  options$watsonWilliams <- TRUE
  set.seed(1)
  data <- data.frame(dependent = 1:100, factor = factor(sample(1:2, 100, TRUE)))
  results <- jaspTools::runAnalysis("CircularStatisticsMultipleSampleTests", data, options)
  testthat::expect(isTRUE(results[["results"]][["error"]]),
                   "Analysis failed to check for too concentrated data")
})

test_that("Harrison-Kanji test leaves out rows with a missing factor", {
  options <- jaspTools::analysisOptions("CircularStatisticsMultipleSampleTests")
  options$dependent <- "angle"
  options$fixedFactors <- c("a", "b")
  options$period <- "custom"
  options$customPeriod <- 360
  options$harrisonKanji <- TRUE
  # 12 complete rows, then 2 rows missing only b (with a level of a that occurs nowhere else),
  # a row missing only a, and a row missing both.
  data <- data.frame(
    angle = c(20, 45, 70, 300, 340, 10, 120, 160, 95, 200, 250, 175, 30, 60, 290, 180),
    a     = factor(c("x", "x", "x", "x", "x", "x", "y", "y", "y", "y", "y", "y", "z", "z", NA, NA)),
    b     = factor(c("u", "u", "u", "v", "v", "v", "u", "u", "u", "v", "v", "v", NA, NA, "v", NA))
  )
  results <- jaspTools::runAnalysis("CircularStatisticsMultipleSampleTests", data, options)
  table <- results[["results"]][["twoWayAnovaTable"]][["data"]]
  # Reference values: the Harrison-Kanji formulas computed directly on the 12 complete rows.
  jaspTools::expect_equal_tables(table,
                                 list(11.5971401753832, 2, "a", 0.00303188697979009,
                                      7.09871323102259, 2, "b", 0.0287431265883022,
                                      0.266046761582614, 1, "Interaction", 0.605996051562103)
                                 )
})
