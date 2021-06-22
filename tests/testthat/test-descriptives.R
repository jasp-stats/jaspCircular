context("Circular Descriptives")

options <- jaspTools::analysisOptions("CircularStatisticsDescriptives")
options$.meta <- list(splitby = list(shouldEncode = TRUE), variables = list(shouldEncode = TRUE))
options$plotVariables <- TRUE
options$splitby <- "Month"
options$variables <- "WindDirection"
set.seed(1)
results <- jaspTools::runAnalysis("CircularStatisticsDescriptives", "ElNino.csv", options)


test_that("August plot matches", {
  plotName <- results[["results"]][["Plots"]][["collection"]][["Plots_WindDirection"]][["collection"]][["Plots_WindDirection_August"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "august")
})

test_that("July plot matches", {
  plotName <- results[["results"]][["Plots"]][["collection"]][["Plots_WindDirection"]][["collection"]][["Plots_WindDirection_July"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "july")
})

test_that("September plot matches", {
  plotName <- results[["results"]][["Plots"]][["collection"]][["Plots_WindDirection"]][["collection"]][["Plots_WindDirection_September"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "september")
})

test_that("Circular Descriptives table results match", {
  table <- results[["results"]][["circularDescriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("August", 297.764156553961, 0, 20, 1.70812446425806, "WindDirection",
                                      "July", 256.660861072305, 0, 20, 0.449062608233807, "WindDirection",
                                      "September", 146.636758210424, 0, 20, 0.928423790991417, "WindDirection"
                                 ))
})