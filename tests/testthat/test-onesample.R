context("Circular One Sample")

options <- jaspTools::analysisOptions("CircularStatisticsOneSampleTests")
options$.meta <- list(splitby = list(shouldEncode = TRUE), variables = list(shouldEncode = TRUE))
options$alphaRao <- "0.01"
options$alphaVonMises <- "0.01"
options$modifiedRayleigh <- TRUE
options$rayleigh <- TRUE
options$splitby <- "Month"
options$variables <- "WindDirection"
options$vonMisesCheck <- TRUE
options$period <- 360
options$periodGroup <- "custom"
set.seed(1)
results <- jaspTools::runAnalysis("CircularStatisticsOneSampleTests", "ElNino.csv", options)


test_that("Uniformity Tests table results match", {
  table <- results[["results"]][["oneSampleTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0.01, 176.01, "August", 142, "Rao's spacing", "WindDirection",
                                      "August", 0.34354205363543, 0.232504757290755, "Rayleigh", "WindDirection",
                                      "August", 0.751604169754176, -0.108308427549541, "V against 180",
                                      "WindDirection", 1, 0.01, 176.01, "July", 255, "Rao's spacing",
                                      "WindDirection", "July", 8.493427427151e-08, 0.90408796848175,
                                      "Rayleigh", "WindDirection", "July", 0.0944421435812171, 0.208586171979047,
                                      "V against 180", "WindDirection", 1, 0.01, 176.01, "September",
                                      190, "Rao's spacing", "WindDirection", "September", 8.57343657937474e-05,
                                      0.649868419684243, "Rayleigh", "WindDirection", "September",
                                      0.000196908407970856, 0.542770658679874, "V against 180", "WindDirection"
                                 ))
})

test_that("Von Mises Assumption Check table results match", {
  table <- results[["results"]][["vonMisesCheckTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0.01, 0.09, 0.478144574774414, "August", 0.040399719533319,
                                      "WindDirection", 0.01, 0.164, 5.50228611491871, "July", 0.0644792434618744,
                                      "WindDirection", 0.01, 0.128, 1.7314268317444, "September",
                                      0.0642929504742415, "WindDirection"))
})


test_that("Von Mises Assumption Check catches too concentrated data", {
  testthat::skip_on_os("windows")
  
  options <- analysisOptions("CircularStatisticsOneSampleTests")
  options$.meta <- list(splitby = list(shouldEncode = TRUE), variables = list(shouldEncode = TRUE))
  options$alphaRao <- "0.01"
  options$alphaVonMises <- "0.01"
  options$period <- 360
  options$periodGroup <- "custom"
  options$variables <- "contWide"
  options$vonMisesCheck <- TRUE
  
  set.seed(1)
  results <- runAnalysis("CircularStatisticsOneSampleTests", "test.csv", options)
  errorMessage <- results[["results"]][["vonMisesCheckTable"]][["error"]][["errorMessage"]]
  testthat::expect_identical(errorMessage, 
                             gettextf("Estimated %s is infinite, could not compute results. Your data is too concentrated to calculate the assumption check.", "\u03BA"))
})