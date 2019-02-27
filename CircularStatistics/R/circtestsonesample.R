CircularStatisticsOneSampleTests <- function(jaspResults, dataset, options, ...) {
  # Get the correct period. This step is neccessary since pi is hard to specify in the GUI
  if (options$periodGroup == "pi")
    options$period <- pi
  if (options$periodGroup == "pi_2")
    options$period <- 2 * pi

  # Set title
  jaspResults$title <- "Test Results"

  # Read dataset
  dataset <- .circularTestsOneSampleReadData(dataset, options)

  # Error checking
  #errors <- .circularTestsOneSampleCheckErrors(dataset, options)

  circularTestsOneSampleResults <- .circularTestsOneSampleComputeResults(jaspResults, dataset, options)
  if(options$vonMisesCheck)
    circularTestsOneSampleVonMisesResults <- .circularTestsOneSampleComputeResultsVonMises(jaspResults, dataset, options)

  # Output tables and plots
  .circularTestsOneSampleCreateTable(jaspResults, dataset, options, circularTestsOneSampleResults)
  if(options$vonMisesCheck)
    .circularTestsOneSampleCreateTableVonMises(jaspResults, dataset, options, circularTestsOneSampleVonMisesResults)
}

# Preprocessing functions ----
.circularTestsOneSampleReadData <- function(dataset, options) {
  variables <- unlist(options$variables)
  splitName <- options$splitby
  wantsSplit <- splitName != ""
  if (wantsSplit) {
    dataset <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = splitName)
  } else {
    dataset <- .readDataSetToEnd(columns.as.numeric = variables)
  }
  return(dataset)
}
.circularTestsOneSampleCheckErrors <- function(dataset, options){
}
# Results functions ----
.circularTestsOneSampleComputeResults <- function(jaspResults, dataset, options) {
  splitName <- options$splitby
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)

  results <- list()

  if(wantsSplit){
    split <- dataset[[.v(options$splitby)]]
    splitLevels <- levels(split)
    for(variable in variables){
      for(level in splitLevels){
        column <- dataset[[.v(variable)]][split == level]
        validData <- column[!is.na(column)]
        validDataNormalized <- .normalizeData(validData, options$period)
        validDataCirc <- circular::circular(validDataNormalized)

        if (options$rao){
          testResults <- .circularTestsOneSampleComputeResultsRao(jaspResults, validDataCirc, options)
          results[[variable]][[level]][["rao"]] <- list(
            variable = variable,
            level = level,
            testName = "Rao's Spacing",
            p = testResults$p,
            statistic = testResults$statistic,
            criticalValue = testResults$criticalValue,
            n = testResults$n
          )
        }
        if (options$rayleigh){
          testResults <- .circularTestsOneSampleComputeResultsRayleigh(jaspResults, validDataCirc, options)
          results[[variable]][[level]][["rayleigh"]] <- list(
            variable = variable,
            level = level,
            testName = "Rayleigh",
            p = testResults$p,
            statistic = testResults$statistic
          )
        }
        if (options$modifiedRayleigh){
          testResults <- .circularTestsOneSampleComputeResultsModifiedRayleigh(jaspResults, validDataCirc, options)
          results[[variable]][[level]][["modifiedRayleigh"]] <- list(
            variable = variable,
            level = level,
            testName = paste("V against", toString(options$testValue)),
            p = testResults$p,
            statistic = testResults$statistic
          )
        }
      }
    }

  }
  else {
    for (variable in variables){
      column <- dataset[[.v(variable)]]
      validData <- column[!is.na(column)]
      validDataNormalized <- .normalizeData(validData, options$period)
      validDataCirc <- circular::circular(validDataNormalized)

      if (options$rao){
        testResults <- .circularTestsOneSampleComputeResultsRao(jaspResults, validDataCirc, options)
        results[[variable]][["rao"]] <- list(
          variable = variable,
          testName = "Rao's Spacing",
          p = testResults$p,
          statistic = testResults$statistic,
          criticalValue = testResults$criticalValue,
          n = testResults$n
        )
      }
      if (options$rayleigh){
        testResults <- .circularTestsOneSampleComputeResultsRayleigh(jaspResults, validDataCirc, options)
        results[[variable]][["rayleigh"]] <- list(
          variable = variable,
          testName = "Rayleigh",
          p = testResults$p,
          statistic = testResults$statistic
        )
      }
      if (options$modifiedRayleigh){
        testResults <- .circularTestsOneSampleComputeResultsModifiedRayleigh(jaspResults, validDataCirc, options)
        results[[variable]][["modifiedRayleigh"]] <- list(
          variable = variable,
          testName = paste("V against", toString(options$testValue)),
          p = testResults$p,
          statistic = testResults$statistic
        )
      }
    }
  }

  return(results)
}
.circularTestsOneSampleComputeResultsRao <- function(jaspResults, data, options) {
  # the test will always run with a p value of 0.01.
  # TODO (abahde): make it possible to specify the p-value of the test as 0.001, 0.01, 0.05, or 0.1 in the GUI
  p <- 0.01
  testResult <- circular::rao.spacing.test(data,alpha=p)
  U <- testResult$statistic
  n <- testResult$n
  # the following is a routine that gets the critical value for the specified p value, depending in the dataset size.
  # it is neccesary, since the circular package just prints the test results without returning the critical values.
  data(rao.table, package = "circular", envir = sys.frame(which = sys.nframe()))    # table for critical values (Levitin, Rusell 1995)
  criticalTableColumn <- (1:4)[p == c(0.001, 0.01, 0.05, 0.1)]
  # get the table row where the data count is as closest to the one in the table
  countColumn <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,35,40,45,50,75,100,150,200,300,400,500,600,700,800,900,1000)
  criticalTableRow <- which(abs(countColumn-n)==min(abs(countColumn-n)))
  criticalValue <- rao.table[criticalTableRow, criticalTableColumn]
  results <- list(p = p, statistic = U, criticalValue = criticalValue, n=n)
}
.circularTestsOneSampleComputeResultsRayleigh <- function(jaspResults, data, options) {
  testResult <- circular::rayleigh.test(data)
  p <- testResult$p.value
  statistic <- testResult$statistic
  results <- list(p = p, statistic = statistic)
  print(p)
return(results)
}
.circularTestsOneSampleComputeResultsModifiedRayleigh <- function(jaspResults, data, options) {
  # we do not trust the implementation of the V test in the circular package. Therefore, we implement it on our own following Fisher 1993, p.69.
  # the test statistic is here based on the mean resultant length and not on "rayleighs R". Therefore the resulting statistic differs by a factor of n from other libraries, like CircStat for MATLAB

  # calculate the statistic r0Bar
  n <- length(data)
  mu0 <- .normalizeData(options$testValue, options$period)    # test direction must be normalized as well
  meanDirection <- as.numeric(circular::mean.circular(data))
  meanResultantLength <- as.numeric(circular::rho.circular(data))
  r0Bar <- meanResultantLength * cos(meanDirection - mu0)

  # determine p-value
  z0 <- sqrt(2 * n) * r0Bar
  pz <- pnorm(z0)
  fz <- dnorm(z0)
  p <- 1 - pz + fz * ((3 * z0 - z0^3)/(16 * n) + (15 * z0 + 305 * z0^3 - 125 * z0^5 + 9 * z0^7)/(4608 * n^2))

  results <- list(p = p, statistic = r0Bar)
  return(results)
}
.circularTestsOneSampleComputeResultsVonMises <- function(jaspResults, dataset, options){
  splitName <- options$splitby
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)

  circularTestsOneSampleVonMisesTestResults <- list()

  if (wantsSplit){
    split       <- dataset[[.v(options$splitby)]]
    splitLevels <- levels(split)
    for(variable in variables){
      for(level in splitLevels){
        column    <- dataset[[.v(variable)]][split == level]
        validData <- column[!is.na(column)]
        validDataNormalized <- .normalizeData(validData, options$period)
        validDataCirc <- circular::circular(validDataNormalized)
        testResults <- .circularTestsOneSampleComputeResultsVonMisesSub(validDataCirc, options, "vonmises")
        circularTestsOneSampleVonMisesTestResults[[variable]][[level]] <- list(
          variable = variable,
          level = level,
          statistic = testResults$statistic,
          critical = testResults$critical,
          p = testResults$p,
          kappa = testResults$kappa
        )
      }
    }
  } else {
    for (variable in variables){
      column <- dataset[[.v(variable)]]
      validData <- column[!is.na(column)]
      validDataNormalized <- .normalizeData(validData, options$period)
      validDataCirc <- circular::circular(validDataNormalized)
      testResults <- .circularTestsOneSampleComputeResultsVonMisesSub(validDataCirc, options, "vonmises")
      circularTestsOneSampleVonMisesTestResults[[variable]] <- list(
        variable = variable,
        statistic = testResults$statistic,
        critical = testResults$critical,
        p = testResults$p,
        kappa = testResults$kappa
      )
    }
  }
  return(circularTestsOneSampleVonMisesTestResults)
}
.circularTestsOneSampleComputeResultsVonMisesSub <- function(data, options, dist = "vonmises"){
  # at the moment we only support this assumption check for a p-value of 0.01 (it is called "alpha" in the circular package)
  alpha <- 0.01

  # Get the estimated kappa for the footnote. If kappa is too small, the data might be rather uniform.
  kappa <- circular::mle.vonmises(data, bias = FALSE)$kappa

  testResult <- circular::watson.test(data, alpha = alpha, dist = dist)
  row <- testResult$row

  statistic <- testResult$statistic

  # this table comparison is copied from circular::print.watson.test() to get the values instead of just printing them to the console.
  u2.crits <- cbind(c(0, 0.5, 1, 1.5, 2, 4, 100),
                    c(0.052, 0.056, 0.066, 0.077, 0.084, 0.093, 0.096),
                    c(0.061,0.066, 0.079, 0.092, 0.101, 0.113, 0.117),
                    c(0.081, 0.09, 0.11, 0.128, 0.142, 0.158, 0.164))

  if (alpha == 0.1)
    col <- 2
  else if (alpha == 0.05)
    col <- 3
  else if (alpha == 0.01)
    col <- 4

  critical <- u2.crits[row, col]

  return(list(critical = critical, statistic = statistic, p = alpha, kappa = kappa))
}
# Output functions ----
.circularTestsOneSampleCreateTable <- function(jaspResults, dataset, options, circularTestsOneSampleResults) {
  splitName <- options$splitby
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)

  # Create table
  oneSampleTable <- createJaspTable(title = "Uniformity Tests")
  jaspResults[["oneSampleTable"]] <- oneSampleTable
  jaspResults[["oneSampleTable"]]$dependOnOptions(c("variables", "splitby", "rayleigh", "modifiedRayleigh", "period"))

  oneSampleTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  oneSampleTable$addColumnInfo(name = "variable",   title = "Variable",   type = "string", combine = TRUE)
  if (wantsSplit)
    oneSampleTable$addColumnInfo(name = "level",   title = "Level",   type = "string", combine = TRUE)

  oneSampleTable$addColumnInfo(name = "testName",   title = "Test",   type = "string")
  oneSampleTable$addColumnInfo(name = "p",   title = "p",   type = "number", format = "dp:3;p:.001")
  oneSampleTable$addColumnInfo(name = "statistic",   title = "Statistic",   type = "number", format = "dp:3")
  if (options$rao)
    oneSampleTable$addColumnInfo(name = "criticalValue",   title = "Critical",   type = "number", format = "dp:3")
  if (wantsSplit){
    split <- dataset[[.v(options$splitby)]]
    splitLevels <- levels(split)

    rowNamesForRaoFootnote <- c()

    for(variable in variables){
      for(level in splitLevels){
        if (options$rao){
          row <- circularTestsOneSampleResults[[variable]][[level]][["rao"]]
          rowName <- paste(variable, level, "rao")
          oneSampleTable$addRows(row, rowNames = rowName)

          rowNamesForRaoFootnote <- c(rowNamesForRaoFootnote, rowName)
          }
        if (options$rayleigh){
          row <- circularTestsOneSampleResults[[variable]][[level]][["rayleigh"]]
          oneSampleTable$addRows(row, rowNames = paste(variable))
        }
        if (options$modifiedRayleigh){
          row <- circularTestsOneSampleResults[[variable]][[level]][["modifiedRayleigh"]]
          oneSampleTable$addRows(row, rowNames = paste(variable))
        }
      }
    }
  }
  else {

    rowNamesForRaoFootnote <- c()

    for (variable in variables){
      if (options$rao){
        row <- circularTestsOneSampleResults[[variable]][["rao"]]
        rowName <- paste(variable, "rao")
        oneSampleTable$addRows(row, rowNames = rowName)

        rowNamesForRaoFootnote <- c(rowNamesForRaoFootnote, rowName)
      }
      if (options$rayleigh){
        row <- circularTestsOneSampleResults[[variable]][["rayleigh"]]
        oneSampleTable$addRows(row, rowNames = paste(variable))
      }
      if (options$modifiedRayleigh){
        row <- circularTestsOneSampleResults[[variable]][["modifiedRayleigh"]]
        oneSampleTable$addRows(row, rowNames = paste(variable))
      }
    }
  }
  oneSampleTable$addFootnote(symbol = "<em>Note.</em>", message = "All statistics are caclulated on a normalized period of 2pi.")
  if (options$rao)
    oneSampleTable$addFootnote(message = "The test is run with p = 0.01, so please compare the statistics to the critical value.", col_names = "testName", row_names = rowNamesForRaoFootnote)
}
.circularTestsOneSampleCreateTableVonMises <-function (jaspResults, dataset, options, circularTestsOneSampleVonMisesTestResults){
  splitName <- options$splitby
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)

  # Create table
  vonMisesCheckTable <- createJaspTable(title = "Von Mises Assumption Check")
  jaspResults[["vonMisesCheckTable"]] <- vonMisesCheckTable
  jaspResults[["vonMisesCheckTable"]]$dependOnOptions(c("variables", "splitby", "vonMisesCheck", "alpha", "period"))

  vonMisesCheckTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  vonMisesCheckTable$addColumnInfo(name = "variable",   title = "Variable",   type = "string", combine=TRUE)
  if (wantsSplit)
    vonMisesCheckTable$addColumnInfo(name = "level",   title = "Level",   type = "string", combine = TRUE)

  vonMisesCheckTable$addColumnInfo(name = "statistic",   title = "U\u00B2",   type = "number", format = "dp:3")
  vonMisesCheckTable$addColumnInfo(name = "critical",   title = "Critical",   type = "number", format = "dp:3")
  vonMisesCheckTable$addColumnInfo(name = "p",   title = "p",   type = "number", format = "dp:2")
  vonMisesCheckTable$addColumnInfo(name = "kappa",   title = "Est. Kappa",   type = "number", format = "dp:2")
  if (wantsSplit){
    split       <- dataset[[.v(options$splitby)]]
    splitLevels <- levels(split)

    rowsForKappaFootnote <- c()

    for(variable in variables){
      for(level in splitLevels){
        row <- circularTestsOneSampleVonMisesTestResults[[variable]][[level]]
        vonMisesCheckTable$addRows(row, rowNames = paste(variable, level))

        if (row$kappa < 1)
          rowsForKappaFootnote <- c(rowsForKappaFootnote, paste(variable, level))

      }
    }
    vonMisesCheckTable$addFootnote(message = "Do not trust a significant result where kappa is small (< 1). The data could rather be uniform.", col_names = "kappa", row_names = rowsForKappaFootnote)
  }
  else {
    rowsForKappaFootnote <- c()

    for(variable in variables){
      row <- circularTestsOneSampleVonMisesTestResults[[variable]]
      vonMisesCheckTable$addRows(row, rowNames = paste(variable))
      if (row$kappa < 1)
        rowsForKappaFootnote <- c(rowsForKappaFootnote, paste(variable))
    }
    vonMisesCheckTable$addFootnote(message = "Do not trust a significant result where kappa is small (< 1). The data could rather be uniform.", col_names = "kappa", row_names = rowsForKappaFootnote)
    vonMisesCheckTable$addFootnote(symbol = "<em>Note.</em>", message = "The test is run with p = 0.01, so please compare the statistics to the critical value.")
  }
}


# Helper functions for circular statistics ----
.normalizeData <- function(data, period){
  return(((data %% period) / period) * 2 * pi)
}