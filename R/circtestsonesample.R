#
# Copyright (C) 2019 Aaron Bahde and Philipp Berens, University of Tuebingen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

CircularStatisticsOneSampleTestsInternal <- function(jaspResults, dataset, options, ...) {
  # Get the correct customPeriod. This step is necessary since pi is hard to specify in the GUI.
  if (options$period == "pi")
    options$customPeriod <- pi
  if (options$period == "pi2")
    options$customPeriod <- 2 * pi

  ready <- (length(options$variables) > 0)
  
  if(ready)
    errors <- .circularTestsOneSampleCheckErrors(dataset, options)
  
  # Output tables and plots
  if(options$rao || options$rayleigh || options$modifiedRayleigh){
    if(ready)
      circularTestsOneSampleResults <- try(.circularTestsOneSampleComputeResults(jaspResults, dataset, options), silent = TRUE)
    .circularTestsOneSampleCreateTable(jaspResults, dataset, options, circularTestsOneSampleResults, ready)
  }
  if(options$vonMisesCheck){
    if(ready)
      circularTestsOneSampleVonMisesResults <- try(.circularTestsOneSampleComputeResultsVonMises(jaspResults, dataset, options), silent = TRUE)
    .circularTestsOneSampleCreateTableVonMises(jaspResults, dataset, options, circularTestsOneSampleVonMisesResults, ready)
  }
}

# Pre-processing functions ----
.circularTestsOneSampleCheckErrors <- function(dataset, options){
  splitName <- options$splitVariable
  wantsSplit <- splitName != ""
  if(wantsSplit){
    
    # check that there is at least one level for the split factor
    .hasErrors(
      dataset              = dataset,
      perform              = "run",
      type                 = "factorLevels",
      factorLevels.target  = options$splitVariable,
      factorLevels.amount  = "< 1",
      exitAnalysisIfErrors = TRUE)
    
    # check that there are no infinity values or zero observations
    .hasErrors(dataset, 
             type = c('observations', 'infinity'),
             all.target = options$variables, 
             all.grouping = options$splitVariable,
             observations.amount = c('< 1'), 
             exitAnalysisIfErrors = TRUE)
    
    # The rao test needs at least 4 data points.
    if(options$rao)
      .hasErrors(dataset, 
                 type = c('observations'),
                 all.target = options$variables, 
                 all.grouping = options$splitVariable,
                 observations.amount = c('< 4'), 
                 exitAnalysisIfErrors = TRUE)
    
    # The von Mises check fails if there is zero variance in the data.
    if(options$vonMisesCheck)
      .hasErrors(dataset, 
                 type = c('variance'),
                 all.target = options$variables, 
                 all.grouping = options$splitVariable, 
                 exitAnalysisIfErrors = TRUE)
    
  } else {
    
    # check that there are no infinity values or zero observations
    .hasErrors(dataset, 
               type = c('observations', 'infinity'),
               all.target = options$variables, 
               observations.amount = c('< 1'), 
               exitAnalysisIfErrors = TRUE)

    # The rao test needs at least 4 data points.
    if(options$rao)
      .hasErrors(dataset, 
                 type = c('observations'),
                 all.target = options$variables,
                 observations.amount = c('< 4'), 
                 exitAnalysisIfErrors = TRUE)
    
    # The von Mises check fails if there is zero variance in the data.
    if(options$vonMisesCheck)
      .hasErrors(dataset, 
                 type = c('variance'),
                 all.target = options$variables, 
                 exitAnalysisIfErrors = TRUE)
  }
  
  # check for reasonable period and that the data does not exceed a tolerable concentration
  # (It might happen that the user forgets to specify the correct period 
  # which leads to data that can be very concentrated when normalized to the unit circle, i.e. almost zero circular variance).
  # This can cause time outs in the circular package.
  .oneSampleTestsCheckForReasonablePeriodAndConcentration <- function(){

    tolerance <- 10**-3

    splitName <- options$splitVariable
    wantsSplit <- splitName != ""
    variables <- unlist(options$variables)

    if(wantsSplit){
      split <- dataset[[options$splitVariable]]
      splitLevels <- levels(split)
      for(variable in variables){
        for(level in splitLevels){
          column <- dataset[[variable]][split == level]
          validData <- column[!is.na(column)]
          validDataNormalized <- .normalizeData(validData, options$customPeriod)
          validDataCirc <- circular::circular(validDataNormalized)
          meanResultantLength <- as.numeric(circular::rho.circular(validDataCirc))
          if (abs(meanResultantLength-1) < tolerance)    # The maximum mean resultant length is 1. So if it exceeds the tolerance, return an error.
            return(gettextf("The data of the variable %1$s on level %2$s exceeds the tolerance for the concentration. The data shows almost zero variance. Did you maybe specify the wrong period?", variable, level))
        }
      }
    } else{
      for(variable in variables){
        column <- dataset[[variable]]
        validData <- column[!is.na(column)]
        validDataNormalized <- .normalizeData(validData, options$customPeriod)
        validDataCirc <- circular::circular(validDataNormalized)
        meanResultantLength <- as.numeric(circular::rho.circular(validDataCirc))
        if (abs(meanResultantLength-1) < tolerance)    # The maximum mean resultant length is 1. So if it exceeds the tolerance, return an error.
          return(gettextf("The data of the variable %s exceeds the tolerance for the concentration. The data shows almost zero variance. Did you maybe specify the wrong period?", variable))
      }
    }
  }
  .hasErrors(      dataset              = dataset,
                   perform              = "run",
                   custom               = .oneSampleTestsCheckForReasonablePeriodAndConcentration,
                   exitAnalysisIfErrors = TRUE)
}
# Results functions ----
.circularTestsOneSampleComputeResults <- function(jaspResults, dataset, options) {
  
  splitName <- options$splitVariable
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)

  results <- list()

  if(wantsSplit){
    split <- dataset[[options$splitVariable]]
    splitLevels <- levels(split)
    for(variable in variables){
      for(level in splitLevels){
        column <- dataset[[variable]][split == level]
        validData <- column[!is.na(column)]
        validDataNormalized <- .normalizeData(validData, options$customPeriod)
        validDataCirc <- circular::circular(validDataNormalized)

        if (options$rao){
          testResults <- .circularTestsOneSampleComputeResultsRao(jaspResults, validDataCirc, options)
          results[[variable]][[level]][["rao"]] <- list(
            variable = variable,
            level = level,
            testName = gettext("Rao's spacing"),
            alpha = testResults$alpha,
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
            testName = gettext("Rayleigh"),
            p = testResults$p,
            statistic = testResults$statistic
          )
        }
        if (options$modifiedRayleigh){
          testResults <- .circularTestsOneSampleComputeResultsModifiedRayleigh(jaspResults, validDataCirc, options)
          results[[variable]][[level]][["modifiedRayleigh"]] <- list(
            variable = variable,
            level = level,
            testName = gettextf("V against %s", toString(options$testValue)),
            p = testResults$p,
            statistic = testResults$statistic
          )
        }
      }
    }

  }
  else {
    for (variable in variables){
      column <- dataset[[variable]]
      validData <- column[!is.na(column)]
      validDataNormalized <- .normalizeData(validData, options$customPeriod)
      validDataCirc <- circular::circular(validDataNormalized)

      if (options$rao){
        testResults <- .circularTestsOneSampleComputeResultsRao(jaspResults, validDataCirc, options)
        results[[variable]][["rao"]] <- list(
          variable = variable,
          testName = gettext("Rao's Spacing"),
          alpha = testResults$alpha,
          statistic = testResults$statistic,
          criticalValue = testResults$criticalValue,
          n = testResults$n
        )
      }
      if (options$rayleigh){
        testResults <- .circularTestsOneSampleComputeResultsRayleigh(jaspResults, validDataCirc, options)
        results[[variable]][["rayleigh"]] <- list(
          variable = variable,
          testName = gettext("Rayleigh"),
          p = testResults$p,
          statistic = testResults$statistic
        )
      }
      if (options$modifiedRayleigh){
        testResults <- .circularTestsOneSampleComputeResultsModifiedRayleigh(jaspResults, validDataCirc, options)
        results[[variable]][["modifiedRayleigh"]] <- list(
          variable = variable,
          testName = gettextf("V against %s", toString(options$testValue)),
          p = testResults$p,
          statistic = testResults$statistic
        )
      }
    }
  }

  return(results)
}
.circularTestsOneSampleComputeResultsRao <- function(jaspResults, data, options) {
  
  alpha <- as.numeric(options$raoAlpha)
  testResult <- circular::rao.spacing.test(data,alpha=alpha)
  U <- testResult$statistic
  n <- testResult$n
  # The following is a routine that gets the critical value for the specified alpha value, depending in the dataset size.
  # It is neccesary, since the circular package just prints the test results without returning the critical values.
  criticalTableColumn <- (1:4)[alpha == c(0.001, 0.01, 0.05, 0.1)]
  # get the table row where the data count is as closest to the one in the table
  countColumn <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,35,40,45,50,75,100,150,200,300,400,500,600,700,800,900,1000)
  criticalTableRow <- which.min(abs(countColumn-n))
  # table for critical values (Levitin, Rusell 1995)
  data(rao.table, package="circular", envir = environment())
  criticalValue <- rao.table[criticalTableRow, criticalTableColumn]
  results <- list(alpha = alpha, statistic = U, criticalValue = criticalValue, n = n)
}

.circularTestsOneSampleComputeResultsRayleigh <- function(jaspResults, data, options) {
  testResult <- circular::rayleigh.test(data)
  p <- testResult$p.value
  statistic <- testResult$statistic
  results <- list(p = p, statistic = statistic)
  return(results)
}

.circularTestsOneSampleComputeResultsModifiedRayleigh <- function(jaspResults, data, options) {
  # We do not trust the implementation of the V test in the circular package. Therefore, we implement it on our own following Fisher 1993, p.69.
  # The test statistic is here based on the mean resultant length and not on "rayleighs R". Therefore the resulting statistic differs by a factor of n from other libraries, like CircStat for MATLAB

  # calculate the statistic r0Bar
  n <- length(data)
  mu0 <- .normalizeData(options$testValue, options$customPeriod)    # test direction must be normalized as well
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
  splitName <- options$splitVariable
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)

  circularTestsOneSampleVonMisesTestResults <- list()

  if (wantsSplit){
    split       <- dataset[[options$splitVariable]]
    splitLevels <- levels(split)
    for(variable in variables){
      for(level in splitLevels){
        column    <- dataset[[variable]][split == level]
        validData <- column[!is.na(column)]
        validDataNormalized <- .normalizeData(validData, options$customPeriod)
        validDataCirc <- circular::circular(validDataNormalized)
        testResults <- .circularTestsOneSampleComputeResultsVonMisesSub(validDataCirc, options, "vonmises")
        circularTestsOneSampleVonMisesTestResults[[variable]][[level]] <- list(
          variable = variable,
          level = level,
          statistic = testResults$statistic,
          critical = testResults$critical,
          alpha = testResults$alpha,
          kappa = testResults$kappa
        )
      }
    }
  } else {
    for (variable in variables){
      column <- dataset[[variable]]
      validData <- column[!is.na(column)]
      validDataNormalized <- .normalizeData(validData, options$customPeriod)
      validDataCirc <- circular::circular(validDataNormalized)
      testResults <- .circularTestsOneSampleComputeResultsVonMisesSub(validDataCirc, options, "vonmises")
      circularTestsOneSampleVonMisesTestResults[[variable]] <- list(
        variable = variable,
        statistic = testResults$statistic,
        critical = testResults$critical,
        alpha = testResults$alpha,
        kappa = testResults$kappa
      )
    }
  }
  return(circularTestsOneSampleVonMisesTestResults)
}
.circularTestsOneSampleComputeResultsVonMisesSub <- function(data, options, dist = "vonmises"){
  alpha <- as.numeric(options$vonMisesCheckAlpha)

  # Get the estimated kappa for the footnote. If kappa is too small, the data might be rather uniform.
  kappa <- circular::mle.vonmises(data, bias = FALSE)$kappa

  if(is.infinite(kappa))
    stop(gettextf("Estimated %s is infinite, could not compute results. Your data is too concentrated to calculate the assumption check.", "\u03BA"))
  
  testResult <- circular::watson.test(data, alpha = alpha, dist = dist)
  row <- testResult$row

  statistic <- testResult$statistic

  # This table comparison is copied from circular::print.watson.test() to get the values instead of just printing them to the console.
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
  
  return(list(critical = critical, statistic = statistic, alpha = alpha, kappa = kappa))
}
# Output functions ----
.circularTestsOneSampleCreateTable <- function(jaspResults, dataset, options, circularTestsOneSampleResults, ready) {

  wantsSplit <- options$splitVariable != ""
  
  # Create table
  oneSampleTable <- createJaspTable(title = gettext("Uniformity Tests"))
  jaspResults[["oneSampleTable"]] <- oneSampleTable
  jaspResults[["oneSampleTable"]]$dependOn(options = c("variables", "splitVariable", "rao", "raoAlpha", "rayleigh", "modifiedRayleigh", "customPeriod", "period"))

  oneSampleTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  oneSampleTable$addColumnInfo(name = "variable",        title = gettext("Variable"),  type = "string", combine = TRUE)
  if (wantsSplit)
    oneSampleTable$addColumnInfo(name = "level",         title = gettext("Level"),     type = "string", combine = TRUE)

  oneSampleTable$addColumnInfo(name = "testName",        title = gettext("Test"),      type = "string")
  if(options$rayleigh || options$modifiedRayleigh){
    oneSampleTable$addColumnInfo(name = "p",             title = gettext("p"),         type = "pvalue")
  }
  if (options$rao){
    oneSampleTable$addColumnInfo(name = "alpha",         title = "\u03B1",             type = "number")
    oneSampleTable$addColumnInfo(name = "criticalValue", title = gettext("Critical"),  type = "number")
  }
  oneSampleTable$addColumnInfo(name = "statistic",       title = gettext("Statistic"), type = "number")
  
  oneSampleTable$addFootnote(symbol = gettext("<em>Note.</em>"), message = gettextf("All statistics are calculated on a normalized period of 2%s.", "\u03C0"))
  
  # add citations
  oneSampleTable$addCitation(gettext("Aaron Bahde and Philipp Berens (2019). University of Tuebingen."))
  oneSampleTable$addCitation(gettext("Ulric Lund and Claudio Agostinelli (2017). Circular (Version 0.4-93): Circular Statistics [R Package]."))
  oneSampleTable$addCitation(gettext("Gerald Russell and Daniel Levitin (2007). An Extended Table of Probability Values for Raos Spacing Test."))
  
  if(ready){
    # If the calculations failed, do not fill the table but rather show the error.
    if(inherits(circularTestsOneSampleResults, "try-error")){
      errorMessage <- as.character(circularTestsOneSampleResults)
      oneSampleTable$setError(errorMessage)
      return()
    } else {
      .circularTestsOneSampleFillTable(oneSampleTable, circularTestsOneSampleResults, options, dataset)
    }
  }
}
.circularTestsOneSampleFillTable <- function(oneSampleTable, circularTestsOneSampleResults, options, dataset) {
  splitName <- options$splitVariable
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)
  if (wantsSplit) {
    split <- dataset[[options$splitVariable]]
    splitLevels <- levels(split)
    
    rowNamesForRaoFootnote <- c()
    
    for (variable in variables) {
      for (level in splitLevels) {
        if (options$rao) {
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
  } else {
    
    rowNamesForRaoFootnote <- c()
    
    for (variable in variables) {
      if (options$rao) {
        row <- circularTestsOneSampleResults[[variable]][["rao"]]
        rowName <- paste(variable, "rao")
        oneSampleTable$addRows(row, rowNames = rowName)
        
        rowNamesForRaoFootnote <- c(rowNamesForRaoFootnote, rowName)
      }
      if (options$rayleigh) {
        row <- circularTestsOneSampleResults[[variable]][["rayleigh"]]
        oneSampleTable$addRows(row, rowNames = paste(variable))
      }
      if (options$modifiedRayleigh) {
        row <- circularTestsOneSampleResults[[variable]][["modifiedRayleigh"]]
        oneSampleTable$addRows(row, rowNames = paste(variable))
      }
    }
  }
  if (options$rao)
    oneSampleTable$addFootnote(message = gettextf("The Rao spacing test was run with %1$s = %2$s, %3$s", "\u03B1", options$raoAlpha, "so please compare the statistics to the critical value."), colNames = "testName", rowNames = rowNamesForRaoFootnote)
}

.circularTestsOneSampleCreateTableVonMises <-function (jaspResults, dataset, options, circularTestsOneSampleVonMisesTestResults, ready){
  wantsSplit <- options$splitVariable != ""
  
  # Create table
  vonMisesCheckTable <- createJaspTable(title = gettext("Von Mises Assumption Check"))
  jaspResults[["vonMisesCheckTable"]] <- vonMisesCheckTable
  jaspResults[["vonMisesCheckTable"]]$dependOn(options = c("variables", "splitVariable", "vonMisesCheck", "vonMisesCheckAlpha", "customPeriod", "period"))

  vonMisesCheckTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  vonMisesCheckTable$addColumnInfo(name = "variable",  title = gettext("Variable"),           type = "string", combine = TRUE)
  if (wantsSplit)
    vonMisesCheckTable$addColumnInfo(name = "level",   title = gettext("Level"),              type = "string", combine = TRUE)
  vonMisesCheckTable$addColumnInfo(name = "alpha",     title = "\u03B1",                      type = "number")
  vonMisesCheckTable$addColumnInfo(name = "critical",  title = gettext("Critical"),           type = "number")
  vonMisesCheckTable$addColumnInfo(name = "statistic", title = "U\u00B2",                     type = "number")
  vonMisesCheckTable$addColumnInfo(name = "kappa",     title = gettextf("Est. %s", "\u03BA"), type = "number")
  
  # add citations
  vonMisesCheckTable$addCitation(gettext("Aaron Bahde and Philipp Berens (2019). University of Tuebingen."))
  vonMisesCheckTable$addCitation(gettext("Ulric Lund and Claudio Agostinelli (2017). Circular (Version 0.4-93): Circular Statistics [R Package]."))
  vonMisesCheckTable$addCitation(gettext("R. A. Lockhart and M. A. Stephens (1985). Tests of Fit for the Von Mises Distribution."))
  
  if(ready){
    # If the calculations failed, do not fill the table but rather show the error.
    if(isTryError(circularTestsOneSampleVonMisesTestResults)){
      errorMessage <- .extractErrorMessage(circularTestsOneSampleVonMisesTestResults)
      vonMisesCheckTable$setError(errorMessage)
      return()
    }
    .circularTestsOneSampleFillTableVonMises(vonMisesCheckTable, circularTestsOneSampleVonMisesTestResults, options, dataset)
  }
}
.circularTestsOneSampleFillTableVonMises <- function(vonMisesCheckTable, circularTestsOneSampleVonMisesTestResults, options, dataset) {
  
  splitName <- options$splitVariable
  wantsSplit <- splitName != ""
  variables <- unlist(options$variables)
  
  if (wantsSplit){
    split       <- dataset[[options$splitVariable]]
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
    vonMisesCheckTable$addFootnote(message = gettextf("Do not trust a significant result where %s is small (< 1). The data could rather be uniform.", "\u03BA"), colNames = "kappa", rowNames = rowsForKappaFootnote)
  }
  else {
    rowsForKappaFootnote <- c()
    
    for(variable in variables){
      row <- circularTestsOneSampleVonMisesTestResults[[variable]]
      vonMisesCheckTable$addRows(row, rowNames = paste(variable))
      if (row$kappa < 1)
        rowsForKappaFootnote <- c(rowsForKappaFootnote, paste(variable))
    }
    vonMisesCheckTable$addFootnote(message = gettextf("Do not trust a significant result where %s is small (< 1). The data could rather be uniform.", "\u03BA"), colNames = "kappa", rowNames = rowsForKappaFootnote)
    vonMisesCheckTable$addFootnote(symbol = gettext("<em>Note.</em>"), message = gettextf("The test is run with %1$s = %2$s, so please compare the statistics to the critical value.", "\u03B1", options$vonMisesCheckAlpha))
  }
}
