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

CircularStatisticsMultipleSampleTestsInternal <- function(jaspResults, dataset, options, ...) {
  # Get the correct period. This step is neccessary since pi is hard to specify in the GUI.
  if (options$period == "pi")
    options$customPeriod <- pi
  if (options$period == "pi2")
    options$customPeriod <- 2 * pi

  # Set title
  #jaspResults$title <- "Test Results"

  ready <- (options$dependent != "") && (length(options$fixedFactors) > 0)
  readyHK <- (options$dependent != "") && (length(options$fixedFactors) >= 2)    # The HK test is only runable if there are at least two factors.
  if (ready){
    # Read dataset
    dataset <- .circularTestsMultipleSampleReadData(dataset, options)

    # Error checking
    .circularTestsMultipleSampleCheckErrors(dataset, options)
  }
  
  if(options$watsonWilliams || options$watsonWheeler){
    if(ready)
      oneWayAnovaResults <- try(.circularTestsMultipleSampleComputeResultsOneWayAnova(jaspResults, dataset, options))
    .circularTestsMultipleSampleTableOneWayAnova(jaspResults, dataset, options, oneWayAnovaResults, ready)
  }
  
  if (options$harrisonKanji){
    if(readyHK)
      twoWayAnovaResults <- try(.circularTestsMultipleSampleComputeResultsTwoWayAnova(jaspResults, dataset, options))
    .circularTestsMultipleSampleTableTwoWayAnova(jaspResults, dataset, options, twoWayAnovaResults, readyHK)
  }
}

# Preprocessing functions ----
.circularTestsMultipleSampleReadData <- function(dataset, options){
  dependent <- unlist(options$dependent)
  factors <- unlist(options$fixedFactors)
  dataset <- .readDataSetToEnd(columns.as.numeric = dependent, columns.as.factor = factors)
  return(dataset)
}
.circularTestsMultipleSampleCheckErrors <- function(dataset, options){
    
  # check that there is at least one level for each fixed factor
  .hasErrors(
    dataset              = dataset,
    perform              = "run",
    type                 = "factorLevels",
    factorLevels.target  = options$fixedFactors,
    factorLevels.amount  = "< 2",
    exitAnalysisIfErrors = TRUE)

  # For each assigned factor check that there is no sample with infinity values, zero observations, or zero variance.
  for (fac in options$fixedFactors){
      .hasErrors(dataset,
                 type = c('observations', 'infinity', 'variance'),
                 all.target = options$dependent,
                 all.grouping = fac,
                 observations.amount = c('< 1'),
                 exitAnalysisIfErrors = TRUE)
      }

  if(options$harrisonKanji && (length(options$fixedFactors) >= 2)){
    # Additionally for the HK test, each cell must contain at least two measurements and is not allowed to have zero variance.
    HkFactors <- options$fixedFactors[c(1,2)]
    .hasErrors(dataset,
               type = c('observations', 'variance'),
               all.target = options$dependent,
               all.grouping = HkFactors,
               observations.amount = c('< 2'),
               exitAnalysisIfErrors = TRUE)
  }
  
  # check for reasonable period and that the data does not exceed a tolerable concentration.
  # (It might happen that the user forgets to specify the correct period 
  # which leads to data that can be very concentrated when normalized to the unit circle, i.e. almost zero circular variance).
  # This can cause time outs in the circular package.
  .multipleSampleTestsCheckForReasonablePeriodAndConcentration <- function(){
    tolerance <- 1e-3
    
    fixedFactors <- unlist(options$fixedFactors)
    dependent <- unlist(options$dependent)
    for(fac in fixedFactors){
      split <- dataset[[.v(fac)]]
      splitLevels <- levels(split)
      for(level in splitLevels){
        column <- dataset[[.v(dependent)]][split == level]
        validData <- column[!is.na(column)]
        validDataNormalized <- .normalizeData(validData, options$customPeriod)
        validDataCirc <- circular::circular(validDataNormalized)
        
        meanResultantLength <- as.numeric(circular::rho.circular(validDataCirc))
        if (abs(meanResultantLength-1) < tolerance)    # The maximum mean resultant length is 1. So if it exceeds the tolerance, return an error.
          return(gettextf("The data of the dependent variable %1$s grouped on factor %2$s exceeds the tolerance for the concentration. The data shows almost zero variance. Did you maybe specify the wrong period?", dependent, fac))
      }
    }
  }
  .hasErrors(      dataset              = dataset,
                   perform              = "run",
                   custom               = .multipleSampleTestsCheckForReasonablePeriodAndConcentration,
                   exitAnalysisIfErrors = TRUE)
}

# Results functions ----

# one-way ANOVA
.circularTestsMultipleSampleComputeResultsOneWayAnova <- function(jaspResults, dataset, options) {
  facs <- unlist(options$fixedFactors)
  results <- list()

  for (fac in facs){
    if (options$watsonWilliams){
      results[["watsonWilliams"]][[fac]] <- .circularTestsMultipleSampleWatsonWilliams(fac, dataset, options)
    }
    if (options$watsonWheeler)
      results[["watsonWheeler"]][[fac]] <- .circularTestsMultipleSampleWatsonWheeler(fac, dataset, options)
  }
  return(results)
}
.circularTestsMultipleSampleWatsonWilliams <- function(fac, dataset, options){
  # runs a watson williams test for a specific factor
  
  dependent <- unlist(options$dependent)

  # get valid data and normalize period
  validIndices <- !is.na(dataset[[.v(dependent)]])
  dependentColumnNormalized <- .normalizeData(dataset[[.v(dependent)]][validIndices], options$customPeriod)
  factorColumn <- dataset[[.v(fac)]][validIndices]

  # Check if the test is applicable, i.e. the assumption of equal kappas across the groups is justified. Store the result to decide on a warning footnote when creating the table.
  equalKappaTestResult <- circular::equal.kappa.test(dependentColumnNormalized, factorColumn)
  equalKappaTestPvalue <- equalKappaTestResult$p.value
  kappas <- paste(format(equalKappaTestResult$kappa, digits = 3), collapse = ", ")
  
  testResults <- circular::watson.williams.test(dependentColumnNormalized, factorColumn)

  p <- as.numeric(testResults$p.value)
  f <- as.numeric(testResults$statistic)
  df1 <- as.numeric(testResults$parameter)[1]
  df2 <- as.numeric(testResults$parameter)[2]

  results <- list(
    testName = gettext("Watson-Williams"),
    fac = fac,
    p = p,
    f = f,
    df1 = df1,
    df2 = df2,
    equalKappaTestPvalue = equalKappaTestPvalue,
    kappas = kappas
  )
  return(results)
}
.circularTestsMultipleSampleWatsonWheeler <- function(fac, dataset, options){
  # runs a watson wheeler test for a specific factor

  dependent <- unlist(options$dependent)

  # get valid data and normalize customPeriod
  validIndices <- !is.na(dataset[[.v(dependent)]])
  dependentColumnNormalized <- .normalizeData(dataset[[.v(dependent)]][validIndices], options$customPeriod)
  factorColumn <- dataset[[.v(fac)]][validIndices]

  # Check if each group has at least 10 measurements. Store the result to add a warning as a footnote later (if neccessary).
  hasTenMeasurementsPerGroup <- TRUE    # default is TRUE
  split <- dataset[[.v(fac)]]
  splitLevels <- levels(split)
  for (level in splitLevels){
    groupData <- dataset[[.v(dependent)]][split == level]
    validGroupData <- groupData[!is.na(groupData)]
    groupSize <- length(validGroupData)
    if(groupSize < 10)
      hasTenMeasurementsPerGroup <- FALSE    # if one group has less than 10, set flag to false
  }
  
  testResults <- circular::watson.wheeler.test(dependentColumnNormalized, factorColumn)

  p <- as.numeric(testResults$p.value)
  w <- as.numeric(testResults$statistic)
  df <- as.numeric(testResults$parameter)

  results <- list(
    testName = gettext("Watson-Wheeler"),
    fac = fac,
    p = p,
    w = w,
    df = df,
    hasTenMeasurementsPerGroup = hasTenMeasurementsPerGroup
  )
  return(results)
}

# two-way ANOVA
.circularTestsMultipleSampleComputeResultsTwoWayAnova <- function(jaspResults, dataset, options) {
  
  dependent <- unlist(options$dependent)
  fac1 <- options$fixedFactors[1]
  fac2 <- options$fixedFactors[2]

  # get valid data and normalize period
  validIndices <- !is.na(dataset[[.v(dependent)]])
  dependentColumnNormalized <- .normalizeData(dataset[[.v(dependent)]][validIndices], options$customPeriod)
  fac1Column <- dataset[[.v(fac1)]][validIndices]
  fac2Column <- dataset[[.v(fac2)]][validIndices]

  results <- list()

  results[["harrisonKanji"]] <- .circularTestsMultipleSampleHarrisonKanji(dependentColumnNormalized, fac1Column, fac2Column)
  
  return(results)
}
.circularTestsMultipleSampleHarrisonKanji <- function(dependent, fac1, fac2, inter = TRUE){
  # The Harrison-Kanji test is not available in the circular package. Thus, this test was migrated from the Matlab library CircStat by Philipp Berens 2009. For easy comparison, the following code is implemented as close as possible to the original (including variable names).
  # Input:
  # Dependent: A column of the dependent measurements
  # Fac1: A column of factor1 which contains the levels for dependent
  # Fac2: A column of factor2 which contains the levels for dependent
  # Output:
  # A list containing the p-value and the test statistics for each case.

  p <- nlevels(fac1)
  q <- nlevels(fac2)
  n <- length(dependent)

  # creates a dataframe for later purposes
  df <- data.frame(dependent = dependent, fac1 = fac1, fac2 = fac2)

  # both factors
  cn <- matrix(0, p, q)    # cell counts
  cr <- cn    # cell mean lengths
  # fac1
  pm <- matrix(0, p, 1)    # fac1 mean directions
  pr <- pm    # fac1 mean lengths
  pn <- pm    # fac1 counts
  # fac2
  qm <- matrix(0, q, 1)
  qr <- qm
  qn <- qm
  
  for(pp in c(1:p)){
    for (qq in c(1:q)){
      level1 <- levels(fac1)[pp]
      level2 <- levels(fac2)[qq]
      # get cell mean length (both factors)
      cellData <- subset(df, fac1 == level1 & fac2 == level2)$dependent
      rCell <- circular::rho.circular(cellData)
      cn[pp,qq] <- length(cellData)
      cr[pp,qq] <- cn[pp, qq] * rCell

      # r and mean angle for fac2. Doing it here saves a loop
      fac2Data <- subset(df, fac2 == level2)$dependent
      qn[qq] <- length(fac2Data)
      rFac2 <- circular::rho.circular(fac2Data)
      qr[qq] <-   qn[qq] * rFac2
      qm[qq] <- .convertBack(circular::mean.circular(fac2Data))
    }
    # r and mean angle for fac1. Doing it here saves a loop
    fac1Data <- subset(df, fac1 == level1)$dependent
    pn[pp] <- length(fac1Data)
    rFac1 <- circular::rho.circular(fac1Data)
    pr[pp] <-   pn[pp] * rFac1
    pm[pp] <- .convertBack(circular::mean.circular(fac1Data))
  }
  
  # r and mean angle for total data
  tr <- n * circular::rho.circular(dependent)

  # estimate kappa
  kk <- circular::mle.vonmises(dependent)$kappa

  # different formulas for different kappa
  if (kk > 2){    # for large kappa
    # effect of fac1
    eff1 <- sum(pr**2 / rowSums(cn)) - tr**2 / n
    df1 <- p - 1
    ms1 <- eff1 / df1
    # effect of fac2
    eff2 <- sum(qr**2 / colSums(cn))- tr**2 / n
    df2 <- q - 1
    ms2 <- eff2 / df2
    # total effect
    efft <- n - tr**2 / n
    dft <- n - 1

    m <- mean(cn)
    if (inter == TRUE){
      # correction factor for improved F statistic
      beta = 1 / (1 - 1 / (5 * kk) - 1 / (10 * (kk**2)))

      # residual effects
      effr <- n - sum(cr**2 / cn)
      dfr <- p * q * (m - 1)
      msr <- effr / dfr
      # interaction effects
      effi <- sum(cr**2 / cn) - sum(qr**2 / qn) - sum(pr**2 / pn) + tr**2 / n
      dfi <- (p - 1) * (q - 1)
      msi <- effi / dfi
      # interaction test statistic
      FI <- msi / msr
      pI <- 1 - pf(FI, dfi, dfr)
      
    } else {
      # residual effect
      effr <- n - sum(qr**2 / qn) - sum(pr**2 / pn) + tr**2 / n
      dfr <- (p - 1) * (q - 1)
      msr <- effr / dfr
      # interaction effects
      effi <- NULL
      dfi <- NULL
      msi <- NULL
      # interaction test statistics
      FI <- NULL
      pI <- NaN
      beta <- 1
    }
    # compute all test statistics
    F1 <- beta * ms1 / msr
    p1 <- 1 - pf(F1, df1, dfr)

    F2 <- beta * ms2 / msr
    p2 <- 1 - pf(F2, df2, dfr)
  }
  else{    # for small kappa
    # correction factor
    rr <- besselI(kk, 1) / besselI(kk, 0)
    f <- 2 / (1 - rr**2)

    chi1 <- f * (sum(pr**2 / pn) - tr**2 / n)
    df1 <-2 * (p - 1)
    p1 <- 1 - pchisq(chi1, df1)

    chi2 <- f * (sum(qr**2 / qn) - tr**2 / n)
    df2 <- 2 * (q - 1)
    p2 <- 1 - pchisq(chi2, df2)

    chiI <- f * (sum(cr**2 / cn) - sum(pr**2 / pn) - sum(qr**2 / qn) + tr**2 / n)
    dfi <- (p - 1) * (q - 1)
    pI <- 1 - pchisq(chiI, dfi)
  }
  
  # prepare output depending on case
  if (kk > 2){
    fac1Results <- list(p = p1, df =df1, ms = ms1, f = F1, ss = eff1)
    fac2Results <- list(p = p2, df = df2, ms = ms2, f = F2, ss = eff2)
    interResults <- list(p = pI, df = dfi, ms = msi, f = FI, ss = effi)
    residualResults <- list(df = dfr, ms = msr, ss = effr)
    results <- list(estimatedKappa = kk, fac1 = fac1Results, fac2 = fac2Results, inter = interResults, residual = residualResults)
    return(results)
  }else{
    pvals <- list(fac1 = p1, fac2 = p2, inter = pI)
    chis <- list(fac1 = chi1, fac2 = chi2, inter = chiI)
    dfs <- list(fac1 = df1, fac2 = df2, inter = dfi)
    fac1Results <- list(p = p1, chi = chi1, df = df1)
    fac2Results <- list(p = p2, chi = chi2, df = df2)
    interResults <- list(p = pI, chi = chiI, df = dfi)
    results <- list(estimatedKappa = kk, fac1 = fac1Results, fac2 = fac2Results, inter = interResults)
    return(results)
  }
}

# Output functions ----
.circularTestsMultipleSampleTableOneWayAnova <- function(jaspResults, dataset, options, oneWayAnovaResults, ready) {

  # Create table
  oneWayAnovaTable <- createJaspTable(title = gettext("One-way ANOVA"))
  jaspResults[["oneWayAnovaTable"]] <- oneWayAnovaTable
  jaspResults[["oneWayAnovaTable"]]$dependOn(options = c("dependent", "watsonWilliams", "customPeriod", "period", "watsonWheeler", "fixedFactors"))

  oneWayAnovaTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  oneWayAnovaTable$addColumnInfo(  name = "fac",      title = gettext("Cases"), type = "string", combine = TRUE)
  oneWayAnovaTable$addColumnInfo(  name = "testName", title = gettext("Test"),  type = "string")
  oneWayAnovaTable$addColumnInfo(  name = "p",        title = gettext("p"),     type = "pvalue")
  if (options$watsonWilliams){
    oneWayAnovaTable$addColumnInfo(name = "f",        title = gettext("F"),     type = "number", format = "dp:3")
    oneWayAnovaTable$addColumnInfo(name = "df1",      title = gettext("df1"),   type = "integer")
    oneWayAnovaTable$addColumnInfo(name = "df2",      title = gettext("df2"),   type = "integer")
  }
  if (options$watsonWheeler){
    oneWayAnovaTable$addColumnInfo(name = "w",        title = gettext("W"),     type = "number", format = "dp:3")
    oneWayAnovaTable$addColumnInfo(name = "df",       title = gettext("df"),    type = "integer")
  }

  oneWayAnovaTable$addFootnote(message = gettextf("All statistics are calculated on a normalized period of 2%s.", "\u03C0"))
  if (options$watsonWheeler)
    oneWayAnovaTable$addFootnote(message = gettextf("The degrees of freedom of the %s%s-distribution to which W is compared.", "\u03C7", "\u00B2"), colNames = "df")

  # add citations
  oneWayAnovaTable$addCitation(gettext("Aaron Bahde and Philipp Berens (2019). University of Tuebingen."))
  oneWayAnovaTable$addCitation(gettext("Ulric Lund and Claudio Agostinelli (2017). Circular (Version 0.4-93): Circular Statistics [R Package]."))
  
  if(ready){
    # If the calculations failed, do not fill the table but rather show the error.
    if(inherits(oneWayAnovaResults, "try-error")){
      errorMessage <- as.character(oneWayAnovaResults)
      oneWayAnovaTable$setError(errorMessage)
      return()
    } else {
    .circularTestsMultipleSampleFillOneWayAnovaTable(oneWayAnovaTable, oneWayAnovaResults, options)
    }
  }
}
.circularTestsMultipleSampleFillOneWayAnovaTable <- function(oneWayAnovaTable, oneWayAnovaResults, options) {
  dependent <- unlist(options$dependent)
  facs <- unlist(options$fixedFactors)
  for (fac in facs){
    if (options$watsonWilliams){
      row <- oneWayAnovaResults[["watsonWilliams"]][[fac]]
      oneWayAnovaTable$addRows(row, rowNames = paste(fac))
      
      # add a warning if the equal kappa assumption is violated
      if(row$equalKappaTestPvalue < 0.05)
        oneWayAnovaTable$addFootnote(symbol = gettext("<em>Warning.</em>"), message = gettextf("Concentration parameters (%1$s) not equal between the groups of factor %2$s. The Watson-Williams test might not be applicable.", row$kappas, fac))
    }
    
    if (options$watsonWheeler){
      row <- oneWayAnovaResults[["watsonWheeler"]][[fac]]
      oneWayAnovaTable$addRows(row, rowNames = (paste(fac)))
      if(!row$hasTenMeasurementsPerGroup)
        oneWayAnovaTable$addFootnote(symbol = gettext("<em>Warning.</em>"), message = gettextf("Some groups of factor %s contain less than 10 measurements. The Watson-Wheeler test might not be applicable.", fac))
    }
  }
}

.circularTestsMultipleSampleTableTwoWayAnova <- function(jaspResults, dataset, options, twoWayAnovaResults, readyHK) {
  # Create table
  twoWayAnovaTable <- createJaspTable(title = gettext("Two-way ANOVA (Harrison-Kanji Test)"))
  jaspResults[["twoWayAnovaTable"]] <- twoWayAnovaTable
  jaspResults[["twoWayAnovaTable"]]$dependOn(options = c("dependent", "harrisonKanji", "period", "customPeriod", "fixedFactors"))
  
  twoWayAnovaTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  twoWayAnovaTable$addColumnInfo(name = "fac",   title = gettext("Cases"),   type = "string")
  
  
  # If the analysis is not ready, show as default the table of the small kappa case. So we have to set kappa less than 2.
  if(readyHK){
    # We need to know kappa from the results. so rather return the error if one occured during the calculations.
    if(inherits(twoWayAnovaResults,"try-error")){
      errorMessage <- as.character(twoWayAnovaResults)
      twoWayAnovaTable$setError(errorMessage)
      return()
    } else {
      kappa<-twoWayAnovaResults[["harrisonKanji"]][["estimatedKappa"]]
    }
  } else {
    kappa <- 0
  }
  
  footnote <- gettextf("All statistics are calculated on a normalized period of 2%s.", "\u03C0")
  if(kappa > 2) {    # the HK test differs depending on the estimated kappa
    twoWayAnovaTable$addColumnInfo(name = "p",  title = gettext("p"),             type = "pvalue")
    twoWayAnovaTable$addColumnInfo(name = "f",  title = gettext("F"),             type = "number", format = "dp:3")
    twoWayAnovaTable$addColumnInfo(name = "df", title = gettext("df"),            type = "integer")
    twoWayAnovaTable$addColumnInfo(name = "ss", title = gettext("Sum of Square"), type = "number", format = "dp:3")
    twoWayAnovaTable$addColumnInfo(name = "ms", title = gettext("Mean Square"),   type = "number", format = "dp:3")
    footnote <- paste(footnote, 
                      gettextf("Estimated %1$s is %2$s (> 2). The respective routine was run.", "\u03BA", round(kappa, digits = 2)), 
                      collapse = " ")
  } else {
    twoWayAnovaTable$addColumnInfo(name = "p",   title = gettext("p"),   type = "pvalue")
    twoWayAnovaTable$addColumnInfo(name = "chi", title = "\u03C7\u00B2", type = "number", format = "dp:3")
    twoWayAnovaTable$addColumnInfo(name = "df",  title = gettext("df"),  type = "integer")
    footnote <- paste(footnote, 
                      gettextf("Estimated %1$s is %2$s (< 2). The respective routine was run.", "\u03BA", round(kappa, digits = 2)), 
                      collapse = " ")
  }
  twoWayAnovaTable$addFootnote(message = footnote)
  
  # add citations
  twoWayAnovaTable$addCitation(gettext("Aaron Bahde and Philipp Berens (2019). University of Tuebingen."))
  twoWayAnovaTable$addCitation(gettext("Ulric Lund and Claudio Agostinelli (2017). Circular (Version 0.4-93): Circular Statistics [R Package]."))
  twoWayAnovaTable$addCitation(gettext("D. Harrison, G.K Kanji and R. J. Gadsden (2016). Analysis of Variance for Circular Data."))
  
  if(readyHK){
    # If the calculations failed, do not fill the table but rather show the error.
    if(inherits(twoWayAnovaResults,"try-error")){
      errorMessage <- as.character(twoWayAnovaResults)
      twoWayAnovaTable$setError(errorMessage)
      return()
    } else {
      .circularTestsMultipleSampleFillTwoWayAnovaTable(twoWayAnovaTable, twoWayAnovaResults, options)
    }
  }
  
}
.circularTestsMultipleSampleFillTwoWayAnovaTable <- function(twoWayAnovaTable, twoWayAnovaResults, options){
  dependent <- unlist(options$dependent)
  fac1 <- options$fixedFactors[1]
  fac2 <- options$fixedFactors[2]
  
  kappa<-twoWayAnovaResults[["harrisonKanji"]][["estimatedKappa"]]
  
  row <- twoWayAnovaResults[["harrisonKanji"]][["fac1"]]
  twoWayAnovaTable$addRows(c(list(fac = fac1), row))
  
  row <- twoWayAnovaResults[["harrisonKanji"]][["fac2"]]
  twoWayAnovaTable$addRows(c(list(fac = fac2), row))
  
  row <- twoWayAnovaResults[["harrisonKanji"]][["inter"]]
  twoWayAnovaTable$addRows(c(list(fac = "Interaction"), row))
  
  if (kappa > 2){
    row <- twoWayAnovaResults[["harrisonKanji"]][["residual"]]
    twoWayAnovaTable$addRows(c(list(fac = "Residual"), row))
  }
}
# Helper functions for circular statistics ----
.convertBack <- function(circularObject){
  # Sometimes, circular objects have a negative value instead of its modulo positive correspondence. This function ensures the correct backconversion.
  return ((as.numeric(circularObject) + 2 * pi) %% ( 2 * pi))
}
