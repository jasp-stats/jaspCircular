CircularStatisticsMultipleSampleTests <- function(jaspResults, dataset, options, ...) {
  # Get the correct period. This step is neccessary since pi is hard to specify in the GUI
  if (options$periodGroup == "pi")
    options$period <- pi
  if (options$periodGroup == "pi_2")
    options$period <- 2 * pi

  # Set title
  jaspResults$title <- "Test Results"

  # workaround
  #TODO remove workaround
  if (length(options$dependent) != 0 && length(options$fixedFactors != 0)){
    # Read dataset
    dataset <- .circularTestsMultipleSampleReadData(dataset, options)

    # Error checking
    #.circularTestsMultipleSampleCheckErrors(dataset, options)

    oneWayAnovaResults <- .circularTestsMultipleSampleComputeResultsOneWayAnova(jaspResults, dataset, options)
    .circularTestsMultipleSampleTableOneWayAnova(jaspResults, dataset, options, oneWayAnovaResults)

    if (length(options$fixedFactors) == 2 && options$harrisonKanji){
      twoWayAnovaResults <- .circularTestsMultipleSampleComputeResultsTwoWayAnova(jaspResults, dataset, options)
      .circularTestsMultipleSampleTableTwoWayAnova(jaspResults, dataset, options, twoWayAnovaResults)
    }
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

}

# Results functions ----

# one-way ANOVA
.circularTestsMultipleSampleComputeResultsOneWayAnova <- function(jaspResults, dataset, options) {
  facs <- unlist(options$fixedFactors)
  results <- list()

  for (fac in facs){
    if (options$watsonWilliams)
      results[["watsonWilliams"]][[fac]] <- .circularTestsMultipleSampleWatsonWilliams(fac, dataset, options)
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
  dependentColumnNormalized <- .normalizeData(dataset[[.v(dependent)]][validIndices], options$period)
  factorColumn <- dataset[[.v(fac)]][validIndices]

  testResults <- circular::watson.williams.test(dependentColumnNormalized, factorColumn)

  p <- as.numeric(testResults$p.value)
  f <- as.numeric(testResults$statistic)
  df1 <- as.numeric(testResults$parameter)[1]
  df2 <- as.numeric(testResults$parameter)[2]

  results <- list(
    testName = "Watson-Williams",
    fac = fac,
    p = p,
    f = f,
    df1 = df1,
    df2 = df2
  )
  return(results)
}
.circularTestsMultipleSampleWatsonWheeler <- function(fac, dataset, options){
  # runs a watson wheeler test for a specific factor

  dependent <- unlist(options$dependent)

  # get valid data and normalize period
  validIndices <- !is.na(dataset[[.v(dependent)]])
  dependentColumnNormalized <- .normalizeData(dataset[[.v(dependent)]][validIndices], options$period)
  factorColumn <- dataset[[.v(fac)]][validIndices]

  testResults <- circular::watson.wheeler.test(dependentColumnNormalized, factorColumn)

  p <- as.numeric(testResults$p.value)
  w <- as.numeric(testResults$statistic)
  df <- as.numeric(testResults$parameter)

  results <- list(
    testName = "Watson-Wheeler",
    fac = fac,
    p = p,
    w = w,
    df = df
  )
  return(results)
}

# two-way ANOVA
.circularTestsMultipleSampleComputeResultsTwoWayAnova <- function(jaspResults, dataset, options) {
  dependent <- unlist(options$dependent)
  #TODO only the first two factors are used for interactions. Make these constraints in the GUI.
  fac1 <- options$fixedFactors[1]
  fac2 <- options$fixedFactors[2]

  # get valid data and normalize period
  validIndices <- !is.na(dataset[[.v(dependent)]])
  dependentColumnNormalized <- .normalizeData(dataset[[.v(dependent)]][validIndices], options$period)
  fac1Column <- dataset[[.v(fac1)]][validIndices]
  fac2Column <- dataset[[.v(fac2)]][validIndices]

  results <- list()

  results[["harrisonKanji"]] <- .circularTestsMultipleSampleHarrisonKanji(dependentColumnNormalized, fac1Column, fac2Column)
  return(results)
}
.circularTestsMultipleSampleHarrisonKanji <- function(dependent, fac1, fac2, inter = TRUE){
  # the Harrison-Kanji test is not available in the circular package. Thus, this test was migrated from the Matlab library CircStat by Philipp Berens 2009. For easy comparison, the following code is implemented as close as possible to the original (including variable names).
  # Input:
  # Dependent
  # Fac1: A column of factor1 which contains the levels for dependent
  # Fac2: A column of factor2 which contains the levels for dependent

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
.circularTestsMultipleSampleTableOneWayAnova <- function(jaspResults, dataset, options, oneWayAnovaResults) {
  dependent <- unlist(options$dependent)
  facs <- unlist(options$fixedFactors)

  # Create table
  oneWayAnovaTable <- createJaspTable(title = "One-way ANOVA")
  jaspResults[["oneWayAnovaTable"]] <- oneWayAnovaTable
  jaspResults[["oneWayAnovaTable"]]$dependOnOptions(c("dependent", "watsonWilliams", "period", "watsonWheeler", "fixedFactors"))

  oneWayAnovaTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  oneWayAnovaTable$addColumnInfo(name = "fac",   title = "Cases",   type = "string", combine = TRUE)
  oneWayAnovaTable$addColumnInfo(name = "testName",   title = "Test",   type = "string")
  oneWayAnovaTable$addColumnInfo(name = "p",   title = "p",   type = "number", format = "dp:3;p:.001")
  if (options$watsonWilliams){
    oneWayAnovaTable$addColumnInfo(name = "f",   title = "F",   type = "number", format = "dp:3")
    oneWayAnovaTable$addColumnInfo(name = "df1",   title = "df1",   type = "integer")
    oneWayAnovaTable$addColumnInfo(name = "df2",   title = "df2",   type = "integer")
  }
  if (options$watsonWheeler){
    oneWayAnovaTable$addColumnInfo(name = "w",   title = "W",   type = "number", format = "dp:3")
    oneWayAnovaTable$addColumnInfo(name = "df",   title = "df",   type = "integer")
  }

  for (fac in facs){
    if (options$watsonWilliams){
      row <- oneWayAnovaResults[["watsonWilliams"]][[fac]]
      oneWayAnovaTable$addRows(row, rowNames = paste(fac))
    }

    if (options$watsonWheeler){
      row <- oneWayAnovaResults[["watsonWheeler"]][[fac]]
      oneWayAnovaTable$addRows(row, rowNames = (paste(fac)))
    }
  }

  oneWayAnovaTable$addFootnote(symbol = "<em>Note.</em>", message = "All statistics are caclulated on a normalized period of 2pi.")
  if (options$watsonWheeler)
    oneWayAnovaTable$addFootnote(message = "The degrees of freedom of the \u03C7\u00B2-distribution to which W is compared.", col_names = "df")

}
.circularTestsMultipleSampleTableTwoWayAnova <- function(jaspResults, dataset, options, twoWayAnovaResults) {
  dependent <- unlist(options$dependent)
  fac1 <- options$fixedFactors[1]
  fac2 <- options$fixedFactors[2]
  # Create table
  twoWayAnovaTable <- createJaspTable(title = "Two-way ANOVA (Harrison-Kanji Test")
  jaspResults[["twoWayAnovaTable"]] <- twoWayAnovaTable
  jaspResults[["twoWayAnovaTable"]]$dependOnOptions(c("dependent", "harrisonKanji", "period", "fixedFactors"))

  twoWayAnovaTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  twoWayAnovaTable$addColumnInfo(name = "fac",   title = "Cases",   type = "string")
  kappa<-twoWayAnovaResults[["harrisonKanji"]][["estimatedKappa"]]
  if(kappa > 2){    # the HK test differs depending on the estimated kappa
    twoWayAnovaTable$addColumnInfo(name = "ss",   title = "Sum of Square",   type = "number", format = "dp:3")
    twoWayAnovaTable$addColumnInfo(name = "df",   title = "df",   type = "integer")
    twoWayAnovaTable$addColumnInfo(name = "ms",   title = "Mean Square",   type = "number", format = "dp:3")
    twoWayAnovaTable$addColumnInfo(name = "f",   title = "F",   type = "number", format = "dp:3")
    twoWayAnovaTable$addColumnInfo(name = "p",   title = "p",   type = "number", format = "dp:3;p:.001")
    twoWayAnovaTable$addFootnote(symbol = "<em>Note.</em>", message = "We estimated kappa greater than 2. The respective routine was run.")
  }else{
    twoWayAnovaTable$addColumnInfo(name = "df",   title = "df",   type = "integer")
    twoWayAnovaTable$addColumnInfo(name = "chi",   title = "\u03C7\u00B2",   type = "number", format = "dp:3")
    twoWayAnovaTable$addColumnInfo(name = "p",   title = "p",   type = "number", format = "dp:3;p:.001")
    twoWayAnovaTable$addFootnote(symbol = "<em>Note.</em>", message = "We estimated kappa less than 2. The respective routine was run.")
  }
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
  twoWayAnovaTable$addFootnote(symbol = "<em>Note.</em>", message = "All statistics are caclulated on a normalized period of 2pi.")
}

# Helper functions for circular statistics ----
.convertBack <- function(circularObject){
  # sometimes circular objects have a negative value instead of its modulo postive correspondence. This function ensures the correct backconversion
  return ((as.numeric(circularObject) + 2 * pi) %% ( 2 * pi))
}
.normalizeData <- function(data, period){
  return(((data %% period) / period) * 2 * pi)
}