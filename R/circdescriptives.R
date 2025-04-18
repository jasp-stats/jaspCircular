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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

CircularStatisticsDescriptivesInternal <- function(jaspResults, dataset, options, ...) {
  # Get the correct period. This step is necessary since pi is hard to specify in the GUI.
  if (options$period == "pi")
    options$customPeriod <- pi
  if (options$period == "pi2")
    options$customPeriod <- 2 * pi

  ready <- (length(options$variables) > 0)
  # Determine if the user splits the calculation or not. It has an effect on the whole analysis.
  wantsSplit <- options$splitVariable != ""

  # If ready, calculate the results
  if (ready) {
    errors <- .circularDescriptivesCheckErrors(dataset, options)
    circularDescriptivesResults <- try(.circularDescriptivesComputeResults(jaspResults, dataset, options))
  }
  
  # Output tables
  if(is.null(jaspResults[["circularDescriptivesTable"]]))
    .circularDescriptivesCreateTable(jaspResults, circularDescriptivesResults, dataset, options, ready, wantsSplit)

  # Output plots
  if(options$distributionPlot && is.null(jaspResults[["Plots"]]))
    .circularDescriptivesCreatePlot(jaspResults, dataset, options, circularDescriptivesResults, wantsSplit, ready)
  
  return()
}

# Pre-processing functions ----

.circularDescriptivesCheckErrors <- function(dataset, options){
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
  } else {
    
    # check that there are no infinity values or zero observations
    .hasErrors(dataset, 
               type = c('observations', 'infinity'),
               all.target = options$variables, 
               observations.amount = c('< 1'), 
               exitAnalysisIfErrors = TRUE)
  }
}
# Results functions ----
.circularDescriptivesComputeResults <- function(jaspResults, dataset, options) {

  # Take results from state if possible
  if (!is.null(jaspResults[["stateCircDescriptivesResults"]]))
    return(jaspResults[["stateCircDescriptivesResults"]]$object)

  wantsSplit <- options$splitVariable != ""
  variables <- unlist(options$variables)

  # This will be the object that we fill with results
  results <- list()

  if (wantsSplit)
  {
    split <- dataset[[options$splitVariable]]
    splitLevels <- levels(split)

    for (variable in variables)
      for (level in splitLevels)
      {
        results[["descr"]][[variable]][[level]] <- list()
        column <- dataset[[variable]][split == level]
        results[["descr"]][[variable]][[level]] <- .circularDescriptivesComputeResultsSub(column, options)
        }
  } else {
    for (variable in options$variables){
      results[["descr"]][[variable]] <- list()
      column <- dataset[[variable]]
      results[["descr"]][[variable]] <- .circularDescriptivesComputeResultsSub(column, options)
    }
  }

  # save results to state
  jaspResults[["stateCircDescriptivesResults"]] <- createJaspState(results)
  jaspResults[["stateCircDescriptivesResults"]]$dependOn(options = c("variables", "splitVariable", "customPeriod", "period", "meanDirection", "meanLength", "standardDeviation", "variance", "range", "median"))
  return(results)
}
.circularDescriptivesComputeResultsSub <- function(column, options){

  nObs <- length(column)
  validData <- column[!is.na(column)]
  nValidObs <- length(validData)
  nInvalidObs <- nObs - nValidObs

  # normalizes the period of the data to 2pi to do the calculations
  validDataNormalized <- .normalizeData(validData, options$customPeriod)
  # convert the valid data to a circular object with period 2pi which is used by all circular package calculations
  validDataCircular <- circular::circular(validDataNormalized, modulo = "2pi")

  # Some circular functions return a circular object. Always convert them back to a numeric.

  meanDirection <- as.numeric(circular::mean.circular(validDataCircular))
  meanLength <- as.numeric(circular::rho.circular(validDataCircular))

  # By convention, the median is the lowest of all possible values.
  medianCircular <- circular::median.circular(validDataCircular)
  median <- min(attr(medianCircular, "medians"))

  stdDev <- as.numeric(circular::sd.circular(validDataCircular))
  variance <- as.numeric(circular::var.circular(validDataCircular))
  range <- as.numeric(circular::range.circular(validDataCircular))

  # store the data that needs to be plotted
  # Plotting data is already circular.
  plotData <- data.frame(x = as.numeric(validDataCircular))

  # For interpretation, some measures are converted back to the original period.
  meanDirection <- .circularDescriptivesConvertBack(meanDirection,options$customPeriod)
  median <- .circularDescriptivesConvertBack(median,options$customPeriod)
  range <- .circularDescriptivesConvertBack(range,options$customPeriod)

  results <- list(
    nValidObs = nValidObs,
    nInvalidObs = nInvalidObs,
    total = nObs,
    meanDirection = meanDirection,
    meanLength = meanLength,
    median = median,
    stdDev = stdDev,
    variance = variance,
    range = range,
    plotData = plotData)

  return(results)
}
# Output functions ----
.circularDescriptivesCreateTable <- function(jaspResults, circularDescriptivesResults, dataset, options, ready, wantsSplit) {
  
  circularDescriptivesTable <- createJaspTable(title = gettext("Circular Descriptives"))
  circularDescriptivesTable$dependOn(options = c("variables", "splitVariable", "customPeriod", "period", "meanDirection", "meanLength", "standardDeviation", "variance", "range", "median"))
  
  circularDescriptivesTable$showSpecifiedColumnsOnly <- TRUE
  circularDescriptivesTable$transpose <- TRUE
  
  if (wantsSplit)
  {
    circularDescriptivesTable$transposeWithOvertitle <- TRUE
    circularDescriptivesTable$addColumnInfo(name = "variable",  title = "", type = "string")
    circularDescriptivesTable$addColumnInfo(name = "level",     title = "", type = "string")
  } else {
    circularDescriptivesTable$addColumnInfo(name = "variable",  title = "", type = "string")
  }
  
  # Add columns to table
  circularDescriptivesTable$addColumnInfo(name = "nValidObs",       title = gettext("Valid"),                 type = "integer")
  circularDescriptivesTable$addColumnInfo(name = "nInvalidObs",     title = gettext("Missing"),               type = "integer")
  if (options$meanDirection)
    circularDescriptivesTable$addColumnInfo(name = "meanDirection", title = gettext("Mean Direction"),        type = "number", format = "dp:3")
  if (options$meanLength)
    circularDescriptivesTable$addColumnInfo(name = "meanLength",    title = gettext("Mean Resultant Length"), type = "number", format = "dp:3")
  if (options$median)
    circularDescriptivesTable$addColumnInfo(name = "median",        title = gettext("Median"),                type = "number", format = "dp:3")
  if (options$standardDeviation)
    circularDescriptivesTable$addColumnInfo(name = "stdDev",        title = gettext("Standard Deviation"),    type = "number", format = "dp:3")
  if (options$variance)
    circularDescriptivesTable$addColumnInfo(name = "variance",      title = gettext("Variance"),              type = "number", format = "dp:3")
  if (options$range)
    circularDescriptivesTable$addColumnInfo(name = "range",         title = gettext("Range"),                 type = "number", format = "dp:3")
  
  # create the footnotes
  circularDescriptivesTable$addFootnote(symbol = gettext("<em>Note.</em>"), message = gettext("If not stated otherwise, all values are calculated on a normalized period of 2\u03C0."))
  if (options$period != "pi2")
    circularDescriptivesTable$addFootnote(message = gettext("Value is shown with respect to the original period."), colNames = list("meanDirection", "median", "range"))
  
  # add citations
  circularDescriptivesTable$addCitation(gettext("Aaron Bahde and Philipp Berens (2019). University of Tuebingen."))
  circularDescriptivesTable$addCitation(gettext("Ulric Lund and Claudio Agostinelli (2017). Circular (Version 0.4-93): Circular Statistics [R Package]."))
  
  jaspResults[["circularDescriptivesTable"]] <- circularDescriptivesTable
  
  if(ready)
    # If the calculations failed, do not fill the table but rather show the error.
    if(inherits(circularDescriptivesResults, "try-error")){
      errorMessage <- as.character(circularDescriptivesResults)
      circularDescriptivesTable$setError(errorMessage)
      return()
    } else {
      .circularDescriptivesFillTable(circularDescriptivesTable, circularDescriptivesResults, options, dataset, wantsSplit)
    }
  
}
.circularDescriptivesFillTable <- function(circularDescriptivesTable, circularDescriptivesResults, options, dataset, wantsSplit){
  variables <- unlist(options$variables)
  if (wantsSplit)
  {
    split <- dataset[[options$splitVariable]]
    splitLevels <- levels(split)
    
    for (variable in variables)
      for (level in splitLevels)
      {
        row <- circularDescriptivesResults[["descr"]][[variable]][[level]]
        row[["variable"]] <- variable
        row[["level"]] <- level
        circularDescriptivesTable$addRows(row, rowNames = paste(variable))
      }
  } else {
    for (variable in options$variables){
      row <- circularDescriptivesResults[["descr"]][[variable]]
      row[["variable"]] <- variable
      circularDescriptivesTable$addRows(row, rowNames = paste(variable))
    }
  }
}


.circularDescriptivesCreatePlot <- function(jaspResults, dataset, options, circularDescriptivesResults, wantsSplit, ready){
  # get a container for plots
  containerPlots <- createJaspContainer(title = gettext("Circular Descriptives Plots"), dependencies = c("variables", "splitVariable", "customPeriod", "period", "distributionPlot", "distributionPlotMeanVector", "distributionPlotHistogram", "distributionPlotPointStack"))
  jaspResults[["Plots"]] <- containerPlots
  
  plotSize <- 320
  variables <- unlist(options$variables)
  if (wantsSplit) {
    split <- dataset[[options$splitVariable]]
    splitLevels <- levels(split)
    for (variable in variables){
      containerLevelPlots <- createJaspContainer(title = variable, dependencies = c("variables", "splitVariable", "customPeriod", "period", "distributionPlot", "distributionPlotMeanVector", "distributionPlotHistogram", "distributionPlotPointStack"))
      for (level in splitLevels){
        jaspPlot <- createJaspPlot(title=level, width = plotSize, height = plotSize, dependencies = c("variables", "splitVariable", "customPeriod", "period", "distributionPlot", "distributionPlotMeanVector", "distributionPlotHistogram", "distributionPlotPointStack"))
        
        # add citations
        jaspPlot$addCitation(gettext("Aaron Bahde and Philipp Berens (2019). University of Tuebingen."))
        jaspPlot$addCitation(gettext("Ulric Lund and Claudio Agostinelli (2017). Circular (Version 0.4-93): Circular Statistics [R Package]."))
        
        containerLevelPlots[[level]] <- jaspPlot
      }
    containerPlots[[variable]] <- containerLevelPlots
    }
  } else {
    for (variable in variables){
      jaspPlot <- createJaspPlot(title=variable, width = plotSize, height = plotSize, dependencies = c("variables", "splitVariable", "customPeriod", "period", "distributionPlot", "distributionPlotMeanVector", "distributionPlotHistogram", "distributionPlotPointStack"))
      
      # add citations
      jaspPlot$addCitation(gettext("Aaron Bahde and Philipp Berens (2019). University of Tuebingen."))
      jaspPlot$addCitation(gettext("Ulric Lund and Claudio Agostinelli (2017). Circular (Version 0.4-93): Circular Statistics [R Package]."))
      
      containerPlots[[variable]] <- jaspPlot
    }
  }
  
  if(ready)
    # If the calculations failed, do not fill the plots because they depend on the calculations.
    if(inherits(circularDescriptivesResults, "try-error")){
      errorMessage <- gettextf("The plotting depends on some of the calculations (e.g. the mean direction). But the calculations returned the following error: %s", as.character(circularDescriptivesResults))
      containerPlots$setError(errorMessage)
    } else {
      .circularDescriptivesFillPlots(containerPlots, circularDescriptivesResults, options, dataset, wantsSplit)
    }
}
.circularDescriptivesFillPlots <- function(containerPlots, circularDescriptivesResults, options, dataset, wantsSplit){
  
  variables <- unlist(options$variables)
  if (wantsSplit) {
    split <- dataset[[options$splitVariable]]
    splitLevels <- levels(split)
    for (variable in variables){
      for (level in splitLevels){
        plotData <- circularDescriptivesResults[["descr"]][[variable]][[level]]$plotData
        meanLength <- circularDescriptivesResults[["descr"]][[variable]][[level]]$meanLength
        meanDirection <- circularDescriptivesResults[["descr"]][[variable]][[level]]$meanDirection
        plot <- try(.circularDescriptivesCreatePlotHelper(plotData, meanDirection, meanLength, options))
        # if the plotting fails, do not fill the plot but rather show the error
        if(inherits(plot, "try-error")){
          errorMessage <- gettextf("The plotting for this set-up failed with: %s", as.character(plot))
          containerPlots[[variable]][[level]]$setError(errorMessage)
        } else {
          containerPlots[[variable]][[level]]$plotObject <- plot
        }
      }
    }
  }
  else {
    for (variable in variables){
      plotData <- circularDescriptivesResults[["descr"]][[variable]]$plotData
      meanLength <- circularDescriptivesResults[["descr"]][[variable]]$meanLength
      meanDirection <- circularDescriptivesResults[["descr"]][[variable]]$meanDirection
      plot <- try(.circularDescriptivesCreatePlotHelper(plotData, meanDirection, meanLength, options))
      # If the plotting fails, do not fill the plot but rather show the error.
      if(inherits(plot, "try-error")){
        errorMessage <- gettextf("The plotting for this set-up failed with: %s", as.character(plot))
        containerPlots[[variable]]$setError(errorMessage)
      } else {
        containerPlots[[variable]]$plotObject <- plot
      }
    }
  }
}
.circularDescriptivesCreatePlotHelper <- function(plotData, meanDirection, meanLength, options){
  # add a column to the data to for the radius of each point (it is 1 for the unit circle)
  plotData <- cbind(plotData, y = rep(1, length(plotData[[1]])))

  # The meanDirection is unnormalized for interpretation. Here we normalize it for plotting on the unit circle (plotData is already normalized).
  meanDirection <- .normalizeData(meanDirection, options$customPeriod)

  nBins <- 20    # used later for the density estimation

  # get the labels for the plot
  label1 <- 0
  label2 <- round(options$customPeriod / 4, digits = 2)
  label3 <- round(options$customPeriod / 2, digits = 2)
  label4 <- round(3 * options$customPeriod / 4, digits = 2)
  label5 <- round(options$customPeriod, digits = 2)

  pointSize <- 2
  
  if(options$distributionPlotPointStack){
    # bin all points for stacking
    his <- hist(as.numeric(plotData$x), breaks = seq(from = 0, to = 2*pi, length.out =  100), plot = FALSE)
    breaks <- his$breaks
    mids <- his$mids
    binOfDataPoints <- findInterval(plotData$x, breaks, all.inside = TRUE)
    
    # set all x values to the bin center
    plotData$x <- mids[binOfDataPoints]
    
    # for every bin with more than one data point, stack the points
    stackingBins <- which(his$counts>1)
    for (bin in stackingBins){
      points <- which(binOfDataPoints == bin)
      plotData$y[points] <- seq(from = 1, by = 0.075, length.out = length(points))    # stack points
    }
  }
  
  p <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw() +
    ggplot2::coord_polar() +
    ggplot2::scale_x_continuous("", limits = c(0, 2*pi), breaks = seq(0, 2*pi, length.out = 5), labels = c(label1, label2, label3, label4, label5)) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y, stroke = 1), show.legend = FALSE) +
    ggplot2::ylim(0, max(plotData$y) + 0.1) +    # set the y limit to the highest y value of all points determined in the routine for stacking and adds a little adjustment to not overlap with the labels
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16, face = "bold"),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.minor.x  = ggplot2::element_blank(),
                   axis.title.y  = ggplot2::element_blank(),
                   axis.text.y  = ggplot2::element_blank(),
                   axis.ticks.y  = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()
    ) +
    # visual help
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0.95 * min(y), xend = 2*pi, yend = 0.95 * min(y))) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 0.1)) + 
    ggplot2::geom_segment(ggplot2::aes(x = pi/2, y = 0, xend = pi/2, yend = 0.1)) +
    ggplot2::geom_segment(ggplot2::aes(x = pi, y = 0, xend = pi, yend = 0.1)) +
    ggplot2::geom_segment(ggplot2::aes(x = 3*pi/2, y = 0, xend = 3*pi/2, yend = 0.1))

  if (options$distributionPlotHistogram)
    p <- p + ggplot2::geom_histogram(ggplot2::aes(x = x, y = sqrt(..ncount..) * 0.9), bins = nBins, color = "black", fill = "white", linetype = "dashed", binwidth = 2*pi / nBins, center = 2*pi / nBins / 2)

  if (options$distributionPlotMeanVector)
    p <- p + ggplot2::geom_segment(x = meanDirection, y = 0, xend = meanDirection, yend = meanLength*0.95, size = 1.5, arrow = ggplot2::arrow(length = ggplot2::unit(meanLength*20, "pt"), angle = 20, type = "closed"))

  return(p)
}
# Helper functions for circular statistics ----
.normalizeData <- function(data, period){
  if(period != 1) {
    data <- (data %% period) / period
  }
  
  return(data * 2 * pi)
}
.circularDescriptivesConvertBack <- function(value, period){
  return((value / (2 * pi)) * period)
}
