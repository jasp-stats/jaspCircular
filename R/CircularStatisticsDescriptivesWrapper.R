#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Hypothesis Tests
#'
CircularStatisticsDescriptives <- function(
          data = NULL,
          version = "0.95",
          customPeriod = 360,
          distributionPlot = FALSE,
          distributionPlotHistogram = FALSE,
          distributionPlotMeanVector = FALSE,
          distributionPlotPointStack = FALSE,
          meanDirection = TRUE,
          meanLength = FALSE,
          median = FALSE,
          period = "pi2",
          plotHeight = 320,
          plotWidth = 480,
          range = FALSE,
          splitVariable = list(types = list(), value = ""),
          standardDeviation = TRUE,
          variables = list(types = list(), value = list()),
          variance = FALSE) {

   defaultArgCalls <- formals(jaspCircular::CircularStatisticsDescriptives)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("splitVariable", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspCircular", "CircularStatisticsDescriptives", "CircularStatisticsDescriptives.qml", options, version, TRUE))
}