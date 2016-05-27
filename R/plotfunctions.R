#' Package plotfunctions: Various Functions to Facilitate Visualization 
#' of Data and Analysis
#' 
#' This package provides a set of simple 
#' tools for building plots incrementally, starting with an empty plot region, 
#' and adding bars, data points, regression lines, error bars, gradient 
#' legends, density distributions in the margins, and even pictures. The 
#' package builds further on R graphics by simply combining functions and 
#' settings in order to reduce the amount of code to produce for the user. As 
#' a result, the package does not use formula input or special syntax, but can 
#' be used in combination with default R plot functions. 
#' 
#' Note: Most of the functions were part of the package itsadug, which is now 
#' split in two packages: 1. the package \code{itsadug}, which contains the 
#' core functions for visualizing and evaluating nonlinear regression models, 
#' and 2. the package \code{plotfunctions}, which contains more general plot 
#' functions.
#' 
#'
#' @section Basic functions:
#' \itemize{
#' \item \code{\link{emptyPlot}} generates an empty plot.
#' \item \code{\link{plot_error}} adds line with (shaded) confidence interval.
#' \item \code{\link{add_bars}} adds bars to a (bar)plot.
#' \item \code{\link{errorBars}} adds confidence intervals to points or bars.
#' }
#' 
#' @section Specialized plots:
#' \itemize{
#' \item \code{\link{color_contour}} and \code{\link{plotsurface}} are 
#' wrappers around \code{\link[graphics]{image}} and 
#' \code{\link[graphics]{contour}} for making easily colored surface plots 
#' for interactions with two (or more) continuous predictors.
#' \item \code{\link{plot_image}} can be used to add a picture to a plot, 
#' or to make a picture the background of a plot.
#' \item \code{\link{marginDensityPlot}} adds distributions in the margins of 
#' the plot.
#' \item \code{\link{check_normaldist}} overlays density of data to normal 
#' distribution. Might help with interpretation of QQ-plots that are generally 
#' used to test for normality.
#' }
#' 
#' @section Other useful features:
#' \itemize{
#' \item \code{\link{alpha}} and \code{\link{alphaPalette}} are simple 
#' function to make colors and palettes transparent.
#' \item \code{\link{legend_margin}} adds a legend in the margins of a plot.
#' \item \code{\link{gradientLegend}} adds a color legend to a plot.
#' \item \code{\link{drawArrows}} for drawing arrows or lines between 
#' different panels.
#' \item \code{\link{getFigCoords}} retrieve the cartesian coordinates 
#' relative to the plot axes for given proportions of the plot region or given 
#' proportions of the figure. 
#' #' \item A list of all available functions is provided in 
#' \code{help(package="itsadug")}.
#' }
#'
#' @author
#' Jacolien van Rij
#' 
#' Maintainer: Jacolien van Rij (\email{vanrij.jacolien@gmail.com})
#'
#' University of Groningen, The Netherlands & University of Tuebingen, Germany
#' @docType package
#' @name plotfunctions
NULL





