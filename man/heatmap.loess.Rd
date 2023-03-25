\name{heatmap.loess}
\alias{heatmap.loess}
\title{Create heatmap of LOESS-interpolated time series data}
\description{
Applies local regression (LOESS) to an input of a single variable and a second input of multiple variables, creating a heatmap of interpolated values for each variable.
}
\usage{
heatmap.loess(x, Y, curves, data_pred, scale = TRUE,
              resolution = 150, span = 0.75,
              title, axis.label.x, axis.label.y,
              axis.line.x, axis.line.y,
              axis.title.x, axis.title.y,
              color.min = "blue", color.max = "red")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class "data.frame" (or one that can be coerced into that class): the variable to be used to interpolate models. ('x' in regression)
}
  \item{Y}{An object of class "data.frame" (or one that can be coerced into that class): the variables to be interpolated. ('y' in regression)}
  \item{scale}{Optional. A boolean to determine whether data should be scaled. Default value is TRUE.}
  \item{curves}{Optional. A list of LOESS models, where each model interpolates the given variables in Y using x. Can reduce computation time. }
  \item{data_pred}{Optional. A "data.frame" of predicted data, where the first column is the \code{x} variable values. Can reduce computation time. }
  \item{span}{Optional. A numeric \code{α} which controls the degree of smoothing.}
  \item{resolution}{Optional. An integer that determines the interval of the \code{x} when interpolating.}
  \item{title}{Optional. A string for the plot title.}
  \item{axis.label.x}{Optional. A boolean that determines whether x label should be printed in the graph.}
  \item{axis.label.y}{Optional. A boolean that determines whether y label should be printed in the graph.}
  \item{axis.line.x}{Optional. A boolean that determines whether x axis lines should be printed in the graph.}
  \item{axis.line.y}{Optional. A boolean that determines whether y axis lines should be printed in the graph.}
  \item{axis.title.x}{Optional. A string for the x axis title.}
  \item{axis.title.y}{Optional. A string for the y axis title.}
  \item{color.min}{Optional. The color for minimum value. Default is blue.}
  \item{color.max}{Optional. The color for maximum value. Default is red. }
}
\details{
The function will use default parameters specified in \link{models.loess} for LOESS interpolation.
}
\value{
A \link{ggplot} plot where each row in the heatmap is the interpolated value for each variable.
}
\seealso{
See \link{loess} for parameters used in curve fitting. Also see \link{models.loess} for possible inputs for the curves parameter.
}
