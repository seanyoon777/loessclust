\name{data.loess}
\alias{data.loess}
\title{Interpolate data using LOESS for each variable
}
\description{
Applies local regression (LOESS) to an input of a single variable and a second input of multiple variables, interpolating values for the variables with respect to the first input variable.}
\usage{
data.loess(x, Y, scale = TRUE, curves,
           span = 0.75, resolution = 150)
}

\arguments{
  \item{x}{An object of class "data.frame" (or one that can be coerced into that class): the variable to be used to interpolate models. ('x' in regression)
}
  \item{Y}{An object of class "data.frame" (or one that can be coerced into that class): the variables to be interpolated. ('y' in regression)}
  \item{scale}{Optional. A boolean to determine whether data should be scaled. Default value is TRUE.}
  \item{curves}{Optional. A list of LOESS models, where each model interpolates the given variables in Y using x. Can reduce computation time. }
  \item{span}{Optional. A numeric \code{α} which controls the degree of smoothing.}
  \item{resolution}{Optional. An integer that determines the interval of the \code{x} when interpolating.}
}
\details{
The function will use default parameters specified in \link{models.loess} for LOESS interpolation.
}
\value{
\item{data_pred}{A data.frame where the first column is evenly spaced values of \code{x}, and the remaining columns are interpolated values of the other variables.}
}

\seealso{
See \link{loess} for parameters used in curve fitting.
Also see \link{models.loess} for possible inputs for the \code{curves} parameter.
}
\examples{
iris_models <- models.loess(iris[1], iris[2:4], control=loess.control(surface="direct"))
data.loess(iris[1], iris[2:4], curves = iris_models)
}


