\name{models.loess}
\alias{models.loess}
\title{Compute LOESS models for each variable}
\description{Applies local regression (LOESS) to an input of a single variable and a second input of multiple variables, generating multiple LOESS models that are based on the first input variable.}
\usage{
models.loess(x, Y, scale = TRUE, weights, subset, na.action,
             span = 0.75, enp.target, degree = 2L, parametric = FALSE,
             drop.square = FALSE, normalize = TRUE,
             family = c("gaussian", "symmetric"),
             method = c("loess", "model.frame"),
             control = loess.control(surface="direct"), ...)
}

\arguments{
  \item{x}{An object of class "data.frame" (or one that can be coerced into that class): the variable to be used to interpolate models. (`x` in regression)}
  \item{Y}{An object of class "data.frame" (or one that can be coerced into that class): the variables to be interpolated. (`y` in regression)}
  \item{scale}{A boolean to determine whether data should be scaled. Default value is \code{TRUE}.}
  \item{...}{Remaining parameters are parameters that will be used in the LOESS fiting process. Refer to \link{loess} for remaining parameters. }
}
}
\value{
\item{curves}{A list of LOESS models, where each model interpolates the given variables in \code{Y} using \code{x}.}
}
\seealso{
Also see \link{loess} for other parameters.
}
\examples{
iris_models <- models.loess(iris[1], iris[2:4], control=loess.control(surface="direct"))
predict(iris_models, seq(2, 7, 0.5))
}
