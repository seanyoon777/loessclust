\name{cluster.loess}
\alias{cluster.loess}
\title{Cluster LOESS-interpolated time series data by trend
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cluster.loess(x, Y, curves, data_pred, scale = TRUE, nclust,
              resolution = 150, loess.span = 0.75,
              clust.method = "ward.D")
}
\arguments{
  \item{x}{An object of class "data.frame" (or one that can be coerced into that class): the variable to be used to interpolate models. ('x' in regression)}
  \item{Y}{	An object of class "data.frame" (or one that can be coerced into that class): the variables to be interpolated. ('y' in regression)}
  \item{curves}{Optional. A list of LOESS models, where each model interpolates the given variables in Y using x. Can reduce computation time.}
  \item{data_pred}{Optional. A "data.frame" of predicted data, where the first column is the x variable values. Can reduce computation time.}
  \item{scale}{Optional. A boolean to determine whether data should be scaled. Default value is TRUE.}
  \item{nclust}{Optional. An integer for desired number of clusters. If not specified, automatically determines optimal number of clusters using the Silhouette method. }
  \item{resolution}{Optional. An integer that determines the interval of the \code{x} when interpolating.}
  \item{loess.span}{Optional. A numeric \code{α} which controls the degree of smoothing.}
  \item{clust.method}{Optional. Method for hierarchical clustering. Unless specified, uses "ward.D".}
}
\details{
The optimal number of clusters is decided by the number of clusters that has the highest silhouette coefficient. Also, the function will use default parameters specified in \link{models.loess} for LOESS interpolation.
}
\value{
  \item{cluster}{A "data.frame" where each row contains \code{x} values, name of the variable, its LOESS interpolated value, and cluster number. }
}

\seealso{
See \link{loess} for parameters used in curve fitting. Also see \link{models.loess} for possible inputs for the curves parameter.}


