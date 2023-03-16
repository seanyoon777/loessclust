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

\examples{
## Create data; can disregard
x_min <- -4
x_max <- 4
x <- data.frame(x = sort(runif(40, x_min, x_max)))
n_clusters <- 6
cluster_size <- c(700, 500, 300, 400, 550, 50)
Y <- data.frame(matrix(nrow = 40, ncol = 2500))

start_value <- c(1, 0, 0, 0, -1, -1)
end_value <- c(-1, -1, -1, 1, 0, 1)

x_max <- max(x)
x_min <- min(x)

for (i in 1:n_clusters) {
  # Generate sigmoid function for cluster
  sigmoid_fun <- function(n) {
    temp <- 1 / (1 + exp(-n))
    start_value[i] + (end_value[i] - start_value[i]) * temp
  }

  gamma_fun <- function(n) {
    # Generate gamma distribution CDF for cluster 3
    n_norm <- (n - x_min) / (x_max - x_min)
    -pgamma(n_norm, shape=0.25, rate = 1.5)
  }

  # Assign values using sigmoid function with noise
  start_index <- sum(cluster_size[0:i]) - cluster_size[i] + 1
  end_index <- start_index + cluster_size[i] - 1
  for (j in start_index:end_index) {
    noise <- runif(40, min = -0.5, max = 0.5)
    if (i == 3) {
      Y[1:40, j] <- sapply(x, gamma_fun) + noise
    } else {
      Y[1:40, j] <- sapply(x, sigmoid_fun) + noise
    }
  }
}
colnames(Y) <- paste0("Y", 1:2500)

## Use cluster function
cluster <- cluster.loess(x, Y, scale = FALSE)
head(cluster)
}
