# Load libraries
library(testthat)
library(loessclust)
library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)

# Synthesize dataset
set.seed(333)
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

# models.loess test
cluster.loess_test <- test_that("cluster.loess tests", {

  # cluster.loess produce correct output on generic inputs
  curves <- models.loess(x, Y, scale = FALSE)
  data <- data.loess(x, Y, curves = curves, scale = FALSE)
  cluster <- cluster.loess(x, Y, curves = curves, data_pred = data, scale = FALSE)
  unique_cluster <- unique(cluster$cluster)
  nclust <- length(unique_cluster)
  expect_equal(nclust, 6)
})
cluster.loess_test
