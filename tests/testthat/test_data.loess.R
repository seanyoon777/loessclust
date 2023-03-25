# Load libraries
library(testthat)
library(loessclust)
library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)

# Load dataset
data(iris)
iris <- iris %>%
  select("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
x <- data.frame(iris$Sepal.Length)
Y <- iris %>%
  select(-"Sepal.Length")

# models.loess test
data.loess_test <- test_that("data.loess tests", {

  # data.loess produce correct models on generic inputs
  data_pred <- data.loess(x, Y)
  sds <- apply(data_pred, 2, sd)
  sds <- c(sds[[1]], sds[[2]], sds[[3]], sds[[4]])
  sds_expect <- c(1.2676364, 0.3092250, 1.0306964, 0.9413753)
  expect_equal(round(sds, digits = 7), sds_expect)
  mean <- apply(data_pred, 2, mean)
  mean <- c(mean[[1]], mean[[2]], mean[[3]], mean[[4]])
  mean_expect <- c(0.30995914, 0.01168202, 0.22489831, 0.17746447)
  expect_equal(round(mean, digits = 8), mean_expect)

  # models.loess produce correct models with optional parameters
  data_pred <- data.loess(x, Y, resolution = 100, span = 0.2)
  sds <- apply(data_pred, 2, sd)
  sds <- c(sds[[1]], sds[[2]], sds[[3]], sds[[4]])
  sds_expect <- c(1.2740086, 0.4588609, 1.0502528, 0.9573130)
  expect_equal(round(sds, digits = 7), sds_expect)
  mean <- apply(data_pred, 2, mean)
  mean <- c(mean[[1]], mean[[2]], mean[[3]], mean[[4]])
  mean_expect <- c(0.3099591, 0.0779245, 0.1990770, 0.1474532)
  expect_equal(round(mean, digits = 7), mean_expect)

  #models.loess produce correct output with memoization
  curves <- models.loess(x, Y)
  data_pred <- data.loess(x, Y, curves = curves)
  sds <- apply(data_pred, 2, sd)
  sds <- c(sds[[1]], sds[[2]], sds[[3]], sds[[4]])
  sds_expect <- c(1.2676364, 0.3092250, 1.0306964, 0.9413753)
  expect_equal(round(sds, digits = 7), sds_expect)
  mean <- apply(data_pred, 2, mean)
  mean <- c(mean[[1]], mean[[2]], mean[[3]], mean[[4]])
  mean_expect <- c(0.30995914, 0.01168202, 0.22489831, 0.17746447)
  expect_equal(round(mean, digits = 8), mean_expect)
})
data.loess_test


