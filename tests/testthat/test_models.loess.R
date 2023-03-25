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
models.loess_test <- test_that("models.loess tests", {

  # models.loess produce correct models on generic inputs
  curves <- models.loess(x, Y)
  expect_equal(round(curves[[1]]$s, digits = 7), 0.9549919)
  expect_equal(round(curves[[2]]$s, digits = 7), 0.4466225)
  expect_equal(round(curves[[3]]$s, digits = 7), 0.5270514)

  # models.loess produce correct models with optional parameters
  curves <- models.loess(x, Y, family = "symmetric", span = 0.2)
  expect_equal(round(curves[[1]]$s, digits = 7), 0.6868258)
  expect_equal(round(curves[[2]]$s, digits = 7), 0.2600249)
  expect_equal(round(curves[[3]]$s, digits = 7), 0.4609157)

  #models.loess produce correct models with non-dataframes
  x <- iris$Sepal.Length # numeric vector
  Y <- as.matrix(Y) # matrix array
  curves <- models.loess(x, Y)
  expect_equal(round(curves[[1]]$s, digits = 7), 0.9549919)
  expect_equal(round(curves[[2]]$s, digits = 7), 0.4466225)
  expect_equal(round(curves[[3]]$s, digits = 7), 0.5270514)
})
models.loess_test


