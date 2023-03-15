# Copyright (c) 2023 Sean Yoon
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.





# FUNCTION: models.loess
# ----------------------
#
# The function  Produce LOESS models for given X features
models.loess <- function(x, Y, scale = TRUE, weights, subset, na.action,
                         span = 0.75, enp.target, degree = 2L, parametric = FALSE,
                         drop.square = FALSE, normalize = TRUE,
                         family = c("gaussian", "symmetric"),
                         method = c("loess", "model.frame"),
                         control = loess.control(surface="direct"), ...) {
  Y <- data.frame(Y)
  names_Y <- names(Y)
  models <- vector("list", length(names_Y))
  if (missing(scale)) scale <- TRUE
  if (scale == TRUE) {
    x <- data.frame(scale(x))
    Y <- data.frame(scale(Y))
  }
  # Modify missing values
  if (missing(weights)) weights <- rep(1, nrow(x))
  if (missing(subset)) subset <- NULL
  if (missing(na.action)) na.action <- NULL
  mf <- match.call(expand.dots = FALSE)
  mf$model <- mf$span <- mf$enp.target <- mf$degree <-
    mf$parametric <- mf$drop.square <- mf$normalize <-
    mf$family <- mf$method <- mf$control <- mf$... <- NULL

  # Iterate through Y values to determine LOESS models
  for (i in seq_along(names_Y)) {
    models[[i]] <- loess(Y[[i]] ~ x[,1], weights = weights, subset = subset, na.action = na.action,
                         span = span, enp.target = enp.target, degree = degree, parametric = parametric,
                         drop.square = drop.square, normalize = normalize,
                         family = family, method = method,
                         control = control, ...)
  }

  return(models)
}


data.loess <- function(x, Y, scale = TRUE, curves, span = 0.75, resolution = 150) {
  if (scale == TRUE) {
    x <- data.frame(scale(x))
    Y <- data.frame(scale(Y))
  }
  if (missing(curves)) {
    warning("default LOESS parameters used for value interpolation")
    curves <- models.loess(x, Y, span = span, control=loess.control(surface="direct"))
  }
  names_Y <- names(Y)
  x_seq <- seq(min(x), max(x), length.out = resolution)
  data_pred <- data.frame(axis.values.x = x_seq)

  for (i in seq_along(names_Y)) {
    data_pred[, i + 1] <- predict(curves[[i]], x_seq)
  }
  names(data_pred) <- c("axis.values.x", names_Y)
  return(data_pred)
}

# Produce heatmap where each row is estimated using LOESS
heatmap.loess <- function(x, Y, curves, data_pred, scale = TRUE,
                          resolution = 150, span = 0.75,
                          title, axis.label.x, axis.label.y,
                          #themes = c(), ...
                          color.min = "blue", color.max = "red",
                          axis.line.x, axis.line.y#,
                          #axis.ticks.x, axis.ticks.y
) { #TO DO: ADD cluster = FALSE
  if (scale == TRUE) {
    x <- data.frame(scale(x))
    Y <- data.frame(scale(Y))
  }
  if (missing(data_pred)) {
    if (missing(curves)) {
      warning("default LOESS parameters used for value interpolation")
      curves <- models.loess(x, Y, span = span, control=loess.control(surface="direct"))
    }
    data_pred <- data.loess(x, Y, scale = FALSE, curves, resolution)
  }

  data_long <- data_pred %>%
    gather(key = variables, value = value, -axis.values.x)

  figure <- ggplot(data_long, aes(x = axis.values.x, y = variables, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = color.min, high = color.max) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank())

  if (!missing(title)) figure <- figure + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  if (!missing(axis.label.x)) figure <- figure + xlab(axis.label.x)
  if (!missing(axis.label.y)) figure <- figure + ylab(axis.label.y)
  if (!missing(axis.line.x)) figure <- figure + theme(axis.line.x = axis.line.x)
  if (!missing(axis.line.y)) figure <- figure + theme(axis.line.y = axis.line.y)

  return(figure)
}
#TESTING: heatmap.loess(iris$Sepal.Length, data.frame(Sepal.Width = iris$Sepal.Width, Petal.Width = iris$Petal.Width, Petal.Length = iris$Petal.Length))


cluster.loess <- function(x, Y, curves, data_pred, scale = TRUE,
                          resolution = 150, loess.span = 0.75,
                          clust.method = "ward.D", B = 100) {
  if (scale == TRUE) {
    x <- data.frame(scale(x))
    Y <- data.frame(scale(Y))
  }
  if (missing(data_pred)) {
    if (missing(curves)) {
      warning("default LOESS parameters used for value interpolation")
      curves <- models.loess(x, Y, span = loess.span)
    }
    data_pred <- data.loess(x, Y, curves = curves, resolution = resolution)
  }

  data_dist <- dist(t(data_pred[-1]), method="euclidean")
  data_clust <- hclust(data_dist, method = "ward.D")

  wss <- function(d) {
    sum(scale(d, scale = FALSE)^2)
  }

  wrap <- function(i, hc, x) {
    cl <- cutree(hc, i)
    spl <- split(x, cl)
    wss <- sum(sapply(spl, wss))
    wss
  }

  max_clust <- min(nrow(data_pred) - 2, 10)
  wss <- sapply(seq.int(1, max_clust), wrap, h = data_clust, x = t(data_pred[-1]))
  wss_2nd_der <- diff(diff(wss))
  nclust <- which(wss_2nd_der < 0)[1] + 1
  print(paste("optimal number of clusters: ", nclust))

  # Cluster data
  clust <- cutree(data_clust, k = nclust) %>%
    data.frame(cluster = .)
  clust$variables <- names(Y)
  data_long <- data_pred %>% gather(key = variables, value = value, -axis.values.x)
  data_clust <- data_long %>% inner_join(clust, by = "variables") %>%
    arrange(cluster, variables, axis.values.x)

  return(data_clust)
}



#cluster.plot <- function(data_long)
