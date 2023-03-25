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

############################ FUNCTION IMPLEMTATIONS ############################

# FUNCTION: models.loess
# ----------------------
#
# Produces LOESS models that interpolate for each variables in Y, with respect to x.
models.loess <- function(x, Y, scale = TRUE, weights, subset, na.action,
                         span = 0.75, enp.target, degree = 2L, parametric = FALSE,
                         drop.square = FALSE, normalize = TRUE,
                         family = c("gaussian", "symmetric"),
                         method = c("loess", "model.frame"),
                         control = loess.control(surface="direct"), ...) {
  # Set up names and variables
  Y <- data.frame(Y)
  names_Y <- names(Y)
  models <- vector("list", length(names_Y))

  # Scale input data
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
    models[[i]] <- loess(Y[[i]] ~ x[,1], weights = weights, subset = subset,
                         na.action = na.action, span = span, enp.target =
                           enp.target, degree = degree, parametric = parametric,
                         drop.square = drop.square, normalize = normalize,
                         family = family, method = method,
                         control = control, ...)
  }

  return(models)
}


# FUNCTION: data.loess
# --------------------
#
# Produces interpolated data for each variables in Y, with respect to x. Uses
# default LOESS values in models.loess
data.loess <- function(x, Y, scale = TRUE, curves, span = 0.75, resolution = 150) {
  # Scale input data
  if (scale == TRUE) {
    x <- scale(x)
    Y <- scale(Y)
  }
  x <- data.frame(x)
  Y <- data.frame(Y)

  # Get LOESS models
  if (missing(curves)) {
    warning("default LOESS parameters used for value interpolation")
    curves <- models.loess(x, Y, span = span, control=loess.control(surface="direct"))
  }

  # Set up names and variables
  names_Y <- names(Y)
  x_seq <- seq(min(x), max(x), length.out = resolution)
  data_pred <- data.frame(axis.values.x = x_seq)

  # Iterate through Y values to interpolate
  for (i in seq_along(names_Y)) {
    data_pred[, i + 1] <- predict(curves[[i]], x_seq)
  }
  names(data_pred) <- c("axis.values.x", names_Y)
  return(data_pred)
}


# FUNCTION: cluster.loess
# -----------------------
#
# Clusters the variables by their trends with respect to the single variable x.
# Uses default LOESS values in models.loess
cluster.loess <- function(x, Y, curves, data_pred, scale = TRUE, nclust,
                          resolution = 150, loess.span = 0.75,
                          clust.method = "ward.D") {
  # Scale data
  if (scale == TRUE) {
    x <- scale(x)
    Y <- scale(Y)
  }
  x <- data.frame(x)
  Y <- data.frame(Y)

  # Get curves and data (memoization implemented to speed up computations!)
  if (missing(data_pred)) {
    if (missing(curves)) {
      warning("default LOESS parameters used for value interpolation")
      curves <- models.loess(x, Y, span = loess.span, scale = FALSE)
    }
    data_pred <- data.loess(x, Y, curves = curves, resolution = resolution, scale = FALSE)
  }

  # Compute optimal number of clusters using the silhouette method
  data_dist <- dist(t(data_pred[-1]), method="euclidean")
  data_clust <- hclust(data_dist, method = "ward.D")
  if (missing(nclust)) {
    max_clust <- min(nrow(data_pred) - 2, 10)
    silhouette_list <- numeric(max_clust - 1)
    for (k in 2:max_clust) {
      cut_clusters <- cutree(data_clust, k)
      vals <- silhouette(cut_clusters, data_dist)
      silhouette_val <- mean(vals[, 3])
      silhouette_list[k - 1] <- silhouette_val
    }
    nclust <- which.max(silhouette_list) + 1
  }
  print(paste("optimal number of clusters: ", nclust))

  # Cluster data with optimal number of clusters
  clust <- cutree(data_clust, k = nclust) %>%
    data.frame(cluster = .)
  clust$variables <- names(Y)
  data_long <- data_pred %>% gather(key = variables, value = value, -axis.values.x)
  data_clust <- data_long %>% inner_join(clust, by = "variables") %>%
    arrange(cluster, variables, axis.values.x)

  return(data_clust)
}


# FUNCTION: heatmap.loess
# -----------------------
#
# Produce heatmap for interpolated data for each variables in Y, with respect to
# x. Uses default LOESS values in models.loess
heatmap.loess <- function(x, Y, curves, data_pred, scale = TRUE,
                          resolution = 150, span = 0.75,
                          title, axis.label.x, axis.label.y,
                          axis.line.x, axis.line.y,
                          axis.title.x, axis.title.y,
                          color.min = "blue", color.max = "red") {
  # Scale data
  if (scale == TRUE) {
    x <- scale(x)
    Y <- scale(Y)
  }
  x <- data.frame(x)
  Y <- data.frame(Y)

  # Get curves and data (memoization implemented to speed up computations!)
  if (missing(data_pred)) {
    if (missing(curves)) {
      warning("default LOESS parameters used for value interpolation")
      curves <- models.loess(x, Y, span = span, control=loess.control(surface="direct"))
    }
    data_pred <- data.loess(x, Y, scale = FALSE, curves, resolution)
  }

  # Melt data
  data_long <- data_pred %>%
    gather(key = variables, value = value, -axis.values.x)

  # Generate figure
  figure <- ggplot(data_long, aes(x = axis.values.x, y = variables, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = color.min, high = color.max) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank())

  # Additional parameters for heatmap
  if (!missing(title)) figure <- figure + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  if (!missing(axis.title.x)) figure <- figure + theme(axis.title.x = element_blank())
  if (!missing(axis.title.y)) figure <- figure + theme(axis.title.y = element_blank())
  if (!missing(axis.label.x)) figure <- figure + xlab(axis.label.x)
  if (!missing(axis.label.y)) figure <- figure + ylab(axis.label.y)
  if (!missing(axis.line.x)) figure <- figure + theme(axis.line.x = axis.line.x)
  if (!missing(axis.line.y)) figure <- figure + theme(axis.line.y = axis.line.y)

  return(figure)
}


# FUNCTION: clusterplot.loess
# ---------------------------
#
# Produce a plot where trends for different clusters are shown side-by-side for
# interpolated data for each variables in Y, with respect to x. Uses default
# LOESS values in models.loess
clusterplot.loess <- function(x, Y, curves, data_pred, cluster, scale = TRUE,
                       nclust, resolution = 150, loess.span = 0.75,
                       clust.method = "ward.D",
                       title, axis.label.x, axis.label.y,
                       axis.line.x, axis.line.y,
                       axis.title.x, axis.title.y) {
  # Scale data
  if (scale == TRUE) {
    x <- scale(x)
    Y <- scale(Y)
  }
  x <- data.frame(x)
  Y <- data.frame(Y)

  # Get curves and data (memoization implemented to speed up computations!)
  if (missing(cluster)) {
    if (missing(data_pred)) {
      if (missing(curves)) {
        warning("default LOESS parameters used for value interpolation")
        curves <- models.loess(x, Y, span = loess.span)
      }
      data_pred <- data.loess(x, Y, curves = curves, resolution = resolution)
    }
    if (missing(nclust)) {
      cluster <- cluster.loess(x, Y, curves = curves, data_pred = data_pred)
    } else {
      cluster <- cluster.loess(x, Y, curves = curves, data_pred = data_pred, nclust = nclust)
    }
  }

  # Generate plot
  figure <- cluster %>%
    ggplot(aes(x = axis.values.x, y = value, group = variables, color = factor(cluster))) +
    geom_line(stat = "smooth", method = "loess", se = FALSE, linewidth = 0.5, alpha = 0.1) +
    scale_color_manual(
      values = c("#FF6F61", "#6B5B95", "#88B04B", "#FFA500", "#92A8D1", "#FF69B4", "#955251")) +
    labs(x = "X-axis", y = "Y-axis", color = "Cluster") +
    facet_wrap(~ cluster, ncol = 3, nrow = 4, labeller = labeller(cluster = as.character)) +
    stat_summary(fun.data = "mean_cl_normal", geom = "line",
                 aes(group = cluster, color = factor(cluster)),
                 linewidth = 0.8, alpha = 1, linetype = "solid", color = "white") +
    stat_summary(fun.data = "mean_cl_normal", geom = "line",
                 aes(group = cluster, color = factor(cluster)),
                 linewidth = 0.5, alpha = 0.8)

  # Additional parameters for plot
  if (!missing(title)) figure <- figure + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  if (!missing(axis.title.x)) figure <- figure + theme(axis.title.x = element_blank())
  if (!missing(axis.title.y)) figure <- figure + theme(axis.title.y = element_blank())
  if (!missing(axis.label.x)) figure <- figure + xlab(axis.label.x)
  if (!missing(axis.label.y)) figure <- figure + ylab(axis.label.y)
  if (!missing(axis.line.x)) figure <- figure + theme(axis.line.x = axis.line.x)
  if (!missing(axis.line.y)) figure <- figure + theme(axis.line.y = axis.line.y)

  return(figure)
}


