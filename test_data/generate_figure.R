# Set seed
set.seed(333)

# Create random dataset. Dataset is created to have 40 observations and 2500
# variables. It is designed to be scaled and have 6 clusters as the following:
  # Cluster 1: 700 elements, goes from high to low z-score (1 to -1)
  # Cluster 2: 500 elements, goes from mid to low z-score (0 to -1)
  # Cluster 3: 300 elements, goes from mid to low z-score (0 to -1)
    # But cluster 3 would have an different shape from cluster 2. This would make
    # sure that the function can classify using trends, not only start and end
    # points of the LOESS curves. (Created using Gamma distribution CDF)
  # Cluster 4: 400 elements, goes from mid to high z-score (0 to 1)
  # Cluster 5: 550 elements, goes from low to mid z-score (-1 to 0)
  # Cluster 6: 50 elements, goes from low to high z-score (-1 to 1)
    # Cluster 6 is designed to have a small but significant number of elements.
    # (5% of the total) This will check if the function can classify trends that
    # are significant but have a small number of elements, instead of including
    # cluster 6 in 4 or 5.
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

# Test each function
curves <- models.loess(x, Y, scale = FALSE)
data <- data.loess(x, Y, curves = curves, scale = FALSE)
cluster <- cluster.loess(x, Y, curves = curves, data_pred = data, scale = FALSE)

# Display figure for visual inspection
fig2 <- cluster %>%
  ggplot(aes(x = axis.values.x, y = value, group = variables, color = factor(cluster))) +
  geom_line(stat = "smooth", method = "loess", se = FALSE, linewidth = 0.5, alpha = 0.1) +
  scale_color_manual(values = c("#FF6F61", "#6B5B95", "#88B04B", "#FFA500", "#92A8D1", "#FF69B4", "#955251")) +
  labs(x = "X-axis", y = "Y-axis", color = "Cluster") +
  facet_wrap(~ cluster, ncol = 3, nrow = 2, labeller = labeller(cluster = as.character)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "line",
               aes(group = cluster, color = factor(cluster)),
               linewidth = 0.8, alpha = 1, linetype = "solid", color = "white") +
  stat_summary(fun.data = "mean_cl_normal", geom = "line",
               aes(group = cluster, color = factor(cluster)),
               linewidth = 0.5, alpha = 0.8)

fig2
