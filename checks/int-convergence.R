# Investigating convergence of the integrated kernel depending on number of samples per area

type <- "random"
L <- 200
geometry <- create_sf_grid(height = 6, width = 6)
grid <- sf::st_sf(geometry)
n <- nrow(grid)
samples <- sf::st_sample(grid, type = type, exact = TRUE, size = rep(L, n))
length(samples) # n * L
sf::st_crs(samples) <- NA

# Add id column
samples <- st_sf(id = rep(seq_along(1:L), n), geom = samples)

adapted_integrated_covariance <- function(samples) {
  sample_index <- sf::st_intersects(grid, samples)
  D <- sf::st_distance(samples, samples)
  kD <- matern(D, l = 1)
  K <- matrix(nrow = n, ncol = n)
  # Diagonal entries
  for(i in 1:(n - 1)) {
    K[i, i] <- mean(kD[sample_index[[i]], sample_index[[i]]])
    for(j in (i + 1):n) {
      # Off-diagonal entries
      K[i, j] <- mean(kD[sample_index[[i]], sample_index[[j]]]) # Fill the upper triangle
      K[j, i] <- K[i, j] # Fill the lower triangle
    }
  }
  K[n, n] <- mean(kD[sample_index[[n]], sample_index[[n]]])
  return(K)
}

sample_sets <- lapply(seq(10, L, by = 10), function(i) subset(samples, id <= i))

plot_samples <- function(samples){
  ggplot(grid) +
    geom_sf(fill = "lightgrey") +
    geom_sf(data = samples, alpha = 0.5, shape = 4) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    labs(subtitle = paste0(length(samples$geom), " samples"), fill = "") +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

# Checking that the samples look OK
cowplot::plot_grid(
  plot_samples(sample_sets[[1]]),
  plot_samples(sample_sets[[10]]),
  plot_samples(sample_sets[[20]]),
  ncol = 3
)

# Calculate the Gram matrices
covs <- lapply(sample_sets, adapted_integrated_covariance)

cowplot::plot_grid(
  plot_matrix(covs[[1]]),
  plot_matrix(covs[[10]]),
  plot_matrix(covs[[20]]),
  ncol = 3
)

# Simple metric between matrices of identical dimension
frobenius <- function(M1, M2) {
  diff <- M1 - M2
  sqrt(sum(diff^2))
}

matrix_comparison <- outer(covs, covs, Vectorize(frobenius))

plot_matrix(matrix_comparison)

# Experiment: 10, 20 and 50 points versus ground truth 300 points
L_settings <- list(10, 20, 50)
gt_samples <- sf::st_sample(grid, type = type, exact = TRUE, size = rep(300, n))
gt_cov <- adapted_integrated_covariance(gt_samples)

int_convergence_experiment <- function(L, n_sim = 100) {
  sample_sets <- lapply(1:n_sim, function(i) sf::st_sample(grid, type = type, exact = TRUE, size = rep(L, n)))
  covs <- lapply(sample_sets, adapted_integrated_covariance)
  frobs <- sapply(covs, function(cov) frobenius(gt_cov, cov))
  return(frobs)
}

result <- lapply(L_settings, int_convergence_experiment)
result
