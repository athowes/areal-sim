# Create lists of synthetic data
synthetic <- function(sf, phi_list) {
  sim_list <- lapply(phi_list, function(phi) {
    phi <- as.numeric(phi)
    n <- nrow(sf)
    m <- rep(25, n)
    x <- -2 + phi
    rho <- plogis(x)
    y <- rbinom(n, m, rho)
    sf <- st_sf(y_obs = y, n_obs = m, geometry =  sf$geometry)
    return(list(sf = sf, rho = rho, x = x, phi = phi))
  })
}

sim_iid <- function(sf, nsim, save = TRUE) {
  n <- nrow(sf)
  phi_df <- MASS::mvrnorm(n = nsim, mu = rep(0, n), Sigma = diag(n))
  phi_list <- rows_to_list(phi_df)
  list <- synthetic(sf, phi_list)
  
  if(save) {
    output_dir <- paste0("data/inputs/",  deparse(substitute(sf)), "/")
    safe_saveRDS(object = list, output_dir = output_dir, file = "iid_list")
  }
  else {
   return(list) 
  }
}

sim_icar <- function(sf, nsim, save = TRUE) {
  nb <- sf_to_nb(sf$geometry)
  g <- nb_to_graph(nb)
  
  dat <- list(
    n = length(sf$geometry),
    n_edges = g$n_edges,
    node1 = g$node1,
    node2 = g$node2
  )
  
  fit <- rstan::stan(
    file = "stan/icar.stan", 
    data = dat,
    warmup = nsim * 2,
    iter = nsim * 52,
    chains = 1, 
    thin = 50
  )
  
  phi_df <- rstan::extract(fit)$phi
  phi_list <- rows_to_list(phi_df)
  list <- synthetic(sf, phi_list)
  
  if(save) {
    output_dir <- paste0("data/inputs/",  deparse(substitute(sf)), "/")
    safe_saveRDS(object = list, output_dir = output_dir, file = "icar_list")
  }
  else {
    return(list) 
  }
}

sim_bym2 <- function(sf, pi, nsim, save = TRUE) {
  nb <- sf_to_nb(sf$geometry)
  Q <- nb_to_precision(nb)
  scale <- get_scale(Q)
  g <- nb_to_graph(nb)
  
  dat <- list(
    var_pi = pi,
    scaling_factor = scale,
    n = length(sf$geometry),
    n_edges = g$n_edges,
    node1 = g$node1,
    node2 = g$node2
  )
  
  fit <- rstan::stan(
    file = "stan/bym2.stan", 
    data = dat,
    warmup = nsim * 2,
    iter = nsim * 52,
    chains = 1, 
    thin = 50
  )
  
  phi_df <- rstan::extract(fit)$phi
  phi_list <- rows_to_list(phi_df)
  list <- synthetic(sf, phi_list)
  
  if(save) {
    output_dir <- paste0("data/inputs/",  deparse(substitute(sf)), "/")
    safe_saveRDS(object = list, output_dir = output_dir, file = paste0("bym2_", pi, "_list"))
  }
  else {
    return(list)
  }
}

sim_ck <- function(sf, nsim, save = TRUE, ...) {
  K <- centroid_covariance(sf, ...)
  K_scaled <- K / riebler_gv(K)
  phi_df <- MASS::mvrnorm(n = nsim, mu = rep(0, nrow(sf)), Sigma = K_scaled)
  phi_list <- rows_to_list(phi_df)
  list <- synthetic(sf, phi_list)
  
  if(save) {
    output_dir <- paste0("data/inputs/",  deparse(substitute(sf)), "/")
    safe_saveRDS(object = list, output_dir = output_dir, file = "ck_list")
  }
  else {
    return(list)
  }
}

sim_ik <- function(sf, L, nsim, save = TRUE, ...) {
  K <- integrated_covariance(sf, L = L, kernel = matern, type = "random", ...)
  K_scaled <- K / riebler_gv(K)
  phi_df <- MASS::mvrnorm(n = nsim, mu = rep(0, nrow(sf)), Sigma = K_scaled)
  phi_list <- rows_to_list(phi_df)
  list <- synthetic(sf, phi_list)
  
  if(save) {
    output_dir <- paste0("data/inputs/",  deparse(substitute(sf)), "/")
    safe_saveRDS(object = list, output_dir = output_dir, file = "ik_list")
  }
  else {
    return(list)
  }
}
