# Investigating the sensitivity of the length-scale posterior to prior specification

library(bsae)
library(sf)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

geometry <- create_sf_grid(height = 6, width = 6)
grid <- sf::st_sf(geometry)
ck_one <- sim_ck(grid, nsim = 10, l = 1, save = FALSE)
sf <- ck_one[[1]]$sf

D <- centroid_distance(sf)
dat <- list(n = nrow(sf), y = sf$y, m = sf$n_obs, mu = rep(0, nrow(sf)), D = D)

# No prior on l (uniform): this shouldn't be a good idea
fit_non <- rstan::stan("../stan/prior-sensitivity/non-informative.stan", data = dat, warmup = 100, iter = 1000)
# Nice visual checking tool--a lot of between chain posterior variability would be a bad sign
bayesplot::mcmc_hist_by_chain(fit_non, pars = "l")

# l ~ Gamma(1, 1)
fit_gamma <- rstan::stan("../stan/prior-sensitivity/gamma_.stan", data = dat, warmup = 100, iter = 1000)
bayesplot::mcmc_hist_by_chain(fit_non, pars = "l")

# l ~ N+(0, (ub - lb) / 3): Half-normal with standard deviation based on range of distances
# https://betanalpha.github.io/assets/case_studies/gp_part3/part3.html
dist <- as.vector(D)
lb <- min(dist) # Could do dist[dist > 0] here but results in lb = 1
lb <- 0.1
ub <- max(dist)
(ub - lb) / 3 # Approximately 2.3
fit_normal_data <- rstan::stan("../stan/prior-sensitivity/normal-data.stan", data = dat, warmup = 100, iter = 1000)
bayesplot::mcmc_hist_by_chain(fit_normal_data, pars = "l")

# l ~ N(1, 0.5): Informative prior at the true value
fit_normal_inform <- rstan::stan("../stan/prior-sensitivity/informative.stan", data = dat, warmup = 100, iter = 1000)
bayesplot::mcmc_hist_by_chain(fit_normal_inform, pars = "l")

# l ~ Inverse-Gamma(a, b):
# https://warwick.ac.uk/fac/sci/statistics/crism/workshops/masterclassapril/presentation4.pdf

# Copied from brms (https://github.com/paul-buerkner/brms/commit/524e738aeeb82e49c5338839c3e10113763b6de1)
# plb: prior probability of being lower than minimum length-scale
# pub: prior probability of being higher than maximum length-scale
opt_fun <- function(x, lb, ub) {
  # optimize parameters on the log-scale to make them positive only
  x <- exp(x)
  y1 <- invgamma::pinvgamma(lb, x[1], x[2], log.p = TRUE)
  y2 <- invgamma::pinvgamma(ub, x[1], x[2], lower.tail = FALSE, log.p = TRUE)
  c(y1 - log(plb), y2 - log(pub))
}

opt_fun(c(0, 0), lb = lb, ub = ub)

plb <- 0.01
pub <- 0.01

opt_res <- nleqslv::nleqslv(
  c(0, 0), opt_fun, lb = lb, ub = ub,
  control = list(allowSingular = TRUE)
)

exp(opt_res$x) # Approximately a = 1.7, b = 0.6

fit_invgamma_data <- rstan::stan("../stan/prior-sensitivity/invgamma-data.stan", data = dat, warmup = 100, iter = 1000)
bayesplot::mcmc_hist_by_chain(fit_invgamma_data, pars = "l")

# Prior comparison
ggplot(data = data.frame(x = c(0, 10)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 2.3), col = cbpalette[1]) +
  stat_function(fun = dgamma, n = 101, args = list(1, 1), col = cbpalette[2]) +
  stat_function(fun = invgamma::dinvgamma, n = 101, args = list(1.7, 0.6), col = cbpalette[3]) +
  stat_function(fun = dnorm, n = 101, args = list(1, 0.5), col = cbpalette[4]) +
  stat_function(fun = dunif, n = 101, args = list(0, 100), col = cbpalette[5]) +
  labs(x = "l", y = "p(l)")

# Posterior comparison
models <- list(
  "Non informative" = fit_non, 
  "Gamma" = fit_gamma, 
  "Data informed normal" = fit_normal_data,
  "Informative normal" = fit_normal_inform,
  "Informative inverse gamma" = fit_invgamma_data
)

draws <- lapply(models, FUN = function(model) rstan::extract(model)$l)
df <- do.call("rbind", lapply(seq_along(draws), 
  FUN = function(i) {
    data.frame(l = draws[[i]], type = names(draws)[i], mean = mean(draws[[i]]), truth = 1)
  }
))

ggplot(df, aes(x = l, fill = type)) +
  geom_histogram(aes(y = ..density..), alpha = 0.8) +
  geom_vline(aes(xintercept = mean, fill = type)) +
  geom_vline(aes(xintercept = truth), linetype = "dashed") +
  facet_wrap(~ type, ncol = 1) +
  scale_fill_manual(values = cbpalette) +
  labs(y = "Density", x = "Lengthscale", fill = "Prior type")
