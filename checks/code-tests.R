# Simple testing
# Could be formalised more and sectioned better

source("../00b_local-setup.R")

fit <- bsae::constant_inla(mw) # This model fits a constant
summary(fit)
rho <- rep(0.085, 28) # Not the real values, just made up!

marginal <- ith_marginal_rho(fit, 1)
plot(marginal$samples)

min(marginal$samples) < marginal$mean
max(marginal$samples) > marginal$mean
min(marginal$samples) < marginal$mode
max(marginal$samples) > marginal$mode

# A list of the evaluation metrics
metrics <- assess_ith_marginal_rho(rho, fit, 1)
names(metrics) == c("obs", "mean", "mode", "lower", "upper",
                    "mse", "mae", "mse_mean", "mae_mean", 
                    "mse_mode", "mae_mode", "crps", "lds", "quantile")

# df with columns giving evaluation metrics with first row as above
# The model fitted is a constant so the metrics should all be equal
df <- assess_marginals_rho(rho, fit)

plot(df$mse)
plot(df$mae)
plot(df$lds)
plot(df$crps)
plot(df$quantile)

# Test that assess_replicates works as intended
x <- assess_replicates(list(list(rho = rho)), list(fit))

# This is for the lengthscale test
fit2 <- bsae::ck_stan(mw)
x2 <- assess_replicates(list(list(rho = rho)), list(fit2))

# Does inla.posterior.sample match $fitted.values
samples_list <- INLA::inla.posterior.sample(n = 100000, fit, selection = list(Predictor = 1))
eta_samples = sapply(samples_list, function(x) x$latent)
samples <- plogis(eta_samples)
samples_df <- data.frame(x = samples)

ggplot2::ggplot(marginal$df) +
  geom_histogram(data = samples_df, aes(x = x, y = ..density..), bins = 1000) +
  geom_line(aes(x = x, y = y), col = "red")

f <- approxfun(marginal$df$x, marginal$df$y, yleft = 0, yright = 0)

# Are samples generated outside the range of f (from $fitted.values)
log(f(max(samples))) # Not -Inf
log(f(min(samples)))

# Kernel density approach on samples
kde <- density(samples, bw = 0.1, n = 256, from = 0, to = 1)
df2 <- data.frame(x = kde$x, y = kde$y)
plot(df2$x, df2$y) # What is going on here?
plot(marginal$df$x, marginal$df$y)
f2 <- approxfun(df2$x, df2$y, yleft = 0, yright = 0)
log(f2(max(samples))) # Not -Inf
log(f2(min(samples)))

# inla. function testing

# Should be approximately the same
INLA::inla.dmarginal(0.085, marginal$df, log = TRUE)
log(f(0.085))

# inla.qmarginals gives the value corresponding to a quantile q
# inla.pmarginal gives the cumulative distribution function at p
# Should be approxiamtely the same
INLA::inla.pmarginal(0.085, marginal$df)
cubature::cubintegrate(f, lower = 0, upper = 0.085, method = "pcubature")$integral

# Do these functions work for df created outside of INLA (I don't see why not)
# Yes they do, is it worth switching to them?
fit_stan <- bsae::constant_stan(mw)
marginal_stan <- ith_marginal_rho(fit_stan, 1)
plot(marginal_stan$df$x, marginal_stan$df$y)
plot(df2$x, df2$y)

# Looks the same as that based upon inla.posterior.sample
INLA::inla.dmarginal(0.085, marginal_stan$df, log = TRUE)
INLA::inla.dmarginal(0.085, df2, log = TRUE)
INLA::inla.pmarginal(0.085, marginal_stan$df)
INLA::inla.pmarginal(0.085, df2)
