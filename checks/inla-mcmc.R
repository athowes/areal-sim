# Checking that R-INLA and Stan produce similar results

# Example data, should repeat this for others
data <- readRDS("../data/inputs/grid/iid_list.rds")[[1]]

# Constant
fit_inla <- constant_inla(data$sf)
fit_stan <- constant_stan(data$sf)

plot(marginal_intervals(fit_inla)$mean, marginal_intervals(fit_stan, parameter = "rho")$mean)
get_time(fit_inla); get_time(fit_stan)

# IID
fit_inla <- iid_inla(data$sf)
fit_stan <- iid_stan(data$sf)

plot(marginal_intervals(fit_inla)$mean, marginal_intervals(fit_stan, parameter = "rho")$mean)
get_time(fit_inla); get_time(fit_stan)

# Besag
fit_inla <- besag_inla(data$sf)
fit_stan <- besag_stan(data$sf)

plot(marginal_intervals(fit_inla)$mean, marginal_intervals(fit_stan, parameter = "rho")$mean)
get_time(fit_inla); get_time(fit_stan)

# BYM2
fit_inla <- bym2_inla(data$sf)
fit_stan <- bym2_stan(data$sf)

plot(marginal_intervals(fit_inla)$mean, marginal_intervals(fit_stan, parameter = "rho")$mean)
get_time(fit_inla); get_time(fit_stan)