# Testing code locally fitting to only a single of the replicates

geometries <- c("grid", "ci", "tex")
sim_models <- c("iid", "icar", "ik")
inf_models <- c("constant", "iid", "icar", "bym", "fck", "fik", "ck", "ik")
fns <- list(constant_inla, iid_inla, besag_inla, bym2_inla, fck_inla, fik_inla, ck_stan, ik_stan)
indices <- list(1, 1, 1, 1, 1, 1, 1, 1) # Only the first replicate

pars <- expand.grid(
  "geometry" = geometries, 
  "sim_model" = sim_models, 
  "inf_model" = inf_models
)

k <- length(geometries) * length(sim_models)
pars$fn <- rep(fns, k)
pars$data_frac <- rep(data_fracs, k)
pars$ctx_ver <- "local-test"

purrr::pmap_df(pars, fit_and_assess)
