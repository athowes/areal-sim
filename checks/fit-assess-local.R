# Testing code locally, by fitting to only a single of the replicates

geometries <- c("grid", "ci", "tex")
sim_models <- c("iid", "icar", "ik")
inf_models <- c("constant", "iid", "icar", "bym", "fck", "fik", "ck", "ik")
fns <- list(constant_inla, iid_inla, besag_inla, bym2_inla, fck_inla, fik_inla, ck_stan, ik_stan)
indices <- list(1, 1, 1, 1, 1, 1, 1, 1) # Only the first replicate

# All combinations of geometry, simulation model and inferential model
pars <- expand.grid(
  "geometry" = geometries, 
  "sim_model" = sim_models, 
  "inf_model" = inf_models
)

k <- length(geometries) * length(sim_models)
pars$fn <- rep(fns, k) # fn matches inf_model, e.g. "constant" and constant_inla() should be together
pars$indices <- rep(indices, k) # indices also matched to inf_model
pars$ctx_ver <- "local-test" # Determines the location in data/ where the results are saved

# Check that pars has the right number of rows and a column for each argument to fit_and_assess
assertthat::assert_that(dim(pars) == c(length(geometries) * length(sim_models) * length(inf_model), length(formalArgs(fit_and_assess)))

# Runs fit_and_assess for each row of pars                        
purrr::pmap_df(pars, fit_and_assess)