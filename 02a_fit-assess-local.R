fit_and_assess(
  geometry = "grid",
  sim_model = "iid",
  inf_model = "constant",
  fn = constant_inla,
  indices = 1,
  ctx_ver = "local-test"
)

# Testing code locally fitting to only a single of the replicates

geometries <- c("grid", "ci", "tex")
sim_models <- c("iid", "icar", "ik")
inf_models <- c("constant", "iid", "icar", "bym", "fck", "fik", "ck", "ik")
fns <- list(constant_inla, iid_inla, besag_inla, bym2_inla, fck_inla, fik_inla, ck_stan, ik_stan)
indices <- list(1, 1, 1, 1, 1, 1, 1, 1) # Only the first replicate

pars <- expand.grid(geometries, sim_models, inf_models)
names(pars) <- c("geometry", "sim_model", "inf_model")

k <- length(geometries) * length(sim_models)
pars$fn <- rep(fns, k)
pars$data_frac <- rep(data_fracs, k)
pars$ctx_ver <- "local-test"

purrr::pmap_df(pars, fit_and_assess)
