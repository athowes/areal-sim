# This tests that submission to the DIDE cluster is working. Results will be in data/results/dide-test
test_02 <- obj$enqueue(
  fit_and_assess(geometry = "grid", sim_model = "iid", inf_model = "constant_inla", indices = 1:5, ctx_ver = "dide-test")
)

# Quicker INLA models, don't need to be parallel over datasets here
geometries <- c("grid", "ci", "tex")
sim_models <- c("iid", "icar", "ik")
inf_models <- c("constant_inla", "iid_inla", "besag_inla", "bym2_inla", "fck_inla", "fik_inla")
index <- list(NA, NA, NA, NA, NA, NA) # Possibly could omit this, since NA is the default

pars <- rlist::list.expand(
  geometry = geometries, 
  sim_model = sim_models,
  inf_model = inf_models,
  indices = NA,
  ctx_ver = NA
)

# Don't expand these, just associate with a particular inf_model
k <- length(geometries) * length(sim_models)
for(i in seq_along(pars)){
  pars[[i]]$indices <- rep(index, each = k)[[i]]
  pars[[i]]$ctx_ver <- ctx_ver
}

# Assignment so that the objects have the context name, for reference
assign(paste0("pars_02_inla_", ctx_ver), pars)
assign(paste0("grp_02_inla_", ctx_ver), obj$enqueue_bulk(pars, fit_and_assess, do_call = TRUE))

# BYM2 is slow, so create more jobs (8 each) here
geometries <- c("grid", "ci", "tex")
sim_models <- c("iid", "icar", "ik")
inf_models <- c("bym2_inla")
index <- list(1:25, 26:50, 51:75, 76:100, 101:125, 126:150, 151:175, 176:200)

pars <- rlist::list.expand(
  geometry = geometries, 
  sim_model = sim_models,
  inf_model = inf_models,
  indices = index,
  ctx_ver = NA
)

for(i in seq_along(pars)) pars[[i]]$ctx_ver <- ctx_ver # Probably a less obtuse way to do this

assign(paste0("pars_02_bym2_", ctx_ver), pars)
assign(paste0("grp_02_bym2_", ctx_ver), obj$enqueue_bulk(pars, fit_and_assess, do_call = TRUE))

# Stan is even slower, also run 8 jobs each here
geometries <- c("grid", "ci", "tex")
sim_models <- c("iid", "icar", "ik")
inf_models <- c("ck_stan", "ik_stan")
index <- list(1:25, 26:50, 51:75, 76:100, 101:125, 126:150, 151:175, 176:200)

pars <- rlist::list.expand(
  geometry = geometries, 
  sim_model = sim_models,
  inf_model = inf_models,
  indices = index,
  ctx_ver = NA
)

for(i in seq_along(pars)) pars[[i]]$ctx_ver <- ctx_ver

assign(paste0("pars_02_stan_", ctx_ver), pars)
assign(paste0("grp_02_stan_", ctx_ver), obj$enqueue_bulk(pars, fit_and_assess, do_call = TRUE))
