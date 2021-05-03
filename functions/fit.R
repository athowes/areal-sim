#' Fit an inferential model upon a list of data-set replicates and calculate model assessment criteria.
#' 
#' @param geometry A geometry name as string e.g. `"grid"`.
#' @param sim_model A simulation model name as string e.g. `"iid"`.
#' @param inf_model An inferential model name as string e.g. `"iid_inla"`.
#' @param indices The indices of the data-sets to be fit, defaults to all.
#' @param ctx_ver The version of the context.
fit_and_assess <- function(geometry, sim_model, inf_model, indices = NA, ctx_ver) {
  
  # Read the simulation data, reducing size if required
  list <- readRDS(paste0("data/inputs/", geometry, "/", sim_model, "_list.rds"))
  if(!is.na(indices)){
    list <- list[indices]
  }
  
  # Fit the inferential model fn
  fn <- get(inf_model, mode = "function")
  all_fits <- lapply(list, function(item) fn(item$sf))
  
  # Model evaluation
  assessment <- assess_replicates(list, all_fits)

  safe_saveRDS(
    object = assessment, 
    output_dir = paste0("data/results/", ctx_ver, "/", geometry, "/"), 
    file = paste0(sim_model, "_", strsplit(inf_model, "_")[[1]][1], "_", min(indices), "-", max(indices))
  )
}
