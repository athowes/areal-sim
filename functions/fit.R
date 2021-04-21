#' Function to fit single model.
#'
#' @param sf A simple features data frame.
#' @param fn A fitting function.
#' @return A fitted model.
fit_model <- function(sf, fn, ...) {
  fn(sf, ...)  
}

#' Fit an inferential model upon a list of data-set replicates and calculate model assessment criteria.
#' 
#' @param geometry A geometry name as string e.g. `"grid"`.
#' @param sim_model A simulation model name as string e.g. `"iid"`.
#' @param inf_model An inferential model name as string e.g. `"iid"`.
#' @param fn An inferential fitting function, e.g. `iid_inla`.
#' @param indices The indices of the data-sets to be fit, defaults to all.
#' @param ctx_ver The version of the context.
fit_and_assess <- function(geometry, sim_model, inf_model, fn, indices = NA, ctx_ver) {
  
  # Read the simulation data, reducing size if required
  list <- readRDS(paste0("data/inputs/", geometry, "/", sim_model, "_list.rds"))
  if(!is.na(indices)){
    list <- list[indices]
  }
  
  # Fit the inferential model fn
  all_fits <- lapply(list, function(item) fit_model(item$sf, fn))
  
  # Model evaluation
  assessment <- assess_replicates(list, all_fits)

  safe_saveRDS(
    object = assessment, 
    output_dir = paste0("data/results/", ctx_ver, "/", geometry, "/"), 
    file = paste0(sim_model, "_", inf_model, "_", min(indices), "-", max(indices))
  )
}

#' Alternate likely superior work-flow for future
#' 
alt_fit_and_assess <- function(list, fn) {
  all_fits <- lapply(list, function(item) fit_model(item$sf, fn))
  assess_df <- assess_replicates(list, all_fits)
  return(assess_df)
}
  