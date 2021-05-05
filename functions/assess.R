assess_replicates <- function(data_list, fits_list) {
  rho_list <- Map(assess_marginals_rho, lapply(data_list, "[[", "rho"), fits_list)
  rho_df <- list_to_df(rho_list)

  lengthscale_list <- lapply(fits_list, assess_marginal_lengthscale, lengthscale = 1)
  if(!is.null(lengthscale_list)) {
    lengthscale_df <- list_to_df(lengthscale_list)
  }
  else {
   # When lengthscale in model, but not in the DGP, then forecast assessment is without meaning
   lengthscale_df <- NULL 
  }
  
  intercept_list <- lapply(fits_list, assess_marginal_intercept, intercept = -2)
  intercept_df <- list_to_df(intercept_list)
  
  time_list <- lapply(fits_list, function(fit) list(t = get_time(fit)))
  time_df <- list_to_df(time_list)
  
  return(list(
    rho_df = rho_df, 
    lengthscale_df = lengthscale_df, 
    intercept_df = intercept_df,
    time_df = time_df
  ))
}