full_df_lengthscale <- do.call("rbind", lapply(files, FUN = function(file) {
  assess <- readRDS(file = paste0("data/results/", ctx_ver, "/", file))
  
  if(purrr::is_empty(assess$lengthscale_df)){
    return(NULL)
  }

  df <- assess$lengthscale_df
  meta_data <- strsplit(file, '[/_.]')
  df$geometry <- meta_data[[1]][1]
  df$sim_model <- meta_data[[1]][2]
  df$inf_model <- meta_data[[1]][3]
  return(df)
}))