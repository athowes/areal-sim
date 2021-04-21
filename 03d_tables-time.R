full_df_time <- do.call("rbind", lapply(files, FUN = function(file) {
  assess <- readRDS(file = paste0("data/results/", ctx_ver, "/", file))
  df <- assess$time_df
  meta_data <- strsplit(file, '[/_.]')
  df$geometry <- meta_data[[1]][1]
  df$sim_model <- meta_data[[1]][2]
  df$inf_model <- meta_data[[1]][3]
  return(df)
}))  %>%
  rename_df()

df_time <- full_df_time %>%
  group_by(geometry, sim_model, inf_model) %>%
  summarise(n = n(), across(t, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))

# Supplementary tables
metric_table(df_time, "t", latex = TRUE)
