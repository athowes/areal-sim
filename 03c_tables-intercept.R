full_df_intercept <- do.call("rbind", lapply(files, FUN = function(file) {
  assess <- readRDS(file = paste0("data/results/", ctx_ver, "/", file))
  df <- assess$intercept_df
  meta_data <- strsplit(file, '[/_.]')
  df$geometry <- meta_data[[1]][1]
  df$sim_model <- meta_data[[1]][2]
  df$inf_model <- meta_data[[1]][3]
  return(df)
}))  %>%
  rename_df()

df_intercept <- full_df_intercept %>%
  select(-c(obs, mean, mode)) %>%
  group_by(geometry, sim_model, inf_model) %>%
  summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))

# Average score by replicate
df_replicate_intercept <- full_df_intercept %>%
  select(-c(obs, mean, mode)) %>%
  group_by(geometry, sim_model, inf_model, replicate) %>%
  summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))

# Supplementary tables
metric_table(df_intercept, "crps", title = "Continuous Ranked Probability Score", latex = TRUE, scale = 1000)
