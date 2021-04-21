ctx_ver <- "19-04-2021"

files <- list.files(
  path = paste0("data/results/", ctx_ver, "/"),
  recursive = TRUE
)

full_df_rho <- do.call("rbind", lapply(files, FUN = function(file) {
  assess <- readRDS(file = paste0("data/results/", ctx_ver, "/", file))
  df <- assess$rho_df
  meta_data <- strsplit(file, '[/_.]')
  df$geometry <- meta_data[[1]][1]
  df$sim_model <- meta_data[[1]][2]
  df$inf_model <- meta_data[[1]][3]
  return(df)
})) %>%
  rename_df()

# Average score
df_rho <- full_df_rho %>%
  select(-c(obs, mean, mode)) %>%
  group_by(geometry, sim_model, inf_model) %>%
  summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))

# Average score by area
df_id_rho <- full_df_rho %>%
  select(-c(obs, mean, mode)) %>%
  group_by(geometry, sim_model, inf_model, id) %>%
  summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))

# Average score by replicate
df_replicate_rho <- full_df_rho %>%
  select(-c(obs, mean, mode)) %>%
  group_by(geometry, sim_model, inf_model, replicate) %>%
  summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))

# Supplementary tables
metric_table(df_rho, "crps", title = "Continuous Ranked Probability Score", latex = FALSE, scale = 10^3, figures = 3)
coverage_table(full_df_rho, latex = TRUE)
