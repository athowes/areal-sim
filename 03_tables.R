ctx_ver <- "19-04-2021"

# All of the results corresponding to the above context version
files <- list.files(
  path = paste0("data/results/", ctx_ver, "/"),
  recursive = TRUE
)

# rho parameter -----------------------------------------------------------

full_df_rho <- extract_results("rho_df")

# Average score
df_rho <- group_mean_and_se(
  full_df_rho, 
  c("geometry", "sim_model", "inf_model")
)

# ... by area
df_id_rho <- group_mean_and_se(
  full_df_rho, 
  c("geometry", "sim_model", "inf_model", "id")
)

# ... by replicate
df_replicate_rho <- group_mean_and_se(
  full_df_rho, 
  c("geometry", "sim_model", "inf_model", "replicate")
)

# Supplementary tables
metric_table(df_rho, "crps", title = "Continuous Ranked Probability Score", latex = TRUE, scale = 10^3, figures = 3)
coverage_table(full_df_rho, latex = TRUE)

# intercept parameter -----------------------------------------------------

full_df_intercept <- extract_results("intercept_df")

# Average score
df_intercept <- group_mean_and_se(
  full_df_intercept, 
  c("geometry", "sim_model", "inf_model")
)

# ...by replicate
df_replicate_intercept <- group_mean_and_se(
  full_df_intercept, 
  c("geometry", "sim_model", "inf_model", "replicate")
)

# Supplementary tables
metric_table(df_intercept, "crps", title = "Continuous Ranked Probability Score", latex = TRUE, scale = 10^3)

# lengthscale parameter ---------------------------------------------------

full_df_lengthscale <- extract_results("lengthscale_df")

# time taken --------------------------------------------------------------

full_df_time <- extract_results("time_df")

df_time <- full_df_time %>%
  group_by(geometry, sim_model, inf_model) %>%
  summarise(n = n(), across(t, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x)))))

# Supplementary tables
metric_table(df_time, "t", latex = TRUE)
