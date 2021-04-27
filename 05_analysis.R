# For the IK simulated data, what is the difference between Besag, FCK and FIK over the different geographies?
irregularity_effect <- df_rho %>%
  filter(sim_model == "IK", inf_model %in% c("Besag", "FCK", "FIK")) %>%
  group_by(geometry) %>%
  mutate(crps = 1000 * crps_mean,
         crps_min = min(crps),
         diff = crps - crps_min) %>%
  select(inf_model, diff)

irregularity_effect %>% filter(inf_model == "FCK") %>% mutate_if(is.numeric, round, 1)  # FCK versus best (FIK)
irregularity_effect %>% filter(inf_model == "Besag") %>% mutate_if(is.numeric, round, 1) # Besag versus best (FIK)

# How is the length-scale recovery?
load("data/inputs/geometries.RData")

l_grid <- bsae::best_average(centroid_distance(grid))
l_ci <- bsae::best_average(centroid_distance(ci))
l_tex <- bsae::best_average(centroid_distance(tex))

lengthscale_plot(full_df_lengthscale, inf_model = "ck", geometry = "grid", best = l_grid)
lengthscale_plot(full_df_lengthscale, inf_model = "ck", geometry = "ci", best = l_ci)
lengthscale_plot(full_df_lengthscale, inf_model = "ck", geometry = "tex", best = l_tex)

lengthscale_plot(full_df_lengthscale, inf_model = "ik", geometry = "grid", best = l_grid)
lengthscale_plot(full_df_lengthscale, inf_model = "ik", geometry = "ci", best = l_ci)
lengthscale_plot(full_df_lengthscale, inf_model = "ik", geometry = "tex", best = l_tex)

# Average of the mean lengthscale 
full_df_lengthscale %>%
  filter(sim_model == "ik") %>%
  group_by(geometry, inf_model) %>%
  summarise(overall_mean = mean(mean)) %>%
  arrange(inf_model)

# Lengthscale coverage
full_df_lengthscale %>%
  filter(sim_model == "ik") %>%
  mutate(in50 = between(quantile, 0.25, 0.75),
         in80 = between(quantile, 0.1, 0.9),
         in95 = between(quantile, 0.025, 0.975)) %>%
  group_by(geometry, sim_model, inf_model) %>%
  summarise("50%" = sum(in50) / n(),
            "80%" = sum(in80) / n(),
            "95%" = sum(in95) / n()) %>%
  tidyr::gather(variable, value, -(geometry:inf_model), factor_key = TRUE) %>%
  tidyr::unite(temp, inf_model, variable) %>%
  tidyr::spread(temp, value)
