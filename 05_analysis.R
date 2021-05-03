# For the IK simulated data, what is the difference between Besag, FCK and FIK over the different geographies?
irregularity_effect <- df_rho %>%
  filter(sim_model == "IK", inf_model %in% c("Besag", "FCK", "FIK")) %>%
  group_by(geometry) %>%
  mutate(crps = 1000 * crps_mean,
         crps_min = min(crps),
         diff = crps - crps_min) %>%
  select(inf_model, diff)

# FCK versus best (FIK)
irregularity_effect %>% filter(inf_model == "FCK") %>% mutate_if(is.numeric, round, 1)

# Besag versus best (FIK)
irregularity_effect %>% filter(inf_model == "Besag") %>% mutate_if(is.numeric, round, 1)

# How is the length-scale recovery?
load("data/inputs/geometries.RData")

# These are the values of the length-scale recovered by the best_average() function, without any data
l_grid <- bsae::best_average(centroid_distance(grid))
l_ci <- bsae::best_average(centroid_distance(ci))
l_tex <- bsae::best_average(centroid_distance(tex))

# Truth in blue, mean of posterior means in green and fixed in yellow
lengthscale_plot(full_df_lengthscale, inf_model = "CK", geometry = "Grid", best = l_grid)
lengthscale_plot(full_df_lengthscale, inf_model = "CK", geometry = "Cote d'Ivoire", best = l_ci)
lengthscale_plot(full_df_lengthscale, inf_model = "CK", geometry = "Texas", best = l_tex)

lengthscale_plot(full_df_lengthscale, inf_model = "IK", geometry = "Grid", best = l_grid)
lengthscale_plot(full_df_lengthscale, inf_model = "IK", geometry = "Cote d'Ivoire", best = l_ci)
lengthscale_plot(full_df_lengthscale, inf_model = "IK", geometry = "Texas", best = l_tex)

# Average of the mean lengthscale 
full_df_lengthscale %>%
  filter(sim_model == "IK") %>%
  group_by(geometry, inf_model) %>%
  summarise(overall_mean = mean(mean)) %>%
  arrange(inf_model)

# What's the coverage of the length-scale posteriors?
full_df_lengthscale %>%
  filter(sim_model == "IK") %>%
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
