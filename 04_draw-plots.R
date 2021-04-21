theme_adam <- theme(
  panel.grid.major = element_line(colour = "grey90", size = 0.2),
  panel.grid.minor = element_line(colour = "grey98", size = 0.5),
  panel.background = element_rect()
)

theme_adam_minimal <- theme_adam + theme(
  axis.text = element_text(size = rel(0.8)), 
  axis.ticks = element_line(colour = "black"), 
  panel.background = element_rect(fill = "white", colour = NA)
)

# fig21 -------------------------------------------------------------------

sf <- filter(zw, name_1 == "Matabeleland North")

text <- c("$A_2$", "$ A_7$", "$A_1$", "$ A_3$", "$A_4$", "$A_5$", "$A_6$")
labels <- st_sf(text, geometry = sf::st_centroid(sf$geometry))
hwange_indicator <- c(0, 0, 1, 0, 0, 0, 0)

a <- ggplot() +
  geom_sf(data = sf, aes(fill = as.factor(hwange_indicator)), alpha = 0.8) +
  geom_sf_text(data = labels, aes(label = text)) +
  theme_minimal() +
  labs(x = "", y = "", title = "$\\mathcal{S}$", fill = "") +
  scale_fill_manual(values = c(sf_lightgrey, lightgrey)) +
  theme_void() +
  theme(legend.position = "none")

hwange <- filter(zw, name_2 == "Hwange")
points <- st_sample(hwange, size = 5)

v <- points %>% 
  st_union() %>%
  st_voronoi()

dat <- st_intersection(st_cast(v), st_union(hwange))

b <- ggplot(dat) +
  geom_sf(fill = lightgrey, alpha = 0.8) +
  stat_sf_coordinates(shape = 4, fill = "white") +
  labs(x = "", y = "", title = "$A_1$") + 
  theme_void()

tikz(file = "plots/fig21.tex", width = 4.5, height = 2.5)
cowplot::plot_grid(a, b, ncol = 2)
dev.off()

system("cd plots && lualatex compile_fig21.tex")

# fig22 -------------------------------------------------------------------

nb <- neighbours(zw) # Spatial neighbours object

nb_sf <- spdep::nb2lines(nb, coords = sp::coordinates(as(zw, "Spatial"))) %>%
  as("sf") %>%
  st_set_crs(st_crs(zw))

a <- ggplot(zw) +
  geom_sf() +
  theme_minimal() +
  labs(subtitle = "Geography") + 
  theme_void()

b <- ggplot(zw) +
  geom_sf(data = nb_sf) +
  theme_minimal() +
  labs(subtitle = "Graph") + 
  theme_void()

tikz(file = "plots/fig22.tex", width = 4.5, height = 2.5)
cowplot::plot_grid(a, b, ncol = 2)
dev.off()

system("cd plots && lualatex compile_fig22.tex")

# fig41 -------------------------------------------------------------------

plot_samples <- function(samples, title){
  ggplot(mw) +
    geom_sf(fill = "lightgrey") +
    geom_sf(data = samples, alpha = 0.5, shape = 4, col = midblue) +
    labs(x = "", y = "") +
    theme_minimal() +
    labs(subtitle = title, fill = "") +
    theme_void()
}

L <- 10
n <- nrow(mw)

random <- sf::st_sample(mw, size = rep(L, n))
hexagonal <- sf::st_sample(mw, size = rep(L, n), type = "hexagonal", exact = TRUE)
regular <- sf::st_sample(mw, size = rep(L, n), type = "regular", exact = TRUE)

tikz(file = "plots/fig41.tex", width = 6.5, height = 2.5)
cowplot::plot_grid(
  plot_samples(random, title = "Random"),
  plot_samples(hexagonal, title = "Hexagonal"),
  plot_samples(regular, title = "Regular"),
  ncol = 3
)
dev.off()

system("cd plots && lualatex compile_fig41.tex")

# fig51 -------------------------------------------------------------------

load("data/inputs/geometries.RData")

tikz(file = "plots/fig51.tex", width = 6.5, height = 2.5)
cowplot::plot_grid(
  area_plot(grid, "$6 \\times 6$ grid"),
  area_plot(ci, "33 districts, C\\^{o}te d'Ivoire"),
  area_plot(tex, "36 congressional districts, Texas"),
  ncol = 3,
  align = "h"
)
dev.off()

system("cd plots && lualatex compile_fig51.tex")

# fig52 -------------------------------------------------------------------

tikz(file = "plots/fig52.tex", width = 6.5, height = 4)
boxplot(df_replicate_rho, metric = "crps_mean", y_lab = "Average CRPS", remove_constant = TRUE) + theme_adam
dev.off()

system("cd plots && lualatex compile_fig52.tex")

# fig53 -------------------------------------------------------------------

tikz(file = "plots/fig53.tex", width = 6.5, height = 4)
boxplot(df_replicate_intercept, metric = "crps_mean", y_lab = "Average CRPS", remove_constant = TRUE) + theme_adam
dev.off()

system("cd plots && lualatex compile_fig53.tex")

# fig54 -------------------------------------------------------------------

tikz(file = "plots/fig54.tex", width = 6.5, height = 2.5)
cowplot::plot_grid(
  lengthscale_plot(full_df_lengthscale, inf_model = "ik", geometry = "grid", best = l_grid, subtitle = "Grid") + theme_adam,
  lengthscale_plot(full_df_lengthscale, inf_model = "ik", geometry = "ci", best = l_ci, subtitle = "C\\^{o}te d'Ivoire") + theme_adam,
  lengthscale_plot(full_df_lengthscale, inf_model = "ik", geometry = "tex", best = l_tex, subtitle = "Texas") + theme_adam,
  ncol = 3
)
dev.off()

system("cd plots && lualatex compile_fig54.tex")

# fig81 -------------------------------------------------------------------

plot_lengthscale <- function(df, l, col) {
  
  .k <- function(x, y, l) {
    2 * matern(abs(x - y), l)  
  }
  
  x <- seq(0.01, 5, by = 0.01)
  n <- 2 # Number of breaks
  
  y <- MASS::mvrnorm(1, rep(0, length(x)), outer(x, x, .k, l = l))
  df <- cbind(x, y, group = rep(1:n, each = length(x) / n)) %>% 
    as.data.frame()
  y_agg <- aggregate(df, list(df$group), FUN = mean)$y
  df$y_agg <- rep(y_agg, each = length(x) / n)
  
  ggplot(df) +
    geom_line(aes(x = x, y = y), col = col) +
    geom_line(aes(x = x, y = y_agg), linetype = "dashed") +
    labs(x = NULL, y = NULL) +
    theme_adam +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

tikz(file = "plots/fig81.tex", width = 4.5, height = 3)
cowplot::plot_grid(
  plot_lengthscale(df, l = 0.05, col = lightblue) + labs(subtitle = "$l = 0.05$"),
  plot_lengthscale(df, l = 0.5, col = lightgreen) + labs(subtitle = "$l = 0.5$"),
  plot_lengthscale(df, l = 5, col = "#E69F00") + labs(subtitle = "$l = 5$"),
  ncol = 1
)
dev.off()

system("cd plots && lualatex compile_fig81.tex")

# figA1 -------------------------------------------------------------------

tikz(file = "plots/figA1.tex", width = 6.25, height = 8.5)
scoropleth(df_id_rho, "crps", "Grid", sf = grid, remove_constant = TRUE) + theme_adam_minimal
dev.off()
system("cd plots && lualatex compile_figA1.tex")

# figA2 -------------------------------------------------------------------

tikz(file = "plots/figA2.tex", width = 6.25, height = 8.5)
scoropleth(df_id_rho, "crps", "Cote d'Ivoire", sf = ci, remove_constant = TRUE) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figA2.tex")

# figA3 -------------------------------------------------------------------

tikz(file = "plots/figA3.tex", width = 6.25, height = 8.5)
scoropleth(df_id_rho, "crps", "Texas", sf = tex, remove_constant = TRUE) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figA3.tex")

# figA4 -------------------------------------------------------------------

tikz(file = "plots/figA4.tex", width = 6.25, height = 8.5)
coverage_histogram(full_df_rho, "Grid") + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figA4.tex")

# figA5 -------------------------------------------------------------------

tikz(file = "plots/figA5.tex", width = 6.25, height = 8.5)
coverage_histogram(full_df_rho, "Cote d'Ivoire") + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figA5.tex")

# figA6 -------------------------------------------------------------------

tikz(file = "plots/figA6.tex", width = 6.25, height = 8.5)
coverage_histogram(full_df_rho, "Texas") + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figA6.tex")

# Experimental ------------------------------------------------------------

# Priors

dlogitnorm <- function(x, mean = 0, sd = 1) {
  ifelse(0 < x & x < 1,
         exp(-0.5 * (log(x / (1 - x)) - mean)^2 / sd^2) * 
           1/(x * (1 - x)) * 1/(sd*sqrt(2*pi)),
         0)
}

ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dlogitnorm, n = 101, args = list(-2, 1), col = midpink) +
  labs(x = "rho", y = "p(rho)", title = "N(-2, 1^2)")

# Q matrices

cowplot::plot_grid(
  Q_plots(grid),
  Q_plots(ci),
  Q_plots(tex),
  ncol = 1,
  nrow = 3,
  align = "h"
)
