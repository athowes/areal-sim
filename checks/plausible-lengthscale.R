# Checking the average distances between centroids in order to chose a plausible lengthscale to recover

load("../data/inputs/geometries.RData")

grid_distances <- c(centroid_distance(grid))
hist(grid_distances)
min(grid_distances[grid_distances > 0])
abline(v = 1, col = "red", lty = "dashed")

ci_distances <- c(centroid_distance(ci))
hist(ci_distances)
min(ci_distances[ci_distances > 0])
abline(v = 1, col = "red", lty = "dashed")

tex_distances <- c(centroid_distance(tex))
hist(tex_distances)
min(tex_distances[tex_distances > 0])
abline(v = 1, col = "red", lty = "dashed")

graph_length_plot <- function(g) {
  nb <- neighbours(g)
  
  nb_sf <- spdep::nb2lines(nb, coords = sp::coordinates(as(g, "Spatial"))) %>%
    as("sf") %>%
    st_set_crs(st_crs(g))
  
  line_sf <- nb_sf %>%
    mutate(length = round(sf::st_length(nb_sf), 2),
           geometry = sf::st_centroid(geometry))
  
  ggplot(g) +
    geom_sf() +
    geom_sf(data = nb_sf) +
    geom_sf_label(data = line_sf, aes(label = length)) +
    theme_minimal() +
    theme_void()
}

cowplot::plot_grid(
  graph_length_plot(grid),
  graph_length_plot(ci),
  graph_length_plot(tex),
  ncol = 3
)
