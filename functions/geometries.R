# Create height by width grid sf object
create_sf_grid <- function(height, width){
  sfc <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(width, 0), c(width, height), c(0, 0)))))
  grid <- sf::st_make_grid(sfc, cellsize = 1, square = TRUE)
  return(grid)
}

# Extract geometry from sf object
extract_geometry <- function(sf){
  sf$geometry 
}