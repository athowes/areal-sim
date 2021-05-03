# The number of data-sets of each type to generate
nsim <- 200

# Create geometries
geometry <- create_sf_grid(height = 6, width = 6)
grid <- sf::st_sf(geometry)

geometry <- sf::st_geometry(ci)
ci <- sf::st_sf(geometry)
sf::st_crs(ci) <- NA

# Texas data from https://gis-txdot.opendata.arcgis.com/datasets/texas-us-house-districts
# Download > Shapefile > Unzip to texas/ folder
geometry <- sf::st_geometry(sf::st_read("texas/U_S__House_District.shp"))
tex <- sf::st_sf(geometry)
sf::st_crs(tex) <- NA

# Save geometries
save(ci, grid, tex, file = "data/inputs/geometries.RData")

# Create and save simulated data-sets
sim_iid(grid, nsim)
sim_icar(grid, nsim)
sim_ik(grid, L = 100, nsim, l = 2.5)

sim_iid(ci, nsim)
sim_icar(ci, nsim)
sim_ik(ci, L = 100, nsim, l = 2.5)

sim_iid(tex, nsim)
sim_icar(tex, nsim)
sim_ik(tex, L = 100, nsim, l = 2.5)
