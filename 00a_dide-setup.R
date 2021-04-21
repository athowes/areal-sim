options(didehpc.username = "ath19")

didehpc::web_login()

my_config <- didehpc::didehpc_config(
  credentials = "ath19",
  cores = 2, 
  parallel = FALSE, 
  cluster = "fi--didemrchnb"
  # "fi--dideclusthn"
  # "fi--didemrchnb"
)

pkgsrc <- conan::conan_sources(
  c("./bsae_0.2.5.tar.gz", "./INLA_20.07.12.tar.gz")
)

src <- c(
  list.files("functions/", full.names = TRUE)
)

ctx_ver <- "19-04-2021"

ctx <- context::context_save(
  ctx_ver, 
  packages = list(loaded = c("tidyr", 
                             "dplyr", 
                             "cubature", 
                             "lwgeom",
                             "sn",
                             "nleqslv",
                             "invgamma"), 
                  attached = c("bsae", 
                               "INLA")), 
  package_sources = pkgsrc, 
  sources = src
)

obj <- didehpc::queue_didehpc(ctx, config = my_config)
