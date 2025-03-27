if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
  sub_clim <- "chelsa_kg"
} else {
  dir_main <- "/lud11/poppman/data/bff"
  sub_clim <- file.path("lud11", "chelsa_kg")
}

dir_dat <- file.path(dir_main, "dat")
dir_lud <- file.path(dir_dat, "lud11")

biome_id <- 12
biome <- paste0("Olson_biome_", biome_id)
biome_num <- biome_id

#>----------------------------------------------------------------------------<|
#> Load data
f_data_chunks <- list.files(
  file.path(dir_lud, "intermediate_data", biome), pattern = "annual_predictors",
  full.names = TRUE
)

name_vect <- c(
  "ID", "fire", "pr", "spi06", "spi12", "spimin", "spei06", "spei12",
  "speimin", "swb", "tasmin", "tasmean", "tasmax", "vpdmean", "vpdmax",
  "npp_before", "npp_biome4_before", "npp_biome4", "ndvi_before",
  "osavi_before", "Lightning", "LightningEquinox", "Lightning_clim",
  "gHM", "nightlights", "aspectcosine_1KMmn_GMTEDmd", "slope_1KMmn_GMTEDmd",
  "tpi_1KMmn_GMTEDmd", "canopyheight", "cmi_clim", "pr_clim", "swb_clim",
  "tasmax_clim", "tasmean_clim", "vpd_clim", "x", "y", "year"
  )

chunks <- list()
i <- 1
corrupt <- c()
for (f_chunk in f_data_chunks) {
  load(f_chunk)
  print(length(names(data)))
  if (!all(name_vect %in% names(data))) {
    corrupt <- c(corrupt, basename(f_chunk))
  }
  rm(data)
  gc()
  i <- i + 1
}

print(corrupt)