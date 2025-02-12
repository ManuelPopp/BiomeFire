require("terra")
require("tidyterra")

folder_raw <- "/lud11/poppman/data/bff/dat/terrain_raw"
files <- list.files(folder_raw, pattern = ".tif", full.names = TRUE)

template <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

for (f in files) {
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/statis/terrain",
    paste0(tools::file_path_sans_ext(basename(f)), "_MODIS.tif")
  )
  terra::rast(f) %>%
    terra::project(template, method = "near") %>%
    terra::resample(template, method = "near") %>%
    terra::writeRaster(
        filename = dst,
        overwrite = TRUE
    )
}