require("progress")
require("terra")
require("tidyterra")

files <- list.files(
  "/lud11/poppman/data/bff/dat/lud11/chelsa_variables/swb",
  pattern = ".tif",
  full.names = TRUE
  )

fire <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(files), clear = FALSE, width = 60
)
for (file in files) {
  print(paste("\nResampling", file))
  f_name <- sub("_V2.1", "", sub("CHELSA_", "", basename(file)))
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/swb_resampled_MODIS", f_name
    )
  if (!file.exists(dst)) {
  swb <- terra::rast(file) %>%
    terra::resample(fire, method = "near") %>%
    terra::writeRaster(
        dst,
        overwrite = TRUE
        )
  print(swb)
  rm(swb)
  gc()
  }
  pb$tick()
}