require("progress")
require("terra")
require("tidyterra")

files <- list.files(
  "/lud11/poppman/data/bff/dat/annual_fire_maps", pattern = ".tif",
  full.names = TRUE
  )

pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(files), clear = FALSE, width = 60
)
for (file in files) {
    print(paste("\nResampling", file))
  f_name <- basename(file)
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS", f_name
    )
  if (!file.exists(dst)) {
    fann <- terra::rast(file) %>%
      terra::crop(terra::ext(-180, 180, -90, 90)) %>%
      terra::writeRaster(
          dst,
          overwrite = TRUE
          )
    print(fann)
    rm(fann)
    gc()
  }
  pb$tick()
}