require("terra")
require("tidyterra")

files <- list.files(
  "/lud11/poppman/data/bff/dat/canopyheight",
  pattern = ".tif", full.names = TRUE
)

template <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

do.call(terra::merge, lapply(files, FUN = terra::rast)) %>%
  terra::resample(template, method = "near") %>%
  terra::writeRaster(
    file.path(
      "/lud11/poppman/data/bff/dat/lud11/static/canopyheight",
      "canopyheight_MODIS.tif"
    ),
    overwrite = TRUE
  )
print("Done.")