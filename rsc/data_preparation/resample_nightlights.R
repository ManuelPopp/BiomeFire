require("terra")
require("tidyterra")

years <- 2012:2018

src_dir <- "/lud11/poppman/data/bff/dat/nightlight_raw"
dst_dir <- "/lud11/poppman/data/bff/dat/lud11/annual/nightlights_resampled_MODIS"

template <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

# Writing annual files
terra::values(template) <- NA

for (year in years) {
  files <- list.files(
    src_dir, pattern = paste0(year, ".tif"), full.names = TRUE
  )
  if (length(files) == 8) {
    do.call(terra::merge, lapply(files, FUN = terra::rast)) %>%
      terra::resample(template, method = "near") %>%
      terra::writeRaster(
        filename = file.path(
          dst_dir,
          paste0("nightlights_", year, ".tif")
        ),
        overwrite = TRUE
      )
  } else {
    cat("Incomplete data for year", year, "\n")
#    cat("Writing empty raster.")
#    terra::writeRaster(
#      template,
#      filename = file.path(
#        "/lud11/poppman/data/bff/dat/lud11/annual/nightlights_resampled_MODIS",
#        paste0("nightlights_", year, ".tif")
#        ),
#      overwrite = TRUE
#    )
  }
}

# Writing static file
files <- list.files(
  dst_dir, pattern = ".tif", full.names = TRUE
)

terra::rast(files) %>%
  terra::app(fun = mean) %>%
  terra::writeRaster(
    filename = file.path(
      sub(
        "annual", "static", sub("_resampled_MODIS", "", dst_dir)
        ), "nightlights_MODIS.tif"
    ),
    overwrite = TRUE
  )