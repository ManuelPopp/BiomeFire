require("progress")
require("lubridate")
require("terra")
require("tidyterra")

f_clm <- "/lud11/poppman/data/bff/dat/lud11/WWLLN/WWLLN_climatology.nc"
f_mth <- "/lud11/poppman/data/bff/dat/lud11/WWLLN/WWLLN_monthly.nc"

fire <- terra::rast(
  "/lud11/poppman/data/bff/dat/lud11/annual/fire_resampled_MODIS/Fire_2001.tif"
  ) %>%
  terra::crop(terra::ext(-180, 180, -90, 90))

dst <- "/lud11/poppman/data/bff/dat/lud11/static/WWLLN/Lightning_clim_MODIS.tif"
if (!file.exists(dst)) {
  print("Resampling lightning climatology...")
    terra::rast(f_clm) %>%
    terra::app(fun = sum) %>%
    terra::resample(fire, method = "bilinear") %>%
    terra::writeRaster(
      filename = dst,
      overwrite = TRUE
    )
}

# Annual values
raster_brick <- terra::rast(f_mth)
layer_names <- names(raster_brick)
layer_dates <- terra::time(raster_brick)
layer_years <- lubridate::year(as.Date(layer_dates))
layer_months <- lubridate::month(as.Date(layer_dates))

years <- max(2002, min(layer_years)):min(max(layer_years), 2024)

pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = length(years), clear = FALSE, width = 60
)

for (year in years) {
  print(paste("\nCreating year", year))
  cat("\nEntire year")
  layer_subset <- raster_brick[[which(layer_years == year)]]
    
  f_name <- paste0("Lightning_", year, ".tif")
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/lightning_resampled_MODIS",
    f_name
    )
  
  if (!file.exists(dst)) {
    layer_subset %>%
      terra::app(fun = mean) %>%
      terra::project(fire, method = "near") %>%
      terra::resample(fire, method = "bilinear") %>%
      terra::writeRaster(
          dst,
          overwrite = TRUE
          )
    gc()
  }

  cat("\nEquinoxes")
  layer_subset <- raster_brick[[
    which(layer_years == year & layer_months %in% 4:9)
    ]]
  
  f_name <- paste0("LightningEquinox_", year, ".tif")
  dst <- file.path(
    "/lud11/poppman/data/bff/dat/lud11/annual/lightning_equinox_resampled_MODIS",
    f_name
    )
  
  if (!file.exists(dst)) {
    layer_subset %>%
      terra::app(fun = mean) %>%
      terra::project(fire, method = "near") %>%
      terra::resample(fire, method = "bilinear") %>%
      terra::writeRaster(
          dst,
          overwrite = TRUE
          )
    gc()
  }
  pb$tick()
}