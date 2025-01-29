require("terra")
require("tidyterra")

if (Sys.info()["sysname"] == "Windows") {
  dir_main <- "C:/Users/poppman/switchdrive/PhD/prj/bff"
} else {
  dir_main <- "/lud11/poppman/data/bff"
}

dir_dat <- file.path(dir_main, "dat")
f_gHM <- file.path(dir_dat, "lud11", "GlobalHumanModification", "gHM_4326.tif")
npp <- terra::rast("C:/Users/poppman/Downloads/MOD17A3H_Y_NPP_2023-01-01_gs_3600x1800.TIFF")
npp[npp == 255] <- NA
kg <- terra::rast("D:/onedrive/OneDrive - Eidg. Forschungsanstalt WSL/switchdrive/PhD/prj/bff/dat/chelsa_kg/CHELSA_kg0_unchanged.tif")
water <- terra::rast(file.path(dir_dat, "lud11", "modis", "landcover", "MCD12Q1_water_4326.tif")) %>%
  terra::resample(kg, method = "near")
water[water == 1] <- NA
gHM <- terra::rast(f_gHM) %>%
  terra::resample(kg, method = "near")

plot(gHM)

kg_pols <- kg %>%
  terra::mask(water, maskvalue = NA, inverse = FALSE, updatevalue = NA) %>%
  terra::mask(gHM, maskvalue = NA, inverse = FALSE, updatevalue = NA) %>%
  terra::as.polygons(values = TRUE, na.rm = TRUE)

npp_by_kg <- terra::extract(npp, kg_pols, fun = mean, na.rm = TRUE, weights = TRUE)
kg_pols$npp <- as.numeric(npp_by_kg[,2])
hist(as.numeric(npp_by_kg[,2]))
plot(kg_pols)
df <- as.data.frame(kg_pols)
df$code <- c(
  "Af", "Am", "As", "Aw", "BWk", "BWh", "BSk", "BSh", "Cfa", "Cfb", "Cfc",
  "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Dfa", "Dfb", "Dfc", "Dfd", "Dsa",
  "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "ET", "EF"
)
df <- df[order(df$npp),]
write.csv(df, file = file.path(dir_dat, "npp.csv"), row.names = FALSE)
