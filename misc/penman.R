# ###########################################################################
# Get an overview over the data and figure out which traits make most sense
# to be used
#
# $Date: 2017-11-29
#
# Author: Philipp Brun, philipp.brun@wsl.ch
# National Institute of Aquatic Resources (DTU-Aqua),
# Technical University of Denmark, Charlottenlund
#
# ###########################################################################

### =========================================================================
### Initialise system
### =========================================================================
# Start with house cleaning
rm(list = ls()); graphics.off()

### =========================================================================
### Initialise system
### =========================================================================

# Load necessary Packages
library(sp)
library(raster)
library(bigleaf)

### =========================================================================
### Go!
### =========================================================================

if(interactive()){
  id=3
} else {
  id <- as.numeric(commandArgs(trailingOnly=TRUE))
  # .libPaths(new='~/R/x86_64-pc-linux-gnu-library/3.6')
}


mnts = paste0("_", seq(1, 12), "_")

#datn=expand.grid(mnts,1984)
datn = cbind(
  c("_02_","_03_","_03_","_07_","_06_","_04_","_04_","_04_"),
  c(1979, 1980, 2018, 2011, 2014, 2019, 2018, 2016)
  )
datn = as.data.frame(datn)

my.dat = datn[id, ]

# Check if pet file exists and is complete - if so, stop the exercise
#pet_check=paste0("/storage/brunp/Data/CHELSA_V.2.1/PET/Penman/Monthly/CHELSA_pet_penman",datn[id,1],datn[id,2],"_V.2.1.tif")
#if(file.exists(pet_check)){
#   rda=values(raster(pet_check))
#   ln=length(which(is.na(rda)))
#   if(ln<=677647411){
#      stop()
#   } else {
#      rm(rda)
#   }
#}

# determine days of month
dte = as.Date(paste0(my.dat[, 2], my.dat[, 1], 15), format = "%Y_%m_%d")
mtna = vector()
for(i in -17:17){
  mtna = append(mtna,months(dte + i))
}
dom = max(table(mtna))

# Get temperature file
tfls = c(
  paste0("tas/tas_files",datn[id,1],".txt"),
  paste0("tas/tas_files",datn[id,1],"addon.txt")
  )
tfl = do.call("c", lapply(tfls, readLines))
tfl = grep(datn[id, 2], tfl, value = TRUE)
tf_temp = paste0("temp/tas", datn[id, 1], datn[id, 2], ".tif")
download.file(url = tfl,destfile = tf_temp)

# Get Radiation file
rsfl = paste0(
  "/storage/brunp/Data/CHELSA_V.2.1/RSDS/CHELSA/Monthly/CHELSA_rsds_",
  datn[id,2],
  datn[id,1],
  "V.2.1.tif"
  )

# Get Radiation file
rtfl = paste0(
  "/storage/karger/chelsa_V2/OUTPUT_MONTHLY/rtsr/ERA5Land_rtsr_",
  datn[id,1],
  datn[id,2],
  "_V.2.1.tif"
  )

# Get vpd file
vpfl = paste0(
  "/storage/brunp/Data/CHELSA_V.2.1/vpd/Monthly/CHELSA_vpd",
  datn[id,1],
  datn[id,2],
  "_V.2.1.tif"
  )


# Get Wind file
wifl = paste0(
  "/storage/karger/chelsa_V2/OUTPUT_MONTHLY/wind/ERA5_wind",
  datn[id,1],
  datn[id,2],
  "_V.2.1.tif"
  )


### =========================================================================
### Prepare Land mask
### =========================================================================

msk = values(raster("chelsa-w5e5v1.0_obsclim_mask_30arcsec_global.tif"))
nna = which(msk == 1)
rm(msk)

### =========================================================================
### Prepare radiation
### =========================================================================

# small batch to fix the 180 dateline problem
tempgrid <- tempfile(fileext = ".sgrd")
rnfl = tempfile(fileext = ".sgrd")
template <- "/storage/karger/aux/template.sgrd"
system(
  paste0(
    "saga_cmd grid_tools 0 -INPUT=", rtfl,
    " -KEEP_TYPE=0 -SCALE_UP=0 -TARGET_DEFINITION=1 -TARGET_TEMPLATE=",
    template, " -OUTPUT=", tempgrid
    )
  )
system(
  paste0(
    "saga_cmd pj_proj4 13 -INPUT=", tempgrid, " -DIRECTION=1 -OUTPUT=",
    tempgrid
    )
  )
system(
  paste0(
    "saga_cmd grid_tools 7 -INPUT=",
    tempgrid, " -MASK=file -RESULT= -THRESHOLD=0.500000 -RESULT=", tempgrid
    )
  )
system(
  paste0(
    "saga_cmd pj_proj4 13 -INPUT=", tempgrid, " -DIRECTION=0 -OUTPUT=", tempgrid
    )
  )

system(
  paste0(
    'saga_cmd grid_calculus 1 -RESAMPLING=1 -FORMULA="a*1000/(3600*24)+b/86400" -NAME=Calculation -FNAME=0 -USE_NODATA=0 -TYPE=7 -GRIDS=',
    rsfl, ' -XGRIDS=', tempgrid, ' -RESULT=', rnfl
    )
  )

rnet = values(raster(gsub(".sgrd", ".sdat", rnfl)))
rnet = rnet[nna]
if(any(na.omit(rnet) < 0)){
 rnet[which(rnet < 0)] = 0
}

### =========================================================================
### Prepare Wind
### =========================================================================

# Downscale wind 10
wind_temp = tempfile(fileext = ".sgrd")
ratio_file = "/storage/brunp/Data/Wind/wind_ratio_file.sgrd"
system(
  paste0(
    'saga_cmd grid_calculus 1 -RESAMPLING=1 -FORMULA="a*b" -NAME=Calculation -FNAME=0 -USE_NODATA=0 -TYPE=7 -GRIDS=',
    ratio_file, ' -XGRIDS=', wifl, ' -RESULT=', wind_temp
    )
  )


# Calculate wind 2
z0 = values(raster("/storage/brunp/Data/Wind/z0_10_50_30arcsec.sdat"))
z0 = z0[nna]

z0[z0 > 4] = 4
z0[z0 < 0.0002] = 0.0002

w10 = values(raster(gsub(".sgrd", ".sdat", wind_temp)))
w10 = w10[nna]

w2 = w10 * log((z0 + 2) / z0) / log(10 / z0)

rm(w10)
rm(z0)

# Calculate GA
Ga = w2/208
rm(w2)

### =========================================================================
### Prepare temperature
### =========================================================================

temp = values(raster(tf_temp))
temp = temp / 10 - 273.15
temp = temp[nna]

### =========================================================================
### Prepare vpd
### =========================================================================

vpd = values(raster(vpfl))
vpd = vpd/1000
vpd = vpd[nna]

### =========================================================================
### Prepare pressure
### =========================================================================

elev = values(raster("/storage/karger/chelsa_V2/INPUT/dem_latlong.sdat"))
elev = elev[nna]

pressure = pressure.from.elevation(elev = elev, Tair = temp, VPD = vpd)

rm(elev)

### =========================================================================
### Calculate PET
### =========================================================================

Gs_pot=ms.to.mol(1/70, temp, pressure)

v.pet = potential.ET(
  Tair = temp,
  pressure = pressure,
  Rn = rnet,
  Ga = Ga,
  G = 0.1*rnet,
  S = 0,
  VPD = vpd,
  Gs_pot = Gs_pot,
  approach = "Penman-Monteith",
  Esat.formula = "Sonntag_1990"
  )

rm(vpd)
rm(temp)
rm(rnet)
rm(Gs_pot)
rm(pressure)
rm(Ga)

### =========================================================================
### Save PET
### =========================================================================

# Create Output raster
oura = raster(raster(tf_temp))
values(oura) = rep(NA,length(oura))
values(oura)[nna] = v.pet[, 1] * 3600 * 24 * dom * 100
pet_temp = tempfile(fileext = ".sdat")
pet_temp2 = paste0(
  "temp/CHELSA_pet_penman", datn[id, 1], datn[id, 2], "_V.2.1.sgrd"
  )
pet_out = paste0(
  "pet/Monthly/CHELSA_pet_penman", datn[id, 1], datn[id, 2], "_V.2.1.tif"
  )

writeRaster(oura,file=pet_temp,format="SAGA",datatype="INT2U",overwrite=T)

# resample to chelsa resolution
system(
  paste0(
    "saga_cmd grid_tools 0 -INPUT=",
    gsub(".sdat",".sgrd",pet_temp),
    " -KEEP_TYPE=1 -SCALE_UP=0 -SCALE_DOWN=0 -TARGET_DEFINITION=1 -TARGET_TEMPLATE=",
    tf_temp," -OUTPUT=",pet_temp2
    )
  )

# convert to packged tif.
ingrid = gsub(".sgrd", ".sdat", pet_temp2)
system(
  paste0(
    "singularity exec /storage/karger/singularity/nco.sif gdal_translate -ot UInt16 -a_scale 0.01 -a_offset 0 -a_nodata 65535 -co \"COMPRESS=DEFLATE\" -co \"PREDICTOR=2\" ",
    ingrid, " ", pet_out
    )
  )

rm(v.pet)
rm(oura)

### =========================================================================
### Save wind
### =========================================================================

# Create Output raster
wini=values(raster(gsub(".sgrd", ".sdat", wind_temp)))
wini=wini[nna]

if(any(na.omit(wini)>60)){
  wini[which(wini>60)]=60
}

oura=raster(raster(tf_temp))
values(oura)=rep(NA,length(oura))
values(oura)[nna]=wini*1000

wind_out = paste0(
  "wind/Monthly/CHELSA_wind", datn[id, 1], datn[id, 2], "_V.2.1.tif"
  )

writeRaster(
  oura,file = pet_temp, format = "SAGA", datatype = "INT2U", overwrite = T
  )

# resample to chelsa resolution
system(
  paste0(
    "saga_cmd grid_tools 0 -INPUT=",
    gsub(".sdat",".sgrd",pet_temp),
    " -KEEP_TYPE=1 -SCALE_UP=0 -SCALE_DOWN=0 -TARGET_DEFINITION=1 -TARGET_TEMPLATE=",
    tf_temp, " -OUTPUT=", pet_temp2
    )
  )

# convert to packged tif.
ingrid = gsub(".sgrd",".sdat",pet_temp2)
system(
  paste0(
    "singularity exec /storage/karger/singularity/nco.sif gdal_translate -ot UInt16 -a_scale 0.001 -a_offset 0 -a_nodata 65535 -co \"COMPRESS=DEFLATE\" -co \"PREDICTOR=2\" ",
    ingrid, " ", wind_out
    )
  )

unlink(tf_temp)
delfin = list.files(
  "/temp", pattern=gsub(".sdat", "", basename(pet_temp2)), full.names = T
  )
unlink(delfin)
