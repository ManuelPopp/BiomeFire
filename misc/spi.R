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

if(interactive()){
  id=6789
} else {
  # load("Redo_indices.RData")
  id <- as.numeric(commandArgs(trailingOnly=TRUE))
  # id = bdones[id]
  # .libPaths(new='~/R/x86_64-pc-linux-gnu-library/3.6')
}

# Load necessary Packages
library(SPEI)
#library(sp)
library(raster)

### =========================================================================
### Prepare
### =========================================================================

#xt=extent(c(6,7,43,44))

msk = raster("/storage/brunp/Data/CHELSA_V.2.1/cmi/Monthly/CHELSA_cmi_01_1980_V.2.1.tif")

cmfl=list.files("/storage/karger/chelsa_V2/OUTPUT_MONTHLY/prec/",full.names=T)

cmfl=cmfl[-grep("2019", cmfl)]
yr=substr(basename(cmfl),start=16,stop=19)
mt=substr(basename(cmfl),start=13,stop=14)
date=as.Date(paste0(yr,"-",mt,"-15"))

dte=date[order(date)]
yr=yr[order(date)]
mt=mt[order(date)]
cmfl=cmfl[order(date)]

# print(yr)
# print(length(cmfl))
# print(head(cmfl))

# check if already done
drs_done = list.dirs("spi/", full.names = TRUE, recursive = FALSE)
ids_done = lapply(drs_done, function(x){
  fli = list.files(x)
  spli = strsplit(fli, split = "_")
  yr = sapply(spli, function(x){return(x[3])})
  return(yr)
})

id_in_all = all(sapply(ids_done, function(x,y){y %in% x}, y = id))
if(id_in_all){
  stop("I already exist")
}


# ### =========================================================================
# ### Generate tiles
# ### =========================================================================

n_cl=100

rorg=raster(cmfl[1])
rst=raster(rorg)
xt.org=extent(rorg)

# Define splits
splx=seq(from=xt.org[1],to=xt.org[2],length.out=n_cl+1)
sply=seq(from=xt.org[3],to=xt.org[4],length.out=n_cl+1)

# Define extents
exxe=list()
for(j in 1:n_cl){

  for(i in 1:n_cl){
    exxe[[length(exxe)+1]]=extent(splx[j],splx[j+1],sply[i],sply[i+1])
  }
}

my.ext=exxe[[id]]

### =========================================================================
### Load data
### =========================================================================

# Check if the exercise is necessary
xtr=crop(msk,my.ext)
if(all(is.na(values(xtr)))){
  stop("I fell in the ocean!")
}

vls=list()
for(i in 1:length(cmfl)){
  
  rst=raster(cmfl[i])
  crp=crop(rst,my.ext)
  vls[[i]]=values(crp)
  # print(i)
}

df.vals=do.call("rbind",vls)

### =========================================================================
### Calculate SPI
### =========================================================================

spi.12=list()

for(i in 1:ncol(df.vals)){
  
  if(!all(is.na(df.vals[,i]))){
    tsi=ts(df.vals[,i],freq=12,start=c(as.numeric(yr[1]),as.numeric(mt[1])))
   
    #spob.1=spei(tsi,1,ref.start=c(1981,1), ref.end=c(2010,1))
    spob.12=SPEI::spi(tsi,12,ref.start=c(1980,1), ref.end=c(2018,12))
    
    #spei.1[[i]]=as.numeric(spob.1$fitted)
    spi.12[[i]]=as.numeric(spob.12$fitted)
  } else {
    spi.12[[i]]=df.vals[,i]
  }
}

cbi.12=do.call("cbind",spi.12)
# #cbi.1=do.call("cbind",spei.1)

# ### =========================================================================
# ### Write rasters
# ### =========================================================================
 
sdirs=paste0("spi/",mt,"_",yr)
 for(i in 1:length(sdirs)){
   if(!dir.exists(sdirs[i])){
     dir.create(sdirs[i])
   }
}

for(i in 1:nrow(cbi.12)){
  
  rst.12=raster(crp)
  
  #values(rst.1)=cbi.1[i,]
  #flo.1=paste0(sdirs[i],"/",gsub("cmi",paste0("spei.1_",id),basename(cmfl[i])))
  #writeRaster(rst.1,file=flo.1,overwrite=T)
  
  if(i>11){
    flo.12=paste0(sdirs[i], "/", gsub("prec", paste0("spi.12_",id), basename(cmfl[i])))
    values(rst.12)=cbi.12[i,]
    writeRaster(rst.12,file=flo.12,overwrite=T)
  }
  
}
