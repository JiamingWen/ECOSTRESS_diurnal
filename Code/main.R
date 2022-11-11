library(raster)
library(ncdf4)
library(stringr)
library(bigleaf)
library(abind)
library(zoo)

# setwd("/local/workdir/jw2495/Github/ECOSTRESS_diurnal/")
setwd('ECOSTRESS_diurnal')

RegionName = 'us-seg-region4new' #us-seg-region2new us-seg-region4new us-seg-region5new us-arm-region2new

source('Code/Functions.R') #load functions

###########################################################################
#step 1: provide region and time period information, generate directories, generate shapefile, download data
source('Code/Region_period_info.R')
source('Code/CreateDirectory.R')
source("Code/CreateShapefile.R")

###########################################################################
#step 2: download and check data
#using the shapefile to download ECOSTRESS LST/ET/qc images, and other ancillary datasets (GOES, MERRA-2, Landsat, CDL, flux tower data)

# ECOSTRESS LST, ET
source("Code/Ecostress_plot.R") #optional: to plot images from ECOSTRESS, compare against tower LST/ET
et_tem = raster_template(RegionName) #generate a raster template with 70m grids

# GOES LST
#run ReadGOES.py #to extract GOES LST samplings in the region of interest into csv files
source("Code/ReadGOES.R") #to interpolate GOES L2 observations to L3 grids

# National Crop data layer CDL
# source("Code/CDL.R")

###########################################################################
# step 3: use DTC model to contruct diurnal LST

#sunrise time 
library(suncalc)
sunrise=time_decomp(getSunlightTimes(as.Date(startdatestr),lat0,lon0)$sunrise+UTC_OFFSET*3600)
t_sr=sunrise[5]
starttime=as.POSIXlt(paste0(startdatestr," ",floor(t_sr),":",(t_sr-floor(t_sr))*60))

source("Code/DTC_functions.R") #Load functions that are needed to run the DTC model

# apply to the site (optional)
source("Code/Extract_site.R") #extract ECOSTRESS, GOES, and tower LST
source("Code/DTC_site.R") #run DTC for site

# apply to the region
#run DTC for GOES LST
source("Code/Compile_GOES_csv.R") #prepare GOES LST for running DTC model
source("Code/GOES_DTC_param.R") #derive DTC parameters for GOES LST
#run DTC for ECOSTRESS LST
source("Code/DTC_region.R")
# source("Code/DTC_region_plot.R") #optional - plot the results

###########################################################################
# step 4: use PT-JPL ET model to contruct diurnal ET
source("Code/MERRA2_met.R") #prepare MERRA-2 meteorological data
source("Code/Landsat.R") #or source("Code/HLS.R") #prepare Landsat or HLS albedo, NDVI, and SAVI
source("Code/Topt.R") #calculate optimal temperature
source("Code/fAPARmax.R") #calculate fPAR annual maximum

# apply to the site (optional)
source("Code/ETmodel_site.R") #The script involves some variables that exported from ETmodel_region.R

# apply to the region
source("Code/ETmodel_region.R")
# source("Code/ETmodel_region_plot.R")  #optional - plot the results
