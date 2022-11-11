# ECOSTRESS_diurnal
## Introduction
This repository archives the scripts for constructing diurnal ECOSTRESS LST and ET for a given site/area (Wen et al., 2022). We first constructs 70 m diurnal LST utilizing a diurnal temperature cycle (DTC) model that fused ECOSTRESS and Geostationary Operational Environmental Satellite (GOES) LST. Next, we derives 70 m diurnal ET from the diurnally resolved LST, along with ancillary meteorological and reflectance data sets, using the Priestley-Taylor Jet Propulsion Laboratory (PT-JPL) algorithm.

Wen, J., Fisher, J. B., Parazoo, N. C., Hu, L., Litvak, M. E., & Sun, Y. (2022). Resolve the Clear‐Sky Continuous Diurnal Cycle of High‐Resolution ECOSTRESS Evapotranspiration and Land Surface Temperature. Water Resources Research, 58(9), e2022WR032227.

## Script Description
The overall workflow is described in main.R. It consists of four steps:

**Step 1**: Generate shapefile for the region of interest, provide information about e.g., time window, time zone, etc.

**Step 2**: Download and Preprocess the data.

**Step 3**: Construct diurnal LST using DTC model.

**Step 4**: Construct diurnal ET using PT-JPL ET model.

Several scripts that are used for analysis, but not used for data generation, are also provided in Code/Others folder.

## Input data
The datasets employed in this work include:

a. ECOSTRESS LST and ET (recommend downloading from AppEEARS https://lpdaac.usgs.gov/tools/appeears)

b. GOES LST (https://www.avl.class.noaa.gov)

c. MERRA-2 meteorological data (https://disc.gsfc.nasa.gov)

d. Landsat 8 reflectance data (https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1_SR) or HLS (https://lpdaac.usgs.gov)

e. Flux tower data (optional, for comparison and validation, can be downloaded from https://ameriflux.lbl.gov)

f. Land cover data (optional, https://nassgeodata.gmu.edu/CropScape)

Data example is not uploaded to Github due to space limit. It can be downloaded from: 
https://drive.google.com/drive/folders/1-Bun4vNSsYUHrGUbpNmd58d2nTspANFE?usp=share_link 

## R Package Version
R version 4.0.5 (2021-03-31)

Platform: x86_64-pc-linux-gnu (64-bit)

Running under: CentOS Linux 7 (Core)    

**Attached base packages**

parallel  stats  graphics  grDevices  utils  datasets  methods  base     

**Other attached packages**

suncalc_0.5.0       rasterVis_0.50.2    latticeExtra_0.6-29 terra_1.2-10       

lattice_0.20-44     rgeos_0.5-5         rgdal_1.5-23        sf_0.9-8    

akima_0.6-2.1       zoo_1.8-9           abind_1.4-5         bigleaf_0.7.1      

stringr_1.4.0       ncdf4_1.17          raster_3.4-10       sp_1.4-5

## Contact Information

jw2495@cornell.edu

Twitter: @JiamingWen233
