#CDL data
library(raster)
year0=2020 #2018 2019 2020 2021
cdl0=raster(paste0("Data/",RegionName,"/CDL/CDL_",year0,".tif"))
crs(cdl0)
plot(cdl0)

cdl1=projectRaster(cdl0,et_tem,method="ngb")
# cdl1[cellFromXY(cdl1,c(lon0,lat0))]
plot(cdl1)
summary(as.factor(cdl1[])) #value meaning: https://developers.google.com/earth-engine/datasets/catalog/USDA_NASS_CDL

################################################
#class 0-6 for us-seg
cdl2=cdl1
cdl2[]=0 #others
cdl2[cdl1[] %in% c(1:60,66:77,204:254)]=1 #croplands
cdl2[cdl1[] %in% c(63,141:143)]=2 #forests
cdl2[cdl1[] %in% c(64,152,176)]=3 #shrublands, grasslands
cdl2[cdl1[] %in% c(82,121:124)]=4 #Developed
cdl2[cdl1[] %in% c(87,190,195)]=5 #Wetlands
cdl2[cdl1[] %in% c(83,111)]=6 #Water
plot(cdl2)
writeRaster(cdl2,paste0("Data/",RegionName,"/CDL/CDL_",year0,"_reclassify.tif"),overwrite=T)

library(lattice)
library(rasterVis)
cdl2=raster(paste0("Data/",RegionName,"/CDL/CDL_",year0,"_reclassify.tif"))
landtype=c('Others','CRO','Forest','SHR&GRA','URB','WET','Water')
cdl2=as.factor(cdl2)
rat <- levels(cdl2)[[1]]
rat[["landtype"]] <- landtype
levels(cdl2) <- rat

palette <- rev(c("#00f1ff","#75B5EE","#9A9C9E","#E8E87A","#5dba5c",
                 "#BF56C3","#82342E"))
levelplot(cdl2,margin=F,colorkey=T,at=0:6,col.regions=palette,xlab="",ylab="")

#####################################################################################
#us-arm
sort(summary(as.factor(cdl1[])))
cdl2=cdl1
cdl2[]=0 #others #999999
cdl2[cdl1[] == 176]=1 #Grassland/Pasture #e8ffbf
cdl2[cdl1[] == 2]=2 #Cotton #ff2626
cdl2[cdl1[] == 4]=3 #Sorghum #ff9e0a
cdl2[cdl1[] == 1]=4 #Corn #ffd300
cdl2[cdl1[] == 26]=5 #Dbl Crop WinWht/Soybeans #707000
cdl2[cdl1[] == 5]=6 #Soybeans #267000
cdl2[cdl1[] == 24]=7 #Winter Wheat #a57000
cdl2[cdl1[] %in% c(111,190,195)]=8 #Water/Wetlands #4970a3
plot(cdl2)
writeRaster(cdl2,paste0("Data/",RegionName,"/CDL/CDL_",year0,"_reclassify.tif"),overwrite=T)

library(lattice)
library(rasterVis)
cdl2=raster(paste0("CDL/CDL_",year0,"_reclassify.tif"))
landtype=c('Others','Grassland/Pasture','Cotton','Sorghum','Corn','Dbl Crop WinWht/Soybeans','Soybeans','Winter Wheat','Water/Wetlands')
cdl2=as.factor(cdl2)
rat <- levels(cdl2)[[1]]
rat[["landtype"]] <- landtype
levels(cdl2) <- rat
palette <- c("#999999","#e8ffbf","#ff2626","#ff9e0a","#ffd300",
                 "#707000","#267000","#a57000","#4970a3")
levelplot(cdl2,margin=F,colorkey=T,at=0:8,col.regions=palette,xlab="",ylab="")
