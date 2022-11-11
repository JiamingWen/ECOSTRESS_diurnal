#extract NDVI and albedo from HLS for us-arm-region2new
setwd("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new")
lon0=-97.4888;lat0=36.6058;RegionName="us-arm-region2new";sitename="us_arm";offset0=0.15;UTC_OFFSET=-6

library(raster)
library(zoo)
source("../et_template2.R")

datatype='L30' #S30 L30
path=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/",datatype,"/2021/14/S/P/F/")
folderlist=list.files(path)
for (folder in folderlist){
  print(folder)
  yearstr=substr(folder,16,19)
  doystr=substr(folder,20,22)
  
  if (datatype=='S30'){
    B02=raster(paste0(path,folder,"/",folder,".B02.tif")) #blue
    B04=raster(paste0(path,folder,"/",folder,".B04.tif")) #red
    B8A=raster(paste0(path,folder,"/",folder,".B8A.tif")) #nir
    B11=raster(paste0(path,folder,"/",folder,".B11.tif")) #swir1
    B12=raster(paste0(path,folder,"/",folder,".B12.tif")) #swir2
    ndvi1=(B8A-B04)/(B8A+B04)
    albedo1=0.356*B02+0.130*B04+0.373*B8A+0.085*B11+0.072*B12-0.0018
  }else if (datatype=='L30'){
    B02=raster(paste0(path,folder,"/",folder,".B02.tif")) #blue
    B04=raster(paste0(path,folder,"/",folder,".B04.tif")) #red
    B05=raster(paste0(path,folder,"/",folder,".B05.tif")) #nir
    B06=raster(paste0(path,folder,"/",folder,".B06.tif")) #swir1
    B07=raster(paste0(path,folder,"/",folder,".B07.tif")) #swir2
    ndvi1=(B05-B04)/(B05+B04)
    albedo1=0.356*B02+0.130*B04+0.373*B05+0.085*B06+0.072*B07-0.0018
  }

  
  ndvi=projectRaster(ndvi1,et_tem)
  ndvi[ndvi<(-1)]=NA
  ndvi[ndvi>1]=NA
  albedo=projectRaster(albedo1,et_tem)
  
  if (sum(!is.na(ndvi[]))>100){
    png(file=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/NDVI/",datatype,"_",yearstr,doystr,".png"),height=300,width=300)
    plot(ndvi,main=paste0(yearstr,doystr))
    dev.off()
    
    png(file=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/albedo/",datatype,"_",yearstr,doystr,".png"),height=300,width=300)
    plot(albedo,main=paste0(yearstr,doystr))
    dev.off()
    
    writeRaster(ndvi,filename = paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/NDVI/",datatype,"_",yearstr,doystr,".nc"),overwrite=T)
    writeRaster(albedo,filename = paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/albedo/",datatype,"_",yearstr,doystr,".nc"),overwrite=T)
  }
}


#manually delete ndvi data that are cloud contaminated
#remove albedo with the same file name
filelist=list.files("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/albedo")
for (filename0 in filelist){
  if (!file.exists(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/NDVI/",filename0))){
    file.remove(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/albedo/",filename0))
  }
}


#interpolated to daily
variable="albedo" #NDVI albedo
if (!dir.exists(paste0("Model/OtherData/Landsat_",variable,"_daily"))){dir.create(paste0("Model/OtherData/Landsat_",variable,"_daily"))}
filelist=list.files(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/",variable),pattern = ".nc")
doylist=sapply(filelist, function(x){as.integer(substr(x,9,11))})
rasstack=stack()
for (doy in 1:365){
  print(doy)
  if (doy %in% doylist){
    ras=raster(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-arm-region2new/HLS/",variable,"/",filelist[which(doylist==doy)][1]))
    if (variable=="NDVI"){
      ras[ras<(-1)]=NA
      ras[ras>1]=NA
    }else if (variable=="albedo"){
      ras[ras<0]=NA
      ras[ras>1]=NA
    }
  }else{
    ras=et_tem
  }
  rasstack=stack(rasstack,ras)
}

mat=as.matrix(rasstack)
mat_gapfilled=t(apply(mat, 1, FUN=function(x){na.approx(x,na.rm=F,maxgap=25)}))

year=2021
for (doy in 1:365){
  doystr=str_pad(doy,3,pad="0")
  ras_result=et_tem
  ras_result[]=mat_gapfilled[,doy]
  writeRaster(ras_result,filename = paste0("Model/OtherData/Landsat_",variable,"_daily/",year,doystr,".nc"),overwrite=T)
}



#daily and 2-week forward average of Landsat SAVI
if (!dir.exists(paste0("Model/OtherData/Landsat_savi_daily"))){dir.create(paste0("Model/OtherData/Landsat_savi_daily"))}
if (!dir.exists(paste0("Model/OtherData/Landsat_savi_2week"))){dir.create(paste0("Model/OtherData/Landsat_savi_2week"))}

savistack=stack()

for (doy in 1:365){ #local_doy
  print(doy)
  doystr=str_pad(doy,3,pad="0")
  
  ndvi=raster(paste0("Model/OtherData/Landsat_NDVI_daily/",year,doystr,".nc"))
  savi0=0.45*ndvi+0.132
  savi=projectRaster(savi0,et_tem)
  writeRaster(savi,filename = paste0("Model/OtherData/Landsat_savi_daily/",year,doystr,".nc"),overwrite=T)
  
  savistack=stack(savistack,savi)
}

savi_mat=as.matrix(savistack)
savi_mat_2week=t(apply(savi_mat, 1, FUN=function(x){movingFun(x,n=14,fun=mean,type="to",na.rm=T)})) #13min

for (doy in 1:365){
  doystr=str_pad(doy,3,pad="0")
  ras_savi=et_tem
  ras_savi[]=savi_mat_2week[,doy]
  writeRaster(ras_savi,filename = paste0("Model/OtherData/Landsat_savi_2week/",year,doystr,".nc"),overwrite=T)
}