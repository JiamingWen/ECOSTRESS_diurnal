#extract LST at site location
##################################################################################
#Ecostress
library(raster)
library(stringr)
QAfile=read.csv(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE-001-SDS-QC-lookup.csv"))
summary(QAfile$Mandatory.QA.flags)
QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
filelist=list.files(paste0("Data/",RegionName,"/Ecostress_LST"),pattern = "^ECO2LSTE.001_SDS_LST_doy")

num=0
for (i in 1:length(filelist)){
  filename0=filelist[i]
  tmp=strsplit(filename0,"_")[[1]]
  LST=raster(paste0("Data/", RegionName,"/Ecostress_LST/",filename0))*0.02
  LST_QC=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE.001_SDS_QC_",tmp[4],"_aid0001.tif"))
  LST_cloudmask=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2CLD.001_SDS_CloudMask_",tmp[4],"_aid0001.tif"))
  LST[!(LST_QC[] %in% QA_good)]=NA
  LST[LST_cloudmask[]!=1]=NA
  Ecostress_tower=LST[cellFromXY(LST,c(lon0,lat0))]
  Emiss=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE.001_SDS_EmisWB_",tmp[4],"_aid0001.tif"))*0.002+0.49 #broadband emissivity
  Emiss[!(LST_QC[] %in% QA_good)]=NA
  Emiss[LST_cloudmask[]!=1]=NA
  Emiss_tower=Emiss[cellFromXY(Emiss,c(lon0,lat0))]
  # LSTerr=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE.001_SDS_LST_err_",tmp[4],"_aid0001.tif"))*0.04
  # LSTerr[!(LST_QC[] %in% QA_good)]=NA
  # LSTerr[LST_cloudmask[]!=1]=NA
  # LSTerr_tower=LSTerr[cellFromXY(Emiss,c(lon0,lat0))]
  
  utc_year=as.integer(substr(tmp[4],4,7))
  doy0=as.integer(substr(tmp[4],8,10))
  utc_hour=as.integer(substr(tmp[4],11,12))
  utc_minute=as.integer(substr(tmp[4],13,14))
  local_time=as.POSIXlt(paste0(as.Date(doy0-1,origin=paste0(utc_year,"-01-01"))," ",utc_hour,":",utc_minute,":00"),tz = "GMT")+UTC_OFFSET*3600
  time_tmp=time_decomp(local_time)
  local_year=time_tmp[1]
  local_month=time_tmp[2]
  local_date=time_tmp[3]
  local_hour=floor(time_tmp[5])
  local_minute=round((time_tmp[5]-floor(time_tmp[5]))*60)
  
  timestamp=local_year*10000+local_month*100+local_date
  if (timestamp>=startdate & timestamp<=enddate){
    num=num+1
    result0=data.frame(local_year,local_month,local_date,local_hour,local_minute,Ecostress_tower,Emiss_tower) #LSTerr_tower
    if (num==1){
      result=result0
    }else{
      result=rbind(result,result0)
    }
  }
  
  #write the eps_s
  if (!dir.exists(paste0("Data/", RegionName,"/Model/OtherData/eps_s"))){dir.create(paste0("Data/", RegionName,"/Model/OtherData/eps_s"))}
  timestr=paste0(local_year,"-",str_pad(local_month,2,pad="0"),"-",str_pad(local_date,2,pad="0"),"_",str_pad(local_hour,2,pad="0"),"-",str_pad(local_minute,2,pad="0"))
  writeRaster(Emiss,filename=paste0("Data/", RegionName,"/Model/OtherData/eps_s/",timestr,".nc"),overwrite=T)
}

result=na.omit(result)
write.csv(result,paste0("Data/", RegionName,"/Tower/Ecostress_tower.csv"),row.names = F)

##################################################################################
#GOES LST interpolated
filelist=list.files(paste0("Data/", RegionName,"/GOES_LST/ncfile"))
num=0
for (i in 1:length(filelist)){
  filename0=filelist[i]
  local_time=as.POSIXlt(substr(filename0,5,23),format = "%Y-%m-%d_%H-%M-%OS")+UTC_OFFSET*3600
  time_tmp=time_decomp(local_time)
  local_year=time_tmp[1]
  local_month=time_tmp[2]
  local_date=time_tmp[3]
  local_hour=floor(time_tmp[5])
  local_minute=floor((time_tmp[5]-floor(time_tmp[5]))*60)

  timestamp=local_year*10000+local_month*100+local_date
  if (timestamp>=startdate & timestamp<=enddate){
    ras=raster(paste0("Data/", RegionName,"/GOES_LST/ncfile/",filename0))
    GOES_LST_tower=ras[cellFromXY(ras,c(lon0,lat0))]
    num=num+1
    result0=data.frame(local_year,local_month,local_date,local_hour,local_minute,GOES_LST_tower)
    if (num==1){
      result=result0
    }else{
      result=rbind(result,result0)
    }

  }
}
write.csv(result,paste0("Data/", RegionName,"/Tower/GOES_tower.csv"),row.names = F)

##################################################################################
#Tower LST estimated using longwave radiation
fluxfolder=list.dirs(paste0("Data/", RegionName,"/Tower"),full.names = T,recursive=F)
fluxfile=list.files(fluxfolder,pattern = 'AMF_.*\\.csv',full.names = T)
Tower_data=read.csv(file=fluxfile,header = T,skip=2,na.strings = -9999)
Tower_data=Tower_data[Tower_data$TIMESTAMP_START>=startdate*10000 & Tower_data$TIMESTAMP_START<(enddate+1)*10000,]

local_time=as.character(Tower_data$TIMESTAMP_START)
local_year=as.integer(substr(local_time,1,4))
local_month=as.integer(substr(local_time,5,6))
local_date=as.integer(substr(local_time,7,8))
local_hour=as.integer(substr(local_time,9,10))
local_minute=as.integer(substr(local_time,11,12))+15

Ecostress_data=read.csv(paste0("Data/",RegionName,"/Tower/Ecostress_tower.csv"))
eps=mean(Ecostress_data$Emiss_tower,na.rm=T)
# Read_eps_s_site=function(x){
#   Ecostress_data=na.omit(read.csv("Tower/Ecostress_tower.csv"))
#   Ecostress_timestamp=(Ecostress_data$local_year*10000+Ecostress_data$local_month*100+Ecostress_data$local_date)*10000
#   Ecostress_data[which(abs(x-Ecostress_timestamp)==min(abs(x-Ecostress_timestamp),na.rm=T)),"Emiss_tower"]}
# eps=sapply(Tower_data$TIMESTAMP_START, Read_eps_s_site) #use the nearest eps_s

#variable name
if (sitename == "us_seg"){
  LW_IN_varname="LW_IN"
  LW_OUT_varname="LW_OUT"
}else if (sitename == "us_arm"){
  LW_IN_varname="LW_IN_1_1_1"
  LW_OUT_varname="LW_OUT_1_1_1"
}

Tower_LST=((Tower_data[,LW_OUT_varname]-Tower_data[,LW_IN_varname]*(1-eps))/(5.67*10^(-8))/eps)^0.25

result=data.frame(local_year,local_month,local_date,local_hour,local_minute,LW_IN=Tower_data$LW_IN,LW_OUT=Tower_data$LW_OUT,Tower_LST)
write.csv(result,paste0("Data/", RegionName,"/Tower/Tower_LST.csv"),row.names = F)
