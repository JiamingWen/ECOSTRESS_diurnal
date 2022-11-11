#plots for Ecostress LST and ET
library(raster)
library(ncdf4)
library(stringr)

variable0 = 'Ecostress_ET' #Ecostress_LST Ecostress_ET
path0 = paste0("Data/",RegionName,"/", variable0) 

#####################################################################
#extract tower LW_OUT or LE if available
fluxfolder=list.dirs(paste0("Data/", RegionName,"/Tower"),full.names = T,recursive = F)
fluxfile=list.files(fluxfolder,pattern = 'AMF_.*\\.csv',full.names = T)
Tower_data=read.csv(file=fluxfile,header = T,skip=2,na.strings = -9999)

#variable name
if (sitename == "us_seg"){
  LW_IN_varname="LW_IN"
  LW_OUT_varname="LW_OUT"
  ET_varname="LE"
}else if (sitename %in% c("us_arm")){
  LW_IN_varname="LW_IN_1_1_1"
  LW_OUT_varname="LW_OUT_1_1_1"
  ET_varname="LE_1_1_1"
}

#time steps
if (strsplit(fluxfile,"_")[[1]][7] == "HH"){
  timestamp_tower=Tower_data$TIMESTAMP_START+15
}else if (strsplit(fluxfile,"_")[[1]][7] == "HR"){
  timestamp_tower=Tower_data$TIMESTAMP_START+30 #us-ne1
}else{
  stop("time step is wrong")
}

############################################################################
#plot ECOSTRESS LST or ET
if (!dir.exists(paste0(path0,"/maps"))){dir.create(paste0(path0,"/maps"))}

if (variable0=="Ecostress_LST"){
  patten0="^ECO2LSTE.001_SDS_LST_doy"
  num0=4
}else if(variable0=="Ecostress_ET"){
  patten0="^ECO3ETPTJPL.001_EVAPOTRANSPIRATION_PT_JPL_ETinst_doy"
  num0=6
}
filenames=list.files(path = path0,pattern = patten0)

num=0
for (filename0 in filenames){
  tmp=strsplit(filename0,"_")[[1]]
  utc_year=as.integer(substr(tmp[num0],4,7))
  doy0=as.integer(substr(tmp[num0],8,10))
  utc_hour=as.integer(substr(tmp[num0],11,12))
  utc_minute=as.integer(substr(tmp[num0],13,14))
  local_time=as.POSIXlt(paste0(as.Date(doy0-1,origin=paste0(utc_year,"-01-01"))," ",utc_hour,":",utc_minute,":00"),tz = "GMT")+UTC_OFFSET*3600
  time_tmp=time_decomp(local_time)
  local_year=time_tmp[1]
  local_month=time_tmp[2]
  local_date=time_tmp[3]
  local_hour=floor(time_tmp[5])
  local_minute=round((time_tmp[5]-floor(time_tmp[5]))*60)
  timestr=paste0(local_year,"-",str_pad(local_month,2,pad="0"),"-",str_pad(local_date,2,pad="0"),"_",str_pad(local_hour,2,pad="0"),"-",str_pad(local_minute,2,pad="0"))
  
  if (variable0=="Ecostress_LST"){
    QAfile=read.csv(paste0(path0, "/ECO2LSTE-001-SDS-QC-lookup.csv"))
    # # summary(QAfile$Mandatory.QA.flags)
    QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
    ras=raster(paste0(path0,"/",filename0))*0.02
    LST_QC=raster(paste0(path0,"/","ECO2LSTE.001_SDS_QC_",tmp[4],"_aid0001.tif"))
    LST_cloudmask=raster(paste0(path0,"/ECO2CLD.001_SDS_CloudMask_",tmp[4],"_aid0001.tif"))
    Emis = raster(paste0(path0,"/","ECO2LSTE.001_SDS_EmisWB_",tmp[4],"_aid0001.tif")) * 0.002 + 0.49
    ras[!(LST_QC[] %in% QA_good)]=NA
    ras[LST_cloudmask[]!=1]=NA
    
    #extract ecostress LST
    colnum=colFromX(ras,lon0)
    rownum=rowFromY(ras,lat0)
    radius=1
    if (sum(is.na(c(colnum,rownum)))==0){
      cellnum=cellFromRowColCombine(ras,(rownum-radius):(rownum+radius),(colnum-radius):(colnum+radius))
      ECOSTRESS_val=mean(ras[cellnum],na.rm=T)
      eps0=mean(Emis[cellnum],na.rm=T)
      # print(eps0)
    }else{
      ECOSTRESS_val=NA
      eps0=NA
    }

    #extract tower
    overpass_timestamp=local_year*1e8+local_month*1e6+local_date*1e4+local_hour*1e2+local_minute
    LW_IN=Tower_data[which(abs(overpass_timestamp-timestamp_tower)==min(abs(overpass_timestamp-timestamp_tower),na.rm=T)),LW_IN_varname]
    LW_OUT=Tower_data[which(abs(overpass_timestamp-timestamp_tower)==min(abs(overpass_timestamp-timestamp_tower),na.rm=T)),LW_OUT_varname]
    Tower_val=((LW_OUT-LW_IN*(1-eps0))/(5.67*10^(-8))/eps0)^0.25
  }else if(variable0=="Ecostress_ET"){
    ras=raster(paste0(path0,"/",filename0))
    # et_l2_qc=raster(paste0(path0,"/","ECO3ANCQA.001_L3_L4_QA_ECOSTRESS_L2_QC_",tmp[4],"_aid0001.tif"))
    # et_mod04_qc=raster(paste0(path0,"/","ECO3ANCQA.001_L3_L4_QA_MOD04_QC_",tmp[4],"_aid0001.tif"))
    # et_mod06_qc=raster(paste0(path0,"/","ECO3ANCQA.001_L3_L4_QA_MOD06_1km_QC_",tmp[4],"_aid0001.tif"))
    # ras[et_l2_qc[]!=1]=NA
    # ras[!(et_mod04_qc[] %in% c(85,119))]=NA
    
    #extract ecostress ET
    colnum=colFromX(ras,lon0)
    rownum=rowFromY(ras,lat0)
    radius=1
    if (sum(is.na(c(colnum,rownum)))==0){
      cellnum=cellFromRowColCombine(ras,(rownum-radius):(rownum+radius),(colnum-radius):(colnum+radius))
      ECOSTRESS_val=mean(ras[cellnum],na.rm=T)
    }else{
      ECOSTRESS_val=NA
    }
    
    #extract tower LW_OUT
    overpass_timestamp=local_year*1e8+local_month*1e6+local_date*1e4+local_hour*1e2+local_minute
    Tower_val=Tower_data[which(abs(overpass_timestamp-timestamp_tower)==min(abs(overpass_timestamp-timestamp_tower),na.rm=T)),ET_varname]
    
  }

  png(file=paste0(path0,"/maps/",timestr,".png"),height=300,width=300)
  plot(ras)
  dev.off()
  
  if(num==0){
    result=data.frame(local_year,local_month,local_date,local_hour,local_minute,ECOSTRESS_val,Tower_val)
  }else{
    result=rbind(result,data.frame(local_year,local_month,local_date,local_hour,local_minute,ECOSTRESS_val,Tower_val))
  }
  num=num+1

  # write.csv(result,file=paste0(path0,"/maps/values.csv"))
}

#compare ECOSTRESS observations with tower 
range0=range(c(result$ECOSTRESS_val,result$Tower_val),na.rm=T)
plot(result$ECOSTRESS_val,result$Tower_val,pch=20,xlim=range0,ylim=range0,xlab="ECOSTRESS",ylab="Tower")
abline(0,1,lty=2)

lmfit=lm(ECOSTRESS_val~Tower_val,data=result)
summary(lmfit)