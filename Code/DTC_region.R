#run DTC for the region of interest
library(raster)
library(ncdf4)

###################################################################################
#read the DTC parameters fitted with GOES LST
GOES_DTC_param=read.csv(paste0("Data/", RegionName,"/GOES_LST/DTC_Parameters/GOES_DTC_param.csv"))
Days=(ncol(GOES_DTC_param)-5)/2
param_names=c("t_max","t_ss",paste("T_sr_Day",1:(Days+1),sep = ""),paste("T_max_Day",1:Days,sep = ""))
GOES_params_stack=stack()
for (i in 3:ncol(GOES_DTC_param)){
  ras=raster(paste0("Data/", RegionName,"/GOES_LST/DTC_Parameters/GOES_DTC_param_",param_names[i-2],".nc"))
  GOES_params_stack=stack(GOES_params_stack,ras)
}

###################################################################################
#read GOES LST
filelist=list.files(paste0("Data/", RegionName,"/GOES_LST/ncfile"))
num=0
GOESstack=stack()
for (i in 1:length(filelist)){
  filename0=filelist[i]
  local_time=as.POSIXlt(substr(filename0,5,23),format = "%Y-%m-%d_%H-%M-%OS")+UTC_OFFSET*3600
  time_tmp=time_decomp(local_time)
  local_year=time_tmp[1]
  local_month=time_tmp[2]
  local_date=time_tmp[3]
  local_hour=floor(time_tmp[5])
  local_minute=round((time_tmp[5]-floor(time_tmp[5]))*60)
  timestamp=local_year*10000+local_month*100+local_date
  fit_time=as.numeric(difftime(local_time,starttime,units="hour"))
  if (timestamp>=startdate & timestamp<=enddate & fit_time>=0){
    num=num+1
    ras=raster(paste0("Data/", RegionName,"/GOES_LST/ncfile/",filename0))
    ras[]=ras[]-273.15
    GOESstack=stack(GOESstack,ras)
    GOESdf0=data.frame(local_year,local_month,local_date,local_hour,local_minute,fit_time)
    if (num==1){
      GOESdf=GOESdf0
    }else{
      GOESdf=rbind(GOESdf,GOESdf0)
    }
  }
}
GOESstack_df=as.data.frame(GOESstack)

###################################################################################
#read Ecostress LST
library(stringr)
QAfile=read.csv(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE-001-SDS-QC-lookup.csv"))
# summary(QAfile$Mandatory.QA.flags)
QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
filelist=list.files(paste0("Data/", RegionName,"/Ecostress_LST/"),pattern = "^ECO2LSTE.001_SDS_LST_doy")
Ecostack=stack()
num=0
for (i in 1:length(filelist)){
  filename0=filelist[i]
  tmp=strsplit(filename0,"_")[[1]]
  utc_year=as.integer(substr(tmp[4],4,7))
  doy0=as.integer(substr(tmp[4],8,10))
  utc_hour=as.integer(substr(tmp[4],11,12))
  utc_minute=as.integer(substr(tmp[4],13,14))
  local_time=as.POSIXlt(paste0(as.Date(doy0-1,origin=paste0(utc_year,"-01-01"))," ",utc_hour,":",utc_minute,":00"))+UTC_OFFSET*3600
  time_tmp=time_decomp(local_time)
  local_year=time_tmp[1]
  local_month=time_tmp[2]
  local_date=time_tmp[3]
  local_hour=floor(time_tmp[5])
  local_minute=round((time_tmp[5]-floor(time_tmp[5]))*60)
  timestamp=local_year*10000+local_month*100+local_date
  fit_time=as.numeric(difftime(local_time,starttime,units="hour"))
  if (timestamp>=startdate & timestamp<=enddate & fit_time>=0){
    LST=raster(paste0("Data/", RegionName,"/Ecostress_LST/",filename0))*0.02
    LST_QC=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE.001_SDS_QC_",tmp[4],"_aid0001.tif"))
    LST_cloudmask=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2CLD.001_SDS_CloudMask_",tmp[4],"_aid0001.tif"))
    LST[!(LST_QC[] %in% QA_good)]=NA
    # LST[LST_cloudmask[]!=1]=NA
    LST[LST_cloudmask[]!=1 & LST_cloudmask[]!=33]=NA
    if (sum(!is.na(LST[]))!=0){
      num=num+1
      LST=projectRaster(LST,GOESstack)
      LST[]=LST[]-273.15
      
      # #test
      # ras=raster(xmn=-106.6522,xmx=-106.5518,ymn=34.35204,ymx=34.4525)
      # LST=crop(LST,ras)
      
      Ecostack=stack(Ecostack,LST)
      Ecodf0=data.frame(local_year,local_month,local_date,local_hour,local_minute,fit_time)
      if (num==1){
        Ecodf=Ecodf0
      }else{
        Ecodf=rbind(Ecodf,Ecodf0)
      }
    }
  }
}

mat=as.matrix(stack(projectRaster(GOESstack,Ecostack),Ecostack,projectRaster(GOES_params_stack,Ecostack)))


# ##########################################
# #test for one pixel
# vec=mat[18251,]
# t0_GOES=GOESdf$fit_time
# t0_Eco=Ecodf$fit_time
# 
# temps0_GOES=vec[1:length(t0_GOES)]
# temps0_Eco=vec[(length(t0_GOES)+1):(length(t0_GOES)+length(t0_Eco))]
# GOES_param=vec[(length(t0_GOES)+length(t0_Eco)+1):length(vec)]
# t_max_GOES=GOES_param[1]
# t_ss_GOES=GOES_param[2]
# Days=(length(GOES_param)-3)/2
# T_sr_GOES=GOES_param[3:(3+Days)]
# T_max_GOES=GOES_param[(4+Days):(length(GOES_param))]
# 
# result0=DTC_Ecostress(t0_Eco,temps0_Eco,t0_GOES,temps0_GOES,t_max_GOES,t_ss_GOES,T_sr_GOES, T_max_GOES,t0_output,alpha=1,alpha_cali=T,leave_one_out=T)
# 
# range0=range(c(result0$temps.est_Eco,temps0_Eco,result0$leave_one_out_pred,result0$temps.est_GOES,temps0_GOES),na.rm=T)
# plot((t0_output+t_sr)/24, result0$temps.est_Eco,col="red",type="l",ylim=range0)
# points((t0_Eco+t_sr)/24, temps0_Eco,pch=20,col="red",cex=1.5)
# points((t0_Eco+t_sr)/24, result0$leave_one_out_pred,pch=8,col="red",cex=1.5)
# points((t0_GOES+t_sr)/24, temps0_GOES,pch=20,col="blue")



##########################################
#run for the whole region
fit_func=function(vec,t0_GOES,t0_Eco,t0_output=NULL,alpha=1,alpha_cali=F,leave_one_out=T){
  temps0_GOES=vec[1:length(t0_GOES)]
  temps0_Eco=vec[(length(t0_GOES)+1):(length(t0_GOES)+length(t0_Eco))]
  GOES_param=vec[(length(t0_GOES)+length(t0_Eco)+1):length(vec)]
  t_max_GOES=GOES_param[1]
  t_ss_GOES=GOES_param[2]
  Days=(length(GOES_param)-3)/2
  T_sr_GOES=GOES_param[3:(3+Days)]
  T_max_GOES=GOES_param[(4+Days):(length(GOES_param))]
  
  if (sum(!is.na(temps0_Eco))>=3){
    result0=DTC_Ecostress(t0_Eco,temps0_Eco,t0_GOES,temps0_GOES,t_max_GOES,t_ss_GOES,T_sr_GOES, T_max_GOES,t0_output,alpha=alpha,alpha_cali=alpha_cali,leave_one_out=leave_one_out)
    result=c(result0$alpha,result0$t_max,result0$t_ss,result0$T_sr.est,result0$T_max.est,result0$leave_one_out_pred,result0$temps.est_Eco)
    return(result)
  }else{
    return(rep(NA,1+1+1+(Days+1)+Days+length(t0_Eco)*leave_one_out+length(t0_output)))
  }
  
}


#output timestamps
timestamplist=seq(0.25,23.75,0.25) #output 0:15, 0:30 0:45, 1:00 etc
t0_output=NULL
predict_timemat=NULL
num=0
datenum=as.integer(as.Date(enddatestr)-as.Date(startdatestr))
for (dnum in 0:datenum){
  current_datestr=as.character(as.Date(startdatestr)+dnum)
  for (time0 in timestamplist){
    num=num+1
    time_tmp=as.POSIXlt(paste0(current_datestr," ",floor(time0),":",(time0-floor(time0))*60))
    tmp=as.double(difftime(time_tmp,starttime,units="hours"))
    if (tmp>0){
      t0_output=c(t0_output,tmp)
      predict_timemat=rbind(predict_timemat,time_decomp(time_tmp))
    }
  }
}

library(parallel)
cl <- makeCluster(30)
parallel::clusterExport(cl, c("calRMSE","fit_func","diurn","diurn.fit","diurn.fit_Eco","DTC_Ecostress"), envir=environment())
# parallel::clusterEvalQ(cl, c(library()))
data0=parApply(cl, mat,1,fit_func,t0_GOES=GOESdf$fit_time,t0_Eco=Ecodf$fit_time,t0_output=t0_output,alpha=1,alpha_cali=T,leave_one_out=T)
data=t(data0)
stopCluster(cl)

#export results
#alpha
alpharas=LST
alpharas[]=data[,1]
writeRaster(alpharas,filename=paste0("Data/", RegionName,"/Model/overpass/LST/alpha.nc"),overwrite=T)

#t_max
tmax_ras=LST
tmax_ras[]=data[,1+1]
writeRaster(tmax_ras,filename=paste0("Data/", RegionName,"/Model/overpass/LST/t_max.nc"),overwrite=T)

#t_ss
tssras=LST
tssras[]=data[,1+1+1]
writeRaster(tssras,filename=paste0("Data/", RegionName,"/Model/overpass/LST/t_ss.nc"),overwrite=T)

#T_sr
Days=(ncol(GOES_DTC_param)-5)/2
for (i in 1:(Days+1)){
  Tsrras=LST
  Tsrras[]=data[,1+1+1+i]
  writeRaster(Tsrras,filename=paste0("Data/", RegionName,"/Model/overpass/LST/T_sr_Day",i,".nc"),overwrite=T)
}

#T_max
for (i in 1:Days){
  Tmaxras=LST
  Tmaxras[]=data[,1+1+1+Days+1+i]
  writeRaster(Tmaxras,filename=paste0("Data/", RegionName,"/Model/overpass/LST/T_max_Day",i,".nc"),overwrite=T)
}

leave_one_out=T
if (leave_one_out){
  for (i in 1:nrow(Ecodf)){
    output=LST
    output[]=data[,1+1+1+Days+1+Days+i]+273.15
    writeRaster(output,filename=paste0("Data/", RegionName,"/Model/overpass/LST/",Ecodf$local_year[i],"_",Ecodf$local_month[i],"_",Ecodf$local_date[i],"_",Ecodf$local_hour[i],"_",Ecodf$local_minute[i],".nc"),overwrite=T)
  }
}

if (!is.null(t0_output)){
  for (i in 1:length(t0_output)){
    output=LST
    output[]=data[,1+1+1+Days+1+Days+nrow(Ecodf)*leave_one_out+i]+273.15
    writeRaster(output,filename=paste0("Data/", RegionName,"/Model/diurnal/LST/",predict_timemat[i,1],"_",predict_timemat[i,2],"_",predict_timemat[i,3],"_",floor(predict_timemat[i,5]),"_",round((predict_timemat[i,5]-floor(predict_timemat[i,5]))*60),".nc"),overwrite=T)
  }
}
