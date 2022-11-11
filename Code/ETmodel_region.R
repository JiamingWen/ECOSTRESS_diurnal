#ECOSTRESS ET model
folderstr="diurnal" #overpass diurnal #calculate ET for ECOSTRESS overpass time or diurnally
obs_str="" #obs or blank if folderstr=='overpass', obs means using original ECOSTRESS LST, blank means using leave-one-out prediction of LST from DTC model
filelist=list.files(paste0('Data/',RegionName, "/Model/",folderstr,"/LST/"),pattern = paste0("^20.*\\",obs_str,".nc"))

ETmodel=function(ncfile,folderstr,obs_str="",UTC_OFFSET,RegionName,makeplot=F,save_var=F){
  print(ncfile)
  LST=raster(paste0('Data/',RegionName, "/Model/",folderstr,"/LST/",ncfile))
  
  #extract time
  local_timestr=strsplit(ncfile,".nc")[[1]]
  tmp0=strsplit(ncfile,paste0(obs_str,".nc"))[[1]]
  tmp=strsplit(tmp0,"_")[[1]]
  local_year=as.integer(tmp[1])
  local_month=as.integer(tmp[2])
  local_date=as.integer(tmp[3])
  local_hour=as.integer(tmp[4])
  local_minute=as.integer(tmp[5])
  local_doy=as.integer(as.Date(paste(local_year,local_month,local_date,sep = "-"))-as.Date(paste0(local_year,"-01-01")))+1
  local_time=as.POSIXct(local_timestr,format="%Y_%m_%d_%H_%M")
  local_doystr=str_pad(local_doy,3,pad="0")
  
  utc_time=local_time-UTC_OFFSET*3600
  tmp=time_decomp(utc_time)
  utc_year=tmp[1]
  utc_month=tmp[2]
  utc_date=tmp[3]
  utc_doy=tmp[4];utc_doystr=str_pad(utc_doy,3,pad='0')
  utc_hour=floor(tmp[5])
  utc_minute=round((tmp[5]-floor(tmp[5]))*60)
  
  ##########################################################
  #met from MERRA-2
  Td=ReadMERRA2_anytime("T2MDEW",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST)-273.15 #deg C
  Ta=ReadMERRA2_anytime("T2M",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST)-273.15 #deg C
  Pres=ReadMERRA2_anytime("PS",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST)/1000 #in kPa
  Spe_hum=ReadMERRA2_anytime("QV2M",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST) #unitless
  
  vpd=Ta;vpd[]=q.to.VPD(q=Spe_hum[],Tair=Ta[],pressure=Pres[])
  rh=Ta;rh[]=VPD.to.rH(VPD=vpd[],Tair=Ta[]);rh[rh>1]=1
  ea=Ta;ea[]=q.to.e(q=Spe_hum[],pressure=Pres[])
  es=ea+vpd
  # summary(ea[]/es[]-rh[])
  
  LWGAB=ReadMERRA2_anytime("LWGAB",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST)
  # SWGNT=ReadMERRA2_anytime("SWGNT",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST)
  # LWGEM=ReadMERRA2_anytime("LWGEM",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST)
  
  Ta_2week=raster(paste0('Data/',RegionName, "/Model/OtherData/Ta_MERRA2_2week/",local_year,local_doystr,".nc"));Ta_2week=projectRaster(Ta_2week,LST)
  vpd_2week=raster(paste0('Data/',RegionName, "/Model/OtherData/vpd_MERRA2_2week/",local_year,local_doystr,".nc"));vpd_2week=projectRaster(vpd_2week,LST)
  rh_2week=raster(paste0('Data/',RegionName, "/Model/OtherData/rh_MERRA2_2week/",local_year,local_doystr,".nc"));rh_2week=projectRaster(rh_2week,LST)
  
  Topt=raster(paste0('Data/',RegionName, "/Model/OtherData/Topt_MERRA2_Landsat.nc")); Topt=projectRaster(Topt,LST)
  
  ##########################################################
  #read Landsat
  ndvi=raster(paste0('Data/',RegionName, "/Model/OtherData/Landsat_NDVI_daily/",local_year,local_doystr,".nc"));ndvi=projectRaster(ndvi,LST)
  fAPARmax=raster(paste0('Data/',RegionName, "/Model/OtherData/fAPARmax_Landsat.nc"));fAPARmax=projectRaster(fAPARmax,LST)
  albedo=raster(paste0('Data/',RegionName, "/Model/OtherData/Landsat_albedo_daily/",local_year,local_doystr,".nc")); albedo=projectRaster(albedo,LST)
  
  ##########################################################
  #parameters
  alpha0=1.26
  delta0=240.97*17.502*es/(Ta+240.97)^2
  
  gamma0=0.066
  kPAR=0.5
  
  savi=0.45*ndvi+0.132
  
  #original
  # fAPAR=1.2*1.136*savi+1.2*(-0.04)
  #modified for us-seg
  fAPAR=1.928*ndvi-0.274
  fAPAR[fAPAR[]<0]=0
  
  fIPAR=ndvi-0.05
  LAI=-log(1-fIPAR)/kPAR
  
  fg=fAPAR/fIPAR;fg[fg[]>1]=1;fg[fg[]<0]=0;fg[fIPAR[]==0]=0
  fM=fAPAR/fAPARmax;fM[fM[]>1]=1
  fwet=rh^4
  
  #previous
  fSM=rh_2week^vpd_2week;fSM[fSM>1]=1
  #modified
  # fSM=0.5*rh_2week^vpd_2week;fSM[fSM>1]=1
  
  fT=exp(-((Ta_2week-Topt)/Topt)^2)

  SWGDN=ReadMERRA2_anytime("SWGDN",utc_year,utc_month,utc_date,utc_hour,utc_minute,LST)
  
  eps_s=Read_eps_s(local_year,local_month,local_date,LST)
  LWu=5.67*10^(-8)*eps_s*LST^4
  # Rn=SWGNT+LWGAB-LWu
  Rn=SWGDN*(1-albedo)+LWGAB-LWu;Rn[Rn<0]=NA
  Rns=Rn*exp(-0.6*LAI)
  Rnc=Rn-Rns
  G=Rn*(0.05+(1-fIPAR)*0.265)
    
  #ET
  ETc=(1-fwet)*fg*fT*fM*alpha0*delta0/(delta0+gamma0)*Rnc
  ETs=(fwet+fSM*(1-fwet))*alpha0*delta0/(delta0+gamma0)*(Rns-G)
  ETi=fwet*alpha0*delta0/(delta0+gamma0)*Rnc
  ET=ETc+ETs+ETi
  writeRaster(ET,paste0('Data/',RegionName, "/Model/",folderstr,"/ET/",local_timestr,".nc"),overwrite=T)
  
  if(makeplot){
    plotfun(albedo,"albedo",local_timestr,folderstr,RegionName)
    plotfun(Ta,"Ta",local_timestr,folderstr,RegionName)
    plotfun(Td,"Td",local_timestr,folderstr,RegionName)
    plotfun(ea,"ea",local_timestr,folderstr,RegionName)
    plotfun(es,"es",local_timestr,folderstr,RegionName)
    plotfun(vpd,"vpd",local_timestr,folderstr,RegionName)
    plotfun(rh,"rh",local_timestr,folderstr,RegionName)
    plotfun(Ta_2week,"Ta_2week",local_timestr,folderstr,RegionName)
    plotfun(vpd_2week,"vpd_2week",local_timestr,folderstr,RegionName)
    plotfun(rh_2week,"rh_2week",local_timestr,folderstr,RegionName)
    plotfun(Topt,"Topt",local_timestr,folderstr,RegionName)
    plotfun(ndvi,"ndvi",local_timestr,folderstr,RegionName)
    plotfun(savi,"savi",local_timestr,folderstr,RegionName)
    plotfun(delta0,"delta0",local_timestr,folderstr,RegionName)
    plotfun(fAPAR,"fAPAR",local_timestr,folderstr,RegionName)
    plotfun(fAPARmax,"fAPARmax",local_timestr,folderstr,RegionName)
    plotfun(fIPAR,"fIPAR",local_timestr,folderstr,RegionName)
    plotfun(LAI,"LAI",local_timestr,folderstr,RegionName)
    plotfun(fg,"fg",local_timestr,folderstr,RegionName)
    plotfun(fM,"fM",local_timestr,folderstr,RegionName)
    plotfun(fwet,"fwet",local_timestr,folderstr,RegionName)
    plotfun(fSM,"fSM",local_timestr,folderstr,RegionName)
    plotfun(fT,"fT",local_timestr,folderstr,RegionName)
    plotfun(LWGAB,"LWGAB",local_timestr,folderstr,RegionName)
    plotfun(SWGDN,"SWGDN",local_timestr,folderstr,RegionName)
    plotfun(LWu,"LWu",local_timestr,folderstr,RegionName)
    plotfun(SWGDN*albedo,"SWGup",local_timestr,folderstr,RegionName)
    plotfun(Rn,"Rn",local_timestr,folderstr,RegionName)
    plotfun(Rns,"Rns",local_timestr,folderstr,RegionName)
    plotfun(Rnc,"Rnc",local_timestr,folderstr,RegionName)
    plotfun(G,"G",local_timestr,folderstr,RegionName)
    plotfun(ETc,"ETc",local_timestr,folderstr,RegionName)
    plotfun(ETs,"ETs",local_timestr,folderstr,RegionName)
    plotfun(ETi,"ETi",local_timestr,folderstr,RegionName)
    plotfun(ET,"ET",local_timestr,folderstr,RegionName)
  }
  
  if (save_var){
    writeRaster(albedo,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/albedo",ncfile),overwrite=T)
    writeRaster(delta0,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/delta",ncfile),overwrite=T)
    writeRaster(Ta,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/Ta",ncfile),overwrite=T)
    writeRaster(Td,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/Td",ncfile),overwrite=T)
    writeRaster(ea,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/ea",ncfile),overwrite=T)
    writeRaster(es,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/es",ncfile),overwrite=T)
    writeRaster(vpd,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/vpd",ncfile),overwrite=T)
    writeRaster(rh,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/rh",ncfile),overwrite=T)
    
    writeRaster(Ta_2week,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/Ta_2week",ncfile),overwrite=T)
    writeRaster(vpd_2week,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/vpd_2week",ncfile),overwrite=T)
    writeRaster(rh_2week,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/rh_2week",ncfile),overwrite=T)
    
    writeRaster(ndvi,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/ndvi",ncfile),overwrite=T)
    writeRaster(savi,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/savi",ncfile),overwrite=T)
    writeRaster(LAI,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/LAI",ncfile),overwrite=T)
    writeRaster(fIPAR,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/fIPAR",ncfile),overwrite=T)
    
    writeRaster(fg,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/fg",ncfile),overwrite=T)
    writeRaster(fM,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/fM",ncfile),overwrite=T)
    writeRaster(fwet,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/fwet",ncfile),overwrite=T)
    writeRaster(fSM,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/fSM",ncfile),overwrite=T)
    writeRaster(fT,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/fT",ncfile),overwrite=T)
    
    # writeRaster(SWGNT,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/SWGNT",ncfile),overwrite=T)
    writeRaster(SWGDN,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/SWGDN",ncfile),overwrite=T)
    writeRaster(SWGDN*albedo,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/SWGup",ncfile),overwrite=T)
    # writeRaster(SWGDN*(1-albedo),filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/SWGNTb",ncfile),overwrite=T)
    writeRaster(LWGAB,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/LWGAB",ncfile),overwrite=T)
    writeRaster(LWu,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/LWu",ncfile),overwrite=T)
    writeRaster(Rn,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/Rn",ncfile),overwrite=T)
    
    
    writeRaster(Rns,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/Rns",ncfile),overwrite=T)
    writeRaster(Rnc,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/Rnc",ncfile),overwrite=T)
    writeRaster(G,filename = paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/G",ncfile),overwrite=T)
    
    writeRaster(ETc,paste0('Data/',RegionName, "/Model/",folderstr,"/ET/ETc",local_timestr,".nc"),overwrite=T)
    writeRaster(ETs,paste0('Data/',RegionName, "/Model/",folderstr,"/ET/ETs",local_timestr,".nc"),overwrite=T)
    writeRaster(ETi,paste0('Data/',RegionName, "/Model/",folderstr,"/ET/ETi",local_timestr,".nc"),overwrite=T)
    writeRaster(ET,paste0('Data/',RegionName, "/Model/",folderstr,"/ET/",local_timestr,".nc"),overwrite=T)
  }
}

#export plot for variables
if(!dir.exists(paste0('Data/',RegionName, "/Model/",folderstr,"/ET/maps_variables/"))){dir.create(paste0('Data/',RegionName, "/Model/",folderstr,"/ET/maps_variables/"))}
if(!dir.exists(paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/"))){dir.create(paste0('Data/',RegionName, "/Model/",folderstr,"/ET/variables/"))}

plotfun=function(ras,varname,timestr,folderstr, RegionName){
  png(file=paste0('Data/',RegionName, "/Model/",folderstr,"/ET/maps_variables/",varname,timestr,".png"),width = 300, height = 300)
  plot(ras)
  title(varname,line=0.5)
  dev.off()
}

#parallel
library(parallel)
cl <- makeCluster(min(30,length(filelist)))
parallel::clusterExport(cl, c("ETmodel","folderstr","lat0","time_decomp","ReadMERRA2_slice","ReadMERRA2_anytime","ReadMERRA2_2week","plotfun","Read_eps_s",'RegionName'), envir=environment())
parallel::clusterEvalQ(cl, c(library(bigleaf),library(raster),library(ncdf4),library(stringr),library(abind)))
data0=parSapply(cl, filelist,ETmodel,folderstr=folderstr,obs_str=obs_str,UTC_OFFSET=UTC_OFFSET, RegionName=RegionName,makeplot=T,save_var=T)
stopCluster(cl) 

