#run ET model for site
###############################################################################
#prepare met data
#calculate daily and 2-week values of Rn vpd Ta, rh, vpd
fluxfolder=list.dirs(paste0("Data/", RegionName,"/Tower"),full.names = T,recursive = F)
fluxfile=list.files(fluxfolder,pattern = 'AMF_.*\\.csv',full.names = T)
Tower_data=read.csv(file=fluxfile,header = T,skip=2,na.strings = -9999)
year=startdate%/%10000
Tower_data=Tower_data[Tower_data$TIMESTAMP_START>=(year*10000+101)*10000 & Tower_data$TIMESTAMP_START<(year*10000+1231+1)*10000,]

if (sitename=='us_seg'){
  Ta_Tower=Tower_data$TA_PI_F #TA TA_PI_F
  rh_Tower=Tower_data$RH_PI_F/100 #RH RH_PI_F convert to 0-1
  vpd_Tower=Tower_data$VPD_PI_F/10 #VPD_PI VPD_PI_F #convert hPa to kPa
  Rn_Tower=Tower_data$NETRAD
}else if(sitename=='us_arm'){
  Ta_Tower=Tower_data$TA_1_1_1
  rh_Tower=Tower_data$RH_1_1_1/100
  Rn_Tower=Tower_data$NETRAD_1_1_1
  vpd_Tower=rH.to.VPD(rh_Tower,Ta_Tower)
}

#Extract tower half-hourly data
timestamp_tower=Tower_data$TIMESTAMP_START+15
tmp=as.character(timestamp_tower)
local_year=as.integer(substr(tmp,1,4))
local_month=as.integer(substr(tmp,5,6))
local_date=as.integer(substr(tmp,7,8))
local_hour=as.integer(substr(tmp,9,10))
local_minute=as.integer(substr(tmp,11,12))
local_doy=as.integer(as.Date(paste(local_year,local_month,local_date,sep = "-"))-as.Date(paste0(local_year,"-01-01")))+1
Tower_HH=data.frame(timestamp_tower,local_year,local_month,local_date,local_hour,local_minute,local_doy,
                    Ta_Tower,rh_Tower,vpd_Tower,Rn_Tower)
write.csv(Tower_HH,file=paste0("Data/", RegionName,"/Tower/Tower_HH.csv"),row.names = F)

#calculate daily average
Tower_daily=NULL
for (doy in 1:365){
  #daily
  tmp=Tower_HH[Tower_HH$local_doy==doy,]
  tmp2=apply(tmp,2,mean,na.rm=T)
  Tower_daily=rbind(Tower_daily,tmp2)
}
Tower_daily=data.frame(Tower_daily)
Tower_daily[,c("timestamp_tower","local_hour","local_minute")]=NULL
write.csv(Tower_daily,file=paste0("Data/", RegionName,"/Tower/Tower_daily.csv"),row.names = F)

#calculate 2-week average
time_mat=Tower_daily[,1:4]
Tower_daily_2week=data.frame(time_mat,apply(Tower_daily[,5:dim(Tower_daily)[2]], 2, FUN=function(x){movingFun(x,n=14,fun=mean,type="to",na.rm=T)}))
savi_2week=read.csv(paste0("Data/", RegionName,"/Model/OtherData/MERRA2_2week.csv"))$savi_2week
phe=savi_2week*Tower_daily_2week$Ta_Tower*Tower_daily_2week$Rn_Tower/Tower_daily_2week$vpd_Tower
Topt_site=Topt_fun(c(phe,Tower_daily_2week$Ta_Tower))
Tower_daily_2week$Topt_site=Topt_site
Tower_daily_2week$fT_site=exp(-((Tower_daily_2week$Ta_Tower-Topt_site)/Topt_site)^2)
write.csv(Tower_daily_2week,file=paste0("Data/", RegionName,"/Tower/Tower_daily_2week.csv"),row.names = F)


###############################################################################
#extract tower data during the test window
fluxfolder=list.dirs("Tower",full.names = T,recursive = F)
fluxfile=list.files(fluxfolder,pattern = 'AMF_.*\\.csv',full.names = T)
Tower_data=read.csv(file=fluxfile,header = T,skip=2,na.strings = -9999)
Tower_data=Tower_data[Tower_data$TIMESTAMP_START>=startdate*10000 & Tower_data$TIMESTAMP_START<(enddate+1)*10000,]

timestamp_tower=Tower_data$TIMESTAMP_START+15
tmp=as.character(timestamp_tower)
local_year=as.integer(substr(tmp,1,4))
local_month=as.integer(substr(tmp,5,6))
local_date=as.integer(substr(tmp,7,8))
local_hour=as.integer(substr(tmp,9,10))
local_minute=as.integer(substr(tmp,11,12))

if (sitename=='us_seg'){
  ET_Tower=Tower_data$LE
  Ta_Tower=Tower_data$TA_PI_F #TA TA_PI_F
  rh_Tower=Tower_data$RH_PI_F/100 #RH RH_PI_F convert to 0-1
  vpd_Tower=Tower_data$VPD_PI_F/10 #VPD_PI VPD_PI_F #convert hPa to kPa
  Rn_Tower=Tower_data$NETRAD
  SW_IN_Tower=Tower_data$SW_IN
  SW_OUT_Tower=Tower_data$SW_OUT
  LW_IN_Tower=Tower_data$LW_IN
  LW_OUT_Tower=Tower_data$LW_OUT
}else if(sitename=='us_arm'){
  ET_Tower=Tower_data$LE_1_1_1
  Ta_Tower=Tower_data$TA_1_1_1
  rh_Tower=Tower_data$RH_1_1_1/100
  Rn_Tower=Tower_data$NETRAD_1_1_1
  vpd_Tower=rH.to.VPD(rh_Tower,Ta_Tower)
  SW_IN_Tower=Tower_data$SW_IN_1_1_1
  SW_OUT_Tower=Tower_data$SW_OUT_1_1_1
  LW_IN_Tower=Tower_data$LW_IN_1_1_1
  LW_OUT_Tower=Tower_data$LW_OUT_1_1_1
}

ea_Tower=VPD.to.e(vpd_Tower,Ta_Tower)
es_Tower=ea_Tower+vpd_Tower

###############################################################################
#Extract other variables' values (produced from ETmodel_region.R) at site location and at given tower timestamps 
#varname - variable name used to name ncfiles in ETmodel_region.R
#dir - directory where ncfiles reside
#timestamp - timestamp at local time (in tower data format)
#lat, lon - site coordinates
#radius - number of nearby row/col for average (0 means 1x1, 1 means 3x3)

Extract_site_var=function(timestamp,varname,dir,lat,lon,radius=1){
  tmp=as.character(timestamp)
  local_year=as.integer(substr(tmp,1,4))
  local_month=as.integer(substr(tmp,5,6))
  local_date=as.integer(substr(tmp,7,8))
  local_hour=as.integer(substr(tmp,9,10))
  local_minute=as.integer(substr(tmp,11,12))
  ncname=paste0(dir,"/",varname,local_year,"_",local_month,"_",local_date,"_",local_hour,"_",local_minute,".nc")
  if (local_minute %in% c(15,45) & local_hour %in% c(5:19) & file.exists(ncname)){
    ras=raster(ncname)
    colnum=colFromX(ras,lon)
    rownum=rowFromY(ras,lat)
    cellnum=cellFromRowColCombine(ras,(rownum-radius):(rownum+radius),(colnum-radius):(colnum+radius))
    val=mean(ras[cellnum],na.rm=T)
  }else{
    val=NA
  }
  return(val)
}

Ta_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="Ta",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
es_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="es",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
rh_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="rh",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
vpd_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="vpd",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
Ta2week_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="Ta_2week",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
rh2week_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="rh_2week",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
vpd2week_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="vpd_2week",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
ndvi=sapply(timestamp_tower,Extract_site_var,varname="ndvi",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
LAI=sapply(timestamp_tower,Extract_site_var,varname="LAI",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
fIPAR=sapply(timestamp_tower,Extract_site_var,varname="fIPAR",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
fg=sapply(timestamp_tower,Extract_site_var,varname="fg",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
fM=sapply(timestamp_tower,Extract_site_var,varname="fM",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
fwet_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="fwet",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
fT_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="fT",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
fSM_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="fSM",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
Rn_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="Rn",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
SW_IN_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="SWGDN",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
SW_OUT_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="SWGup",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
LW_IN_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="LWGAB",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
LW_OUT_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="LWu",dir=paste0("Data/", RegionName,"/Model/diurnal/ET/variables"),lat=lat0,lon=lon0,radius=1)
ET_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="",dir=paste0("Data/", RegionName,"/Model/diurnal/ET"),lat=lat0,lon=lon0,radius=1)
ETc_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="ETc",dir=paste0("Data/", RegionName,"/Model/diurnal/ET"),lat=lat0,lon=lon0,radius=1)
ETs_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="ETs",dir=paste0("Data/", RegionName,"/Model/diurnal/ET"),lat=lat0,lon=lon0,radius=1)
ETi_MERRA2=sapply(timestamp_tower,Extract_site_var,varname="ETi",dir=paste0("Data/", RegionName,"/Model/diurnal/ET"),lat=lat0,lon=lon0,radius=1)


Tower_input=data.frame(timestamp_tower,local_year,local_month,local_date,local_hour,local_minute,
                       ET_Tower,Ta_Tower,es_Tower,rh_Tower,vpd_Tower,
                       Rn_Tower,SW_IN_Tower,SW_OUT_Tower,LW_IN_Tower,LW_OUT_Tower,
                       Ta_MERRA2,Ta2week_MERRA2,es_MERRA2,rh_MERRA2,vpd_MERRA2,rh2week_MERRA2,vpd2week_MERRA2,LAI,fIPAR,ndvi,
                       fg,fM,fwet_MERRA2,fT_MERRA2,fSM_MERRA2,
                       Rn_MERRA2,SW_IN_MERRA2,SW_OUT_MERRA2,LW_IN_MERRA2,LW_OUT_MERRA2,
                       ET_MERRA2,ETc_MERRA2,ETs_MERRA2,ETi_MERRA2)

###############################################################################
#add 2week average
Tower_daily_2week=read.csv("Tower/Tower_daily_2week.csv")
Tower_input$fSM_Tower=NA
Tower_input$Ta2week_Tower=NA
Tower_input$rh2week_Tower=NA
Tower_input$vpd2week_Tower=NA
Tower_input$fT_Tower=NA
for (i in 1:nrow(Tower_input)){
  id=Tower_daily_2week$local_year==Tower_input$local_year[i] & Tower_daily_2week$local_month==Tower_input$local_month[i] & Tower_daily_2week$local_date==Tower_input$local_date[i]
  tmp=Tower_daily_2week[id,]
  Tower_input$Ta2week_Tower[i]=tmp$Ta_Tower
  Tower_input$rh2week_Tower[i]=tmp$rh_Tower
  Tower_input$vpd2week_Tower[i]=tmp$vpd_Tower
  Tower_input$fSM_Tower[i]=tmp$rh_Tower^tmp$vpd_Tower
  Tower_input$fT_Tower[i]=tmp$fT_site
}
Tower_input$fwet_Tower=(Tower_input$rh_Tower)^4
write.csv(Tower_input,file=paste0('Data/',RegionName, "/Tower/Tower_input.csv"),row.names = F)

#############################################################################################
#PT-JPL ET model
#can directly start from here if I have Tower_input.csv ready
Tower_input=read.csv(paste0('Data/',RegionName, "/Tower/Tower_input.csv"))
ETmodel_site=function(Ta,es,rh,LAI,fIPAR,fg,fM,fwet,fSM,fT,Rn=NULL,SW_IN=NA,SW_OUT=NA,LW_IN=NA,LW_OUT=NA){
  alpha0=1.26
  delta0=240.97*17.502*es/(Ta+240.97)^2
  gamma0=0.066
  kPAR=0.5
  
  fwet=rh^4

  if (is.null(Rn)){
    Rn=SW_IN-SW_OUT+LW_IN-LW_OUT
  }
  Rn[Rn<0]=NA
  Rns=Rn*exp(-0.6*LAI)
  Rnc=Rn-Rns
  G=Rn*(0.05+(1-fIPAR)*0.265)
  
  #ET
  ETc=(1-fwet)*fg*fT*fM*alpha0*delta0/(delta0+gamma0)*Rnc
  ETs=(fwet+fSM*(1-fwet))*alpha0*delta0/(delta0+gamma0)*(Rns-G)
  ETi=fwet*alpha0*delta0/(delta0+gamma0)*Rnc
  ET=ETc+ETs+ETi
  
  return(list(ET=ET,ETc=ETc,ETs=ETs,ETi=ETi))
}

#ET estimation using tower met data
Tower_input$Tower_pred=ETmodel_site(Ta=Tower_input$Ta_Tower,es=Tower_input$es_Tower,rh=Tower_input$rh_Tower,LAI=Tower_input$LAI,fIPAR=Tower_input$fIPAR,
                        fg=Tower_input$fg,fM=Tower_input$fM,fwet=Tower_input$fwet_Tower,fSM=Tower_input$fSM_Tower,fT=Tower_input$fT_Tower,
                        Rn=Tower_input$Rn_Tower)$ET

#ET estimation using MERRA-2 met data, but with modified fSM
Tower_input$ET_MERRA2_mod=ETmodel_site(Ta=Tower_input$Ta_MERRA2,es=Tower_input$es_MERRA2,rh=Tower_input$rh_MERRA2,LAI=Tower_input$LAI,fIPAR=Tower_input$fIPAR,
                           fg=Tower_input$fg,fM=Tower_input$fM,fwet=Tower_input$fwet_MERRA2,fSM=Tower_input$fSM_MERRA2*0.5,fT=Tower_input$fT_MERRA2,
                           Rn=Tower_input$Rn_MERRA2)$ET

#ET estimation using tower met data, but with modified fSM
Tower_input$Tower_pred_mod=ETmodel_site(Ta=Tower_input$Ta_Tower,es=Tower_input$es_Tower,rh=Tower_input$rh_Tower,LAI=Tower_input$LAI,fIPAR=Tower_input$fIPAR,
                        fg=Tower_input$fg,fM=Tower_input$fM,fwet=Tower_input$fwet_Tower,fSM=Tower_input$fSM_Tower*0.5,fT=Tower_input$fT_Tower,
                        Rn=Tower_input$Rn_Tower)$ET

write.csv(Tower_input,file=paste0('Data/',RegionName, "/Tower/Tower_input.csv"),row.names = F)

###############################################################################
#extract ECOSTRESS ET for comparison
Extract_val=function(ras,lat,lon,radius=1){
  colnum=colFromX(ras,lon)
  rownum=rowFromY(ras,lat)
  cellnum=cellFromRowColCombine(ras,(rownum-radius):(rownum+radius),(colnum-radius):(colnum+radius))
  val=mean(ras[cellnum],na.rm=T)
  val_sd=sd(ras[cellnum],na.rm=T)
  return(list(val=val,val_sd=val_sd))
}
filelist=list.files(paste0('Data/',RegionName, "/Model/overpass/ET"),pattern="^20.*\\observation.nc")
ECOSTRESS_data=NULL
for (ncfile in filelist){
  tmp=strsplit(ncfile,"observation.nc")[[1]]
  tmp2=strsplit(tmp,"_")[[1]]
  local_year=as.integer(tmp2[1])
  local_month=as.integer(tmp2[2])
  local_date=as.integer(tmp2[3])
  local_hour=as.integer(tmp2[4])
  local_minute=as.integer(tmp2[5])
  
  #original ECOSTRESS ET
  ras=raster(paste0('Data/',RegionName, "/Model/overpass/ET/",ncfile))
  tmp=Extract_val(ras,lat0,lon0,radius=1)
  ETobs=tmp$val
  
  #ET estimation this script with original ECOSTRESS LST
  ras=raster(paste0('Data/',RegionName, "/Model/overpass/ET/",local_year,'_',local_month,'_',local_date,'_',local_hour,'_',local_minute,'obs.nc'))
  tmp=Extract_val(ras,lat0,lon0,radius=1)
  ET_star=tmp$val
  
  ECOSTRESS_data=rbind(ECOSTRESS_data,data.frame(local_year,local_month,local_date,local_hour,local_minute,ETobs,ET_star))
}
write.csv(ECOSTRESS_data,file=paste0('Data/',RegionName, "/Tower/ECOSTRESS_ET.csv"),row.names = F)

###############################################################################
#plot
Tower_data = read.csv(paste0('Data/',RegionName, "/Tower/Tower_input.csv"))
ECOSTRESS_data = read.csv(paste0('Data/',RegionName, "/Tower/ECOSTRESS_ET.csv"))
Tower_time=difftime(as.POSIXlt(paste0(Tower_data$local_year,"-",Tower_data$local_month,"-",Tower_data$local_date," ",Tower_data$local_hour,":",Tower_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
ECOSTRESS_time=difftime(as.POSIXlt(paste0(ECOSTRESS_data$local_year,"-",ECOSTRESS_data$local_month,"-",ECOSTRESS_data$local_date," ",ECOSTRESS_data$local_hour,":",ECOSTRESS_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")

range0=range(c(Tower_data$ET_Tower,Tower_data$ET_MERRA2,Tower_data$ET_MERRA2_mod,Tower_data$Tower_pred_mod,ECOSTRESS_data$ETobs, ECOSTRESS_data$ET_star),na.rm=T)
plot(Tower_time,Tower_data$ET_Tower,type="l",pch=1,col="black",ylim=range0,xlab="",ylab='',main="",axes = F,frame=T)
axis(2)
mtext(expression(paste("ET (W/m"^"2",")")),2,line = 2)
time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+3)
axis(1,at=0:time_intv,labels = F,tck=-0.05)
xtick_loc=seq(0,time_intv,round(time_intv/5))
xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
axis(1,at=xtick_loc,labels = xtick_label)
lines(Tower_time, Tower_data$ET_MERRA2,col="blue")
lines(Tower_time, Tower_data$Tower_pred_mod,col="limegreen")
lines(Tower_time, Tower_data$ET_MERRA2_mod,col="red")
points(ECOSTRESS_time,ECOSTRESS_data$ETobs,pch=20,col="red",cex=1.5)
points(ECOSTRESS_time,ECOSTRESS_data$ET_star,pch=18,col="darkgreen",cex=1.5)

#legend
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F,frame=F)
legend(x = c(0.0, 0.2), y = c(0.2, 1),legend = c("ECOSTRESS ET","ECOSTRESS ET*","Tower ET"),col=c("red","darkgreen","black"),lty=c(NA,NA,1),pch=c(20,18,NA),cex=0.9,ncol = 1,seg.len=2,bty="n")
legend(x = c(0.4, 1), y = c(0.2, 1),legend = c("Constructed ET",expression('Constructed ET (modified f'[SM]*')'),expression('Constructed ET (modified f'[SM]*', tower met)')),col=c("blue","red","limegreen"),lty=c(1,1,1),pch=c(NA,NA,NA),cex=0.9,ncol = 1,seg.len=2,bty="n")
