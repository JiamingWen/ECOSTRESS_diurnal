#check the LST and ET prediction residual vs local clouds
setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")
lon0=-106.7020;lat0=34.3623;UTC_OFFSET=-7
fluxfolder=list.dirs("us-seg-region5new/Tower",full.names = T,recursive=F)
fluxfile=list.files(fluxfolder,pattern = 'AMF_.*\\.csv',full.names = T)
Tower_data0=read.csv(file=fluxfile,header = T,skip=2,na.strings = -9999)

RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226

RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108

RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279

result_ts1=read.csv(paste0(RegionName,"/Tower/DTC_ts.csv"))
Tower_data=Tower_data0[Tower_data0$TIMESTAMP_START>=startdate*10000 & Tower_data0$TIMESTAMP_START<(enddate+1)*10000,]

if (nrow(result_ts1)!=nrow(Tower_data)){
  stop("Check the data")
}

local_time=as.character(Tower_data$TIMESTAMP_START)
local_year=as.integer(substr(local_time,1,4))
local_month=as.integer(substr(local_time,5,6))
local_date=as.integer(substr(local_time,7,8))
local_hour=as.integer(substr(local_time,9,10))
local_minute=as.integer(substr(local_time,11,12))+15

#precipitation
precip=Tower_data$P_PI_F

#clear day SW_IN
clear_day=NULL
for (hour in 0:23){
  for (minute in c(15,45)){
    SW_IN=max(Tower_data$SW_IN[local_hour==hour & local_minute==minute],na.rm=T)
    clear_day=rbind(clear_day,data.frame(hour,minute,SW_IN))
  }
}

#cloud fraction
# cloud_fraction=rep(NA,nrow(Tower_data))
# for (i in 1:nrow(Tower_data)){
#   cloud_fraction[i]=1-Tower_data$SW_IN[i]/clear_day$SW_IN[clear_day$hour==local_hour[i] & clear_day$minute==local_minute[i]]
# }
nDays=as.integer(as.Date(enddatestr)-as.Date(startdatestr))+1
SW_IN_pot=rep(clear_day$SW_IN, nDays)
cloud_fraction=1-Tower_data$SW_IN/SW_IN_pot
cloud_fraction[Tower_data$SW_IN<2]=NA #for some values at sunset when SW_IN=0

result=data.frame(local_year,local_month,local_date,local_hour,local_minute,SW_IN=Tower_data$SW_IN,SW_IN_pot,cloud_fraction,precip)
write.csv(result,file=paste0(RegionName,"/Tower/cloud.csv"),row.names = F)

#exclude rainy days
if (RegionName=="us-seg-region2new"){
  Tower_data[local_year==2018 & local_month==8 & local_date %in% c(10,11),]=NA
  result_ts1[local_year==2018 & local_month==8 & local_date %in% c(10,11),"Tower_ts"]=NA
}else if (RegionName=="us-seg-region5new"){
  Tower_data[local_year==2020 & local_month==4 & local_date==13,]=NA
  result_ts1[local_year==2020 & local_month==4 & local_date==13,"Tower_ts"]=NA
}
range0=range(c(result_ts1$Tower_ts,result_ts1$Eco_ts),na.rm=T)
plot(result_ts1$Tower_ts,result_ts1$Eco_ts,pch=20,xlab="Tower LST", ylab="ECOSTRESS LST",xlim=range0,ylim=range0)


#plot
par(mfrow=c(3,1),mai = c(0.3, 0.9, 0.2, 0.5),oma=c(0,0,0,0))
source("time_decomp.R")
#panel 1 - LST
library(suncalc)
sunrise=time_decomp(getSunlightTimes(as.Date(startdatestr),lat0,lon0)$sunrise+UTC_OFFSET*3600)
t_sr=sunrise[5]
starttime=as.POSIXlt(paste0(startdatestr," ",floor(t_sr),":",(t_sr-floor(t_sr))*60))
GOES_tower=na.omit(read.csv(paste0(RegionName,"/Tower/GOES_tower.csv")))
GOES_tower$local_time=as.POSIXlt(paste0(GOES_tower$local_year,"-",GOES_tower$local_month,"-",GOES_tower$local_date," ",GOES_tower$local_hour,":",GOES_tower$local_minute))
GOES_tower$t0_GOES=as.numeric(difftime(GOES_tower$local_time,starttime,units="hour"))
t0_GOES=GOES_tower$t0_GOES[GOES_tower$t0_GOES>=0]
temps0_GOES=GOES_tower$GOES_LST_tower[GOES_tower$t0_GOES>=0]-273.15
Tower_time=difftime(as.POSIXct(paste0(result_ts1$local_year,"-",result_ts1$local_month,"-",result_ts1$local_date," ",result_ts1$local_hour,":",result_ts1$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
range0=range(c(temps0_GOES,result_ts1$GOES_ts,result_ts1$Eco_ts,result_ts1$Tower_ts),na.rm=T)
plot(Tower_time,result_ts1$Tower_ts,type="l",pch=1,col="black",lwd=2,ylim=range0+c(0,2),xlab="",ylab="",axes = F,frame=T)
axis(2)
mtext("LST (\u00B0C)",2,line=2)
time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
axis(1,at=0:time_intv,labels = F,tck=-0.05)
xtick_loc=seq(0,time_intv,round(time_intv/5))
xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
axis(1,at=xtick_loc,labels = xtick_label)
points((t0_GOES+t_sr)/24, temps0_GOES,pch=20,col="deepskyblue",cex=1)
lines(Tower_time, result_ts1$Eco_ts,col="red")

#panel 2 - cloud fraction
plot(Tower_time,cloud_fraction,type="l",pch=1,col="black",lwd=2,xlab="",ylab="",ylim=c(0,1),axes = F,frame=T)
axis(2)
mtext("Cloud Fraction",2,line=2)
time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
axis(1,at=0:time_intv,labels = F,tck=-0.05)
xtick_loc=seq(0,time_intv,round(time_intv/5))
xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
axis(1,at=xtick_loc,labels = xtick_label)
abline(h = 0.2,lty=2,col="red")

#panel 3 - precip
plot(NULL,lwd=2,xlab="",ylab="",xlim=range(Tower_time),ylim=range(c(precip,1)),axes = F,frame=T)
segments(Tower_time,rep(0,length(precip)),Tower_time,precip,col="dodgerblue")
axis(2)
mtext("Precipitation (mm)",2,line=2)
time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
axis(1,at=0:time_intv,labels = F,tck=-0.05)
xtick_loc=seq(0,time_intv,round(time_intv/5))
xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
axis(1,at=xtick_loc,labels = xtick_label)

par(mfrow=c(1,1),mai = c(1.02, 0.82, 0.82, 0.42),oma=c(0,0,0,0))

##############################################################################
#statistics
#residual dependance on cloud fraction
residual=result_ts1$Eco_ts-result_ts1$Tower_ts
data=data.frame(residual,cloud_fraction)
# data=data[data$cloud_fraction!=1,]
plot(residual)
plot(data$cloud_fraction,data$residual)

calRMSE=function(x,y){
  tmp=na.omit(data.frame(x,y))
  x=tmp$x
  y=tmp$y
  error=x-y
  return(sqrt(sum(error^2)/length(error)))
}
calBias=function(obs,pred){
  tmp=na.omit(data.frame(obs,pred))
  obs=tmp$obs
  pred=tmp$pred
  error=pred-obs
  return(sum(error)/length(error))
}

threshold=0.2
sum(cloud_fraction>threshold,na.rm=T)
sum(cloud_fraction<=threshold,na.rm=T)
calRMSE(result_ts1$Tower_ts[cloud_fraction>threshold],result_ts1$Eco_ts[cloud_fraction>threshold])
calRMSE(result_ts1$Tower_ts[cloud_fraction<=threshold],result_ts1$Eco_ts[cloud_fraction<=threshold])
calBias(result_ts1$Tower_ts[cloud_fraction>threshold],result_ts1$Eco_ts[cloud_fraction>threshold])
calBias(result_ts1$Tower_ts[cloud_fraction<=threshold],result_ts1$Eco_ts[cloud_fraction<=threshold])