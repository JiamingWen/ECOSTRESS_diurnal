#Compile individual GOES LST files altogether
filelist=list.files(paste0("Data/",RegionName,"/GOES_LST/csvfile"))

data=read.csv(paste0("Data/",RegionName,"/GOES_LST/csvfile/",filelist[1]))
GOES_LST_val=data.frame(lon=data$lon,lat=data$lat)
GOES_LST_time=NULL

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
    print(filename0)
    data=read.csv(paste0("Data/",RegionName,"/GOES_LST/csvfile/",filename0))
    data$LST[data$DQF!=0]=NA
    data$LST=data$LST-273.15
    if (sum(data$lat!=GOES_LST_val$lat)+sum(data$lon!=GOES_LST_val$lon)==0){
      GOES_LST_val=data.frame(GOES_LST_val,data$LST)
      if (is.null(GOES_LST_time)){
        GOES_LST_time=data.frame(time=c(local_year,local_month,local_date,local_hour,local_minute,fit_time))
      }else{
        GOES_LST_time=data.frame(GOES_LST_time,data.frame(time=c(local_year,local_month,local_date,local_hour,local_minute,fit_time)))
      }
    }else{
      stop("lat/lon don't match!")
    }
  }
}
write.csv(GOES_LST_val,file=paste0("Data/",RegionName,"/GOES_LST/DTC_Parameters/GOES_LST_val.csv"),row.names = F)
write.csv(GOES_LST_time,file=paste0("Data/",RegionName,"/GOES_LST/DTC_Parameters/GOES_LST_time.csv"),row.names = F)