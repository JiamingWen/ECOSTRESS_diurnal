#MOD07 diurnal interpolation based on ECOSTRESS ET ATBD and Halverson 2018
library(stringr)
library(raster)

#read processed MOD07 Ta/Td files and exported local overpass time
# ncname example: MYD07_L2.A2018219.0920.061.2018219195941Ta.nc
# UTC_OFFSET: local time zone difference to UTC
ReadMOD07name=function(ncname,UTC_OFFSET){
  # sensor=str_sub(ncname,2,2)
  utc_year=as.integer(str_sub(ncname,11,14))
  utc_doy=as.integer(str_sub(ncname,15,17))
  tmp=as.Date(paste0(utc_year,"-01-01"))-1+utc_doy
  tmp2=strsplit(as.character(tmp),"-")[[1]]
  utc_monthstr=tmp2[2]
  utc_datestr=tmp2[3]
  # utc_date
  utc_hour=as.integer(str_sub(ncname,19,20))
  utc_minute=as.integer(str_sub(ncname,21,22))
  utc_time=as.POSIXct(paste0(utc_year,"-",utc_monthstr,"-",utc_datestr," ",utc_hour,":",utc_minute,":00"))
  local_time=utc_time+UTC_OFFSET*3600
  time_decomp(local_time)
}

#read gridded MOD07 nc file and resampled to ras
#if id=2 then merge the image
#export raster stack of two layers: data, local time when data retrieved 
readMOD07file=function(met_var,nclist,overpass_time,id,ras){
  if (length(id)==0){
    stop("MOD07 image is not available")
  }else if(length(id)==1){
    Tras=raster(paste0("MODISmet/",met_var,"/",nclist[id]))
    Tras_proj=projectRaster(Tras,ras)
    tras=Tras_proj;tras[]=overpass_time[id,5]
  }else if(length(id)==2){
    #read each file and merge
    Tras1=raster(paste0("MODISmet/",met_var,"/",nclist[id[1]]))
    Tras_proj1=projectRaster(Tras1,ras)
    tras1=Tras_proj1;tras1[]=overpass_time[id[1],5];tras1[is.na(Tras_proj1[])]=NA
    Tras2=raster(paste0("MODISmet/",met_var,"/",nclist[id[2]]))
    Tras_proj2=projectRaster(Tras2,ras)
    tras2=Tras_proj2;tras2[]=overpass_time[id[2],5];tras2[is.na(Tras_proj2[])]=NA
    Tras_proj=merge(Tras_proj1,Tras_proj2)
    tras=merge(tras1,tras2)
  }else{
    stop(paste0("Number of MOD07 images is ",length(id)))
  }
  return(stack(Tras_proj,tras))
}

#diurnally interpolate MOD07 Ta or Td (in degK) to specific local_t in a area defined by ras - obslete because Josh said they didn't use Aqua
#met_var "Ta" or "Td"
#ras - raster templete
#lat0 - latitude of the location
#local_year - year in local time
#local_doy - doy in local time
#local_t - hours in local time
#UTC_OFFSET: local time zone difference to UTC

MOD07_diurnal=function(met_var,ras,lat0,local_year,local_doy,local_t,UTC_OFFSET){
  # met_var="Ta" #Ta Td
  nclist1=list.files(paste0("MODISmet/",met_var),pattern = paste0('MOD07_L2.A[0-9]{7}.[0-9]{4}.061.[0-9]{13}.nc$'))
  nclist2=list.files(paste0("MODISmet/",met_var),pattern = paste0('MYD07_L2.A[0-9]{7}.[0-9]{4}.061.[0-9]{13}.nc$'))
  nclist=c(nclist1,nclist2)

  # hist(t(sapply(nclist1,ReadMOD07name,UTC_OFFSET=UTC_OFFSET))[,5],breaks=100) #Terra 9-12 20-24
  # summary(t(sapply(nclist1,ReadMOD07name,UTC_OFFSET=UTC_OFFSET))[,5])
  # hist(t(sapply(nclist2,ReadMOD07name,UTC_OFFSET=UTC_OFFSET))[,5],breaks=100) #Aqua 0-4 12-15
  # summary(t(sapply(nclist2,ReadMOD07name,UTC_OFFSET=UTC_OFFSET))[,5])

  overpass_time=t(sapply(nclist,ReadMOD07name,UTC_OFFSET=UTC_OFFSET))
  Tmin_id=which(overpass_time[,1]==local_year & overpass_time[,4]==local_doy & overpass_time[,5]<5)
  Tobs_id=which(overpass_time[,1]==local_year & overpass_time[,4]==local_doy & overpass_time[,5]>9 & overpass_time[,5]<12)
  Tmax_id=which(overpass_time[,1]==local_year & overpass_time[,4]==local_doy & overpass_time[,5]>12 & overpass_time[,5]<15)
  if (length(Tmin_id)*length(Tobs_id)*length(Tmax_id)==0){stop("MOD07 image is not available")}

  tmp=readMOD07file(met_var,nclist,overpass_time,Tmin_id,ras) #Aqua night
  Tmin=tmp[[1]]
  tmin=tmp[[2]]/24

  tmp=readMOD07file(met_var,nclist,overpass_time,Tobs_id,ras) #Terra daily
  Tobs=tmp[[1]]
  tobs=tmp[[2]]/24

  tmp=readMOD07file(met_var,nclist,overpass_time,Tmax_id,ras) #Aqua daily
  Tmax=tmp[[1]]
  tmax=tmp[[2]]/24

  #diurnally interpolated
  gamma1=2*pi*(local_doy-1)/365
  delta1=0.006918-0.39912*cos(gamma1)+0.070257*sin(0.070257)-0.006758*cos(2*gamma1)+0.000907*sin(2*gamma1)-0.00269*cos(3*gamma1)+0.00148*sin(3*gamma1)
  lat_rad=lat0/180*pi
  SHA=acos(-tan(lat_rad)*tan(delta1))*180/pi
  SH=12-SHA/15 #Sunrise hour
  DL=2/15*SHA #Daylight hours

  t_target=local_t/24 #t in hour/24
  HA=(t_target*15-180)*pi/180 #hour angle
  omega1=2*pi*12/DL #wavelength (w) in radians
  phi1=pi/4-2*pi*SH/12 #The phase (j) in radians

  T_target=Tobs+(Tmax-Tmin)/2*(sin(omega1*t_target+phi1)-sin(omega1*tobs+phi1))
}


#################################################################################
# #run for site - need to manually run part of the function MOD07_diurnal first
# Tmaxval=Tmax[cellFromXY(Tmax,c(lon0,lat0))]
# Tminval=Tmin[cellFromXY(Tmin,c(lon0,lat0))]
# Tobsval=Tobs[cellFromXY(Tobs,c(lon0,lat0))]
# tmaxval=tmax[cellFromXY(tmax,c(lon0,lat0))]
# tminval=tmin[cellFromXY(tmin,c(lon0,lat0))]
# tobsval=tobs[cellFromXY(tobs,c(lon0,lat0))]
# 
# gamma1=2*pi*(local_doy-1)/365
# delta1=0.006918-0.39912*cos(gamma1)+0.070257*sin(0.070257)-0.006758*cos(2*gamma1)+0.000907*sin(2*gamma1)-0.00269*cos(3*gamma1)+0.00148*sin(3*gamma1)
# lat_rad=lat0/180*pi
# SHA=acos(-tan(lat_rad)*tan(delta1))*180/pi
# SH=12-SHA/15 #Sunrise hour
# DL=2/15*SHA #Daylight hours
# T_target=rep(NA,49)
# num=0
# for (local_t in seq(0,24,0.5)){
#   num=num+1
#   t_target=local_t/24 #t in hour/24
#   HA=(t_target*15-180)*pi/180 #hour angle
#   omega1=2*pi*12/DL #wavelength (w) in radians
#   phi1=pi/4-2*pi*SH/12 #The phase (j) in radians
#   T_target[num]=Tobsval+(Tmaxval-Tminval)/2*(sin(omega1*t_target+phi1)-sin(omega1*tobsval+phi1))
# }
# plot(seq(0,24,0.5),T_target,type="l",xlab="hour",ylab="T(degK)",ylim=range(c(Tmaxval,Tminval,Tobsval,T_target)))
# points(tmaxval*24,Tmaxval,pch=20,col="red")
# points(tminval*24,Tminval,pch=20,col="red")
# points(tobsval*24,Tobsval,pch=20,col="red")
# abline(v=local_hour+local_minute/60,col="blue")