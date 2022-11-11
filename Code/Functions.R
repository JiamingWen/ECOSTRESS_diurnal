#Functions
########################################################################################
#generate a raster template to standardize the grids of different datasets
raster_template = function(RegionName){
  RegionName = RegionName
  source('Code/Region_period_info.R')
  ETfilenames=list.files(path = paste0('Data/',RegionName, "/Ecostress_ET"),pattern = ".tif",full.names = T)
  et0=raster(ETfilenames[1])
  et0[]=NA
  et_tem=extend(et0,raster(xmn=lon0-offset0,xmx=lon0+offset0,ymn=lat0-offset0,ymx=lat0+offset0))
  return(et_tem)
}

########################################################################################
#decompose time string to values
time_decomp=function(x){
  tmp=strsplit(as.character(x), split=" ")
  Year=as.integer(strsplit(tmp[[1]][1],split="-")[[1]][1])
  month=as.integer(strsplit(tmp[[1]][1],split="-")[[1]][2])
  date=as.integer(strsplit(tmp[[1]][1],split="-")[[1]][3])
  date0=as.Date(tmp[[1]][1])
  DoY=date0-as.Date(paste0(Year,"-01-01"))+1
  if(length(tmp[[1]])==2){
    Hour=as.double(strsplit(tmp[[1]][2],split=":")[[1]][1])+as.double(strsplit(tmp[[1]][2],split=":")[[1]][2])/60+as.double(strsplit(tmp[[1]][2],split=":")[[1]][3])/3600
  }else if(length(tmp[[1]])==1){
    Hour=0
  }
  return(c(Year,month,date,DoY,Hour))
}

########################################################################################
#Interpolate L2 GOES footprints to L3 grids
library(akima)
csv_to_ncdf=function(csvfile,ras,RegionName){
  data=read.csv(paste0('Data/',RegionName, '/GOES_LST/csvfile/',csvfile))
  print(csvfile)
  data=data[data$DQF==0,]
  nrow(data)
  
  # sp2<-ggplot(data, aes(x=lon, y=lat, color=data[,3])) +
  #   geom_point() + xlab("Lon") + ylab("Lat") +
  #   xlim(xmin(et_tem), xmax(et_tem)) +
  #   ylim(ymin(et_tem), ymax(et_tem)) +
  #   scale_colour_gradient(name = names(data)[3]) #low = "green", high = "blue",
  # sp2
  
  ##linear/spline interpolation from irregular grids
  if(nrow(data)>30){
    result=interp(data$lon,data$lat,data$LST,xo=seq(min(data$lon), max(data$lon), by = 0.01),
                  yo=seq(min(data$lat), max(data$lat), by=0.01),linear=T,extrap=F) #at 0.01 approx. 1km resolution
    res_x=abs(result$x[1]-result$x[2])
    res_y=abs(result$y[1]-result$y[2])
    result_ras0=raster(xmn=min(result$x)-res_x/2,xmx=max(result$x)+res_x/2,ymn=min(result$y)-res_y/2,ymx=max(result$y)+res_y/2,res=c(res_x,res_y))
    result_ras0[]=as.vector(result$z)
    result_ras0=flip(result_ras0,direction = 2)
    
    #apply a region mask based on if there is available data within 2km
    dist0=distanceFromPoints(result_ras0,data.frame(data$lon,data$lat))
    result_ras0[dist0[]>2000]=NA
    
    result_ras=projectRaster(result_ras0,et_tem)
  }else{
    result_ras=et_tem;result_ras[]=NA
  }
  
  writeRaster(result_ras,filename = paste0("Data/",RegionName, "/GOES_LST/ncfile/",substr(csvfile,1,nchar(csvfile)-4),".nc"),overwrite=T)
}

########################################################################################
#interpolate GOES LST to anytime (e.g., ECOSTRESS overpass time)
poly_interp=function(x,y,x0,polynum=2){
  data1 = na.omit(data.frame(x,y))
  if (nrow(data1)>polynum){
    lm1 = lm(y ~ poly(x,polynum))
    y0 = predict(lm1,data.frame(x=x0))
    return(y0)
  }else{
    return(rep(NA,length(x0)))
  }
}

ReadGOESLST=function(year0,month0,date0,hour0,minute0, method="linear"){ #in UTC
  monthstr=str_pad(month0,2,pad="0")
  datestr=str_pad(date0,2,pad="0")
  current_time=as.POSIXlt(paste0(year0,"-",monthstr,"-",datestr,"_",hour0,"-",minute0),tz = "GMT",format="%Y-%m-%d_%H-%M")
  
  LSTfilelist=list.files("GOES_LST/ncfile",pattern = ".nc",full.names = F)
  LSTtimelist=as.POSIXlt(unlist(lapply(LSTfilelist,FUN = function(x){substr(x,5,23)})),tz = "GMT",format="%Y-%m-%d_%H-%M-%OS")
  LST_timedif=as.double(difftime(LSTtimelist,current_time,units="mins"))
  
  if (method=="linear"){
    LST_two_bd=which((LST_timedif>-60)&(LST_timedif<=60))
    
    if (length(LST_two_bd)==2){
      LST1=raster(paste0("GOES_LST/ncfile/",LSTfilelist[LST_two_bd[1]]))
      LST2=raster(paste0("GOES_LST/ncfile/",LSTfilelist[LST_two_bd[2]]))
      LST=(LST1*abs(LST_timedif[LST_two_bd[2]])+LST2*abs(LST_timedif[LST_two_bd[1]]))/(abs(LST_timedif[LST_two_bd[2]])+abs(LST_timedif[LST_two_bd[1]]))
      return(LST)
    }else{
      return(NULL)
    }
  }else if (method=="2ndpoly"){
    LST_three_bd=which((LST_timedif>-90)&(LST_timedif<=90))
    if (length(LST_three_bd)==3){
      LST1=raster(paste0("GOES_LST/ncfile/",LSTfilelist[LST_three_bd[1]]))
      LST2=raster(paste0("GOES_LST/ncfile/",LSTfilelist[LST_three_bd[2]]))
      LST3=raster(paste0("GOES_LST/ncfile/",LSTfilelist[LST_three_bd[3]]))
      LSTstack=stack(LST1,LST2,LST3)
      mat=as.matrix(LSTstack)
      # LSTvec=apply(mat, 1, FUN = function(vec){poly_interp(x=LST_timedif[LST_three_bd],y=vec,x0=0)})
      library(parallel)
      cl <- makeCluster(30)
      parallel::clusterExport(cl, c("poly_interp","LST_timedif","LST_three_bd"), envir=environment())
      LSTvec=parApply(cl, mat,1,function(vec){poly_interp(x=LST_timedif[LST_three_bd],y=vec,x0=0)})
      stopCluster(cl)
      LST=LST1;LST[]=LSTvec
      return(LST)
    }else{
      return(NULL)
    }
  }else{
    stop("Method is wrong")
  }
}

########################################################################
#statistical metrics
calRMSE=function(x,y){
  tmp=na.omit(data.frame(x,y))
  x=tmp$x
  y=tmp$y
  error=x-y
  return(sqrt(sum(error^2)/length(error)))
}
calMEF=function(obs,pred){ #the Nash and Sutcliffe model efficiency (MEF)
  1-sum((pred-obs)^2)/sum((obs-mean(obs))^2)
}
calBias=function(obs,pred){
  error=pred-obs
  return(sum(error)/length(error))
}

########################################################################
#heatmap
plot_colorByDensity = function(y,x,
                               xlim=c(min(x,na.rm = T),max(x,na.rm = T)),
                               ylim=c(min(y,na.rm = T),max(y,na.rm = T)),
                               xlab="",ylab="",main="",pch=20,cex=1,axes=T,frame.plot=T) {
  
  df <- data.frame(y,x)
  #df <- na.omit(df)
  x <- densCols(df$y,df$x, colramp=colorRampPalette(c("black", "white")))
  df$dens <- col2rgb(x)[1,] + 1L
  cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
  df$col <- cols[df$dens]
  plot(y~x, data=df[order(df$dens),], 
       ylim=ylim,xlim=xlim,pch=pch,col=col,
       cex=cex,xlab=xlab,ylab=ylab,
       main=main, axes=axes,frame.plot=frame.plot)
} #plot(y~x) or plot(x,y)


########################################################################
#Read MERRA-2 meteorological variables in a region of interest at a given time

#varname - 
# M2T1NXRAD
# SWGDN (surface_incoming_shortwave_flux)
# LWGAB (surface_absorbed_longwave_radiation)
# SWGNT (surface_net_downward_shortwave_flux)
# LWGEM (longwave_flux_emitted_from_surface)

# M2T1NXSLV
# T2M (2-meter_air_temperature K)
# PS (surface_pressure Pa)
# T2MDEW (dew_point_temperature_at_2_m K)
# QV2M (2-meter_specific_humidity kg kg-1)
# TQV (total precipitable water vapor kg m-2)

# M2T1NXLFO
# PARDF (surface_downwelling_par_diffuse_flux)
# PARDR (surface_downwelling_par_beam_flux)

#year0, month0, date0, hour0, minute0 in utc time
#target_ras - a raster template for interpolation

ReadMERRA2_anytime=function(varname,year0,month0,date0,hour0,minute0,target_ras,method="bilinear"){
  utc_time=as.POSIXct(paste0(year0,"-",month0,"-",date0," ",hour0,":",minute0,":00"))
  
  tmp1=time_decomp(utc_time-0.5*3600) #first timestep, id=19 (18:30) in MERRA-2 ncfile (id=1 is 00:30)
  if (hour0==0 & minute0==30){tmp1=time_decomp(utc_time-0.5*3598)}
  utc_year1=as.integer(tmp1[1])
  utc_month1=as.integer(tmp1[2])
  utc_monthstr1=str_pad(utc_month1,2,pad="0")
  utc_date1=as.integer(tmp1[3])
  utc_datestr1=str_pad(utc_date1,2,pad="0")
  utc_id1=ceiling(tmp1[5])
  
  tmp2=time_decomp(utc_time+0.5*3600) #second timestep, id=20 (19:30) in MERRA-2 ncfile (id=1 is 00:30)
  if (hour0==23 & minute0==30){tmp2=time_decomp(utc_time+0.5*3599)}
  utc_year2=as.integer(tmp2[1])
  utc_month2=as.integer(tmp2[2])
  utc_monthstr2=str_pad(utc_month2,2,pad="0")
  utc_date2=as.integer(tmp2[3])
  utc_datestr2=str_pad(utc_date2,2,pad="0")
  utc_id2=ceiling(tmp2[5])
  
  if((utc_year1==2020 & utc_monthstr1=='09')|(utc_year1==2021 & utc_monthstr1 %in% c('06','07','08','09'))){stream1=401}else{stream1=400}
  if((utc_year2==2020 & utc_monthstr2=='09')|(utc_year2==2021 & utc_monthstr2 %in% c('06','07','08','09'))){stream2=401}else{stream2=400}
  
  if (varname %in% c("LWGAB","SWGNT","LWGEM","SWGDN")){
    ncfile1=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/MERRA2/goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXRAD.5.12.4/",utc_year1,"/",utc_monthstr1,"/MERRA2_",stream1,".tavg1_2d_rad_Nx.",utc_year1,utc_monthstr1,utc_datestr1,".nc4.nc4?LWGAB,LWGEM,SWGDN,SWGNT,time,lat,lon")
    ncfile2=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/MERRA2/goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXRAD.5.12.4/",utc_year2,"/",utc_monthstr2,"/MERRA2_",stream2,".tavg1_2d_rad_Nx.",utc_year2,utc_monthstr2,utc_datestr2,".nc4.nc4?LWGAB,LWGEM,SWGDN,SWGNT,time,lat,lon")
  }else if(varname %in% c("T2M","PS","T2MDEW","QV2M","TQV")){
    ncfile1=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/MERRA2/goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXSLV.5.12.4/",utc_year1,"/",utc_monthstr1,"/MERRA2_",stream1,".tavg1_2d_slv_Nx.",utc_year1,utc_monthstr1,utc_datestr1,".nc4.nc4?PS,QV2M,T2M,T2MDEW,TQV,time,lat,lon")
    ncfile2=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/MERRA2/goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXSLV.5.12.4/",utc_year2,"/",utc_monthstr2,"/MERRA2_",stream2,".tavg1_2d_slv_Nx.",utc_year2,utc_monthstr2,utc_datestr2,".nc4.nc4?PS,QV2M,T2M,T2MDEW,TQV,time,lat,lon")
  }else if(varname %in% c("PARDF","PARDR")){
    ncfile1=paste0("/local1/storage/data/MERRA2/goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXLFO.5.12.4/",utc_year1,"/",utc_monthstr1,"/MERRA2_",stream1,".tavg1_2d_lfo_Nx.",utc_year1,utc_monthstr1,utc_datestr1,".nc4.nc4?PARDF,PARDR,time,lat,lon")
    ncfile2=paste0("/local1/storage/data/MERRA2/goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXLFO.5.12.4/",utc_year2,"/",utc_monthstr2,"/MERRA2_",stream2,".tavg1_2d_lfo_Nx.",utc_year2,utc_monthstr2,utc_datestr2,".nc4.nc4?PARDF,PARDR,time,lat,lon")
  }
  
  ras1=ReadMERRA2_slice(ncfile1,varname,utc_id1)
  ras2=ReadMERRA2_slice(ncfile2,varname,utc_id2)
  
  if (minute0>30){
    wt2=minute0-30; wt1=90-minute0
  }else if (minute0<=30){
    wt2=minute0+30; wt1=30-minute0
  }
  
  ras=(ras1*wt1+ras2*wt2)/(wt1+wt2)
  crs(ras)='+proj=longlat +datum=WGS84 +no_defs'
  result=projectRaster(ras,target_ras, method=method)
  return(result)
}

########################################################################
#read hourly slice from a MERRA-2 file
#ncfile - nc file location with full directory
#varname - the variable name
#n - which time step, from 1 to 24, denoting 00:30 to 23:30
#return - a global raster
ReadMERRA2_slice=function(ncfile,varname,n){
  ncin=nc_open(ncfile)
  var=as.vector(ncvar_get(ncin,varname)[,,n])
  lat=ncvar_get(ncin,"lat") # center of the coordinates -90 to 90
  lon=ncvar_get(ncin,"lon") #-180 to 179.375
  ras=raster(xmn=min(lon)-0.625/2,xmx=max(lon)+0.625/2,ymn=min(lat)-0.5/2,ymx=max(lat)+0.5/2,res=c(0.625,0.5))
  ras[]=var
  ras=flip(ras,direction = 2)
  return(ras)
}

########################################################################
#Extract daily average of MERRA-2 Rn, Ta, vpd, rh for days at local time
#year0, doy0 - year and date of time
#UTC_OFFSET - local time zone
#target_ras - raster of the region
library(bigleaf)
ReadMERRA2_daily=function(year0,doy0,UTC_OFFSET,target_ras,method="bilinear"){
  tmp=strsplit(as.character(as.Date(paste0(year0,"-01-01"))+doy0-1),"-")[[1]]
  yearstr=tmp[1];monthstr=tmp[2];datestr=tmp[3]
  utc_timelist=as.POSIXct(paste0(yearstr,"-",monthstr,"-",datestr," ",0:23,":30:00"))-UTC_OFFSET*3600
  tmp=t(sapply(utc_timelist, time_decomp))
  utc_year=tmp[,1]
  utc_month=tmp[,2]
  utc_date=tmp[,3]
  utc_hour=floor(tmp[,5])
  utc_minute=round((tmp[,5]-floor(tmp[,5]))*60)
  
  Rnstack=stack()
  Tastack=stack()
  vpdstack=stack()
  rhstack=stack()
  for (i in 1:length(utc_timelist)){
    print(i)
    LWGAB=ReadMERRA2_anytime("LWGAB",utc_year[i],utc_month[i],utc_date[i],utc_hour[i],utc_minute[i],target_ras,method=method)
    SWGNT=ReadMERRA2_anytime("SWGNT",utc_year[i],utc_month[i],utc_date[i],utc_hour[i],utc_minute[i],target_ras,method=method)
    LWGEM=ReadMERRA2_anytime("LWGEM",utc_year[i],utc_month[i],utc_date[i],utc_hour[i],utc_minute[i],target_ras,method=method)
    Rn=SWGNT+LWGAB-LWGEM
    
    Ta=ReadMERRA2_anytime("T2M",utc_year[i],utc_month[i],utc_date[i],utc_hour[i],utc_minute[i],target_ras,method=method)-273.15 #deg C
    Pres=ReadMERRA2_anytime("PS",utc_year[i],utc_month[i],utc_date[i],utc_hour[i],utc_minute[i],target_ras,method=method)/1000 #in kPa
    Spe_hum=ReadMERRA2_anytime("QV2M",utc_year[i],utc_month[i],utc_date[i],utc_hour[i],utc_minute[i],target_ras,method=method)#unitless
    vpd=Ta;vpd[]=q.to.VPD(q=Spe_hum[],Tair=Ta[],pressure=Pres[])
    rh=Ta;rh[]=VPD.to.rH(VPD=vpd[],Tair=Ta[])
    
    Rnstack=stack(Rnstack,Rn)
    Tastack=stack(Tastack,Ta)
    vpdstack=stack(vpdstack,vpd)
    rhstack=stack(rhstack,rh)
  }
  
  Rn_daily=overlay(Rnstack,fun=mean)
  Ta_daily=overlay(Tastack,fun=mean)
  vpd_daily=overlay(vpdstack,fun=mean)
  rh_daily=overlay(rhstack,fun=mean)
  if (!dir.exists(paste0("Data/", RegionName,"/Model/OtherData/Rn_MERRA2_daily"))){dir.create(paste0("Data/", RegionName,"/Model/OtherData/Rn_MERRA2_daily"))}
  if (!dir.exists(paste0("Data/", RegionName,"/Model/OtherData/Ta_MERRA2_daily"))){dir.create(paste0("Data/", RegionName,"/Model/OtherData/Ta_MERRA2_daily"))}
  if (!dir.exists(paste0("Data/", RegionName,"/Model/OtherData/rh_MERRA2_daily"))){dir.create(paste0("Data/", RegionName,"/Model/OtherData/rh_MERRA2_daily"))}
  if (!dir.exists(paste0("Data/", RegionName,"/Model/OtherData/vpd_MERRA2_daily"))){dir.create(paste0("Data/", RegionName,"/Model/OtherData/vpd_MERRA2_daily"))}
  doystr=str_pad(doy0,3,pad="0")
  writeRaster(Rn_daily,filename = paste0("Data/", RegionName,"/Model/OtherData/Rn_MERRA2_daily/",year0,doystr,".nc"),overwrite=T)
  writeRaster(Ta_daily,filename = paste0("Data/", RegionName,"/Model/OtherData/Ta_MERRA2_daily/",year0,doystr,".nc"),overwrite=T)
  writeRaster(vpd_daily,filename = paste0("Data/", RegionName,"/Model/OtherData/vpd_MERRA2_daily/",year0,doystr,".nc"),overwrite=T)
  writeRaster(rh_daily,filename = paste0("Data/", RegionName,"/Model/OtherData/rh_MERRA2_daily/",year0,doystr,".nc"),overwrite=T)
}


########################################################################
#calculate 2-week forward average of MERRA-2 daily Rn Ta vpd rh extracted from ReadMERRA2_daily
#varname - Rn Ta vpd rh
#year0
#doyrange e.g. 1:365
#may need to modify a little about the first 14 days
ReadMERRA2_2week=function(varname,year0,doyrange=1:365){
  if (!dir.exists(paste0("Data/", RegionName,"/Model/OtherData/",varname,"_MERRA2_2week"))){dir.create(paste0("Data/", RegionName,"/Model/OtherData/",varname,"_MERRA2_2week"))}
  rasstack=stack()
  for (doy in doyrange){
    print(doy)
    doystr=str_pad(doy,3,pad="0")
    rasname=paste0("Data/", RegionName,"/Model/OtherData/",varname,"_MERRA2_daily/",year0,doystr,".nc")
    if (file.exists(rasname)){
      rasdaily=raster(rasname)
    }else{
      warning(paste0("MERRA-2 data not available on ",year0,doyrange))
      rasdaily[]=NA
    }
    rasstack=stack(rasstack,rasdaily)
  }
  
  var_mat=as.matrix(rasstack)
  var_mat_2week=t(apply(var_mat, 1, FUN=function(x){movingFun(x,n=14,fun=mean,type="to",na.rm=T)})) #13min

  for (doy in doyrange){
    doystr=str_pad(doy,3,pad="0")
    ras_result=rasdaily
    ras_result[]=var_mat_2week[,doy]
    writeRaster(ras_result,filename = paste0("Data/", RegionName,"/Model/OtherData/",varname,"_MERRA2_2week/",year,doystr,".nc"),overwrite=T)
  }
}

########################################################################
#get eps_s from the nearest ECOSTRESS overpass; if not available, use the second nearest overpasses
Read_eps_s=function(local_year,local_month,local_date,ras){
  eps_s_list=list.files(paste0('Data/',RegionName, "/Model/OtherData/eps_s"))
  datelist=as.Date(unlist(strsplit(eps_s_list,".nc")),"%Y-%m-%d_%H-%M")
  datedif=abs(as.integer(datelist-as.Date(paste(local_year,local_month,local_date,sep="-"))))
  
  num=1
  eps_s=raster(paste0('Data/',RegionName, "/Model/OtherData/eps_s/",eps_s_list[order(datedif)==num]))
  eps_s=projectRaster(eps_s,ras)
  
  for (i in 2:length(eps_s_list)){
    num=num+1
    # print(sum(is.na(eps_s[])))
    if (sum(is.na(eps_s[]))>0){
      eps_s2=raster(paste0('Data/',RegionName, "/Model/OtherData/eps_s/",eps_s_list[order(datedif)==num]))
      eps_s2=projectRaster(eps_s2,ras)
      eps_s[is.na(eps_s[])]=eps_s2[is.na(eps_s[])]
    }
  }
  return(eps_s)
}


