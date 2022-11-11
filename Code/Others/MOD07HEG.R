#select MOD07 files in this region
library(rgdal)
library(gdalUtils)
library(raster)
library(MODIS)
library(ncdf4)
library(stringr)

source("../MOD07_diurnal.R")

#this function creates parameter file and run HEG tool (variable:"Retrieved_Temperature_Profile","Retrieved_Moisture_Profile","Surface_Pressure")
#hdfname - hdf filename in full directory
#ras - target raster

ftemplate_prm <-  function (hdfname,ras){
  varlist=list("Water_Vapor","Surface_Pressure","Retrieved_Moisture_Profile","Retrieved_Temperature_Profile")
  bandnumlist=list(1,1,paste(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,sep=":"),paste(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,sep=":"))
  outdir=paste0(getwd(),"/MODISmet");if(!dir.exists(outdir)){dir.create(outdir)}
  for(n in 1:length(varlist)){
    if (!dir.exists(paste0(outdir,"/",varlist[n]))){dir.create(paste0(outdir,"/",varlist[n]))}
  }
  tmp=strsplit(hdfname,"/")[[1]]
  tmp2=tmp[length(tmp)]
  outfile=str_sub(tmp2,1,(nchar(tmp2)-4))
  
  fileconn =  file(paste0(outdir,'/',outfile,'.prm'),'w')
  total_runs  = length(varlist)
  writeLines('', fileconn)
  writeLines(paste0('NUM_RUNS = ',total_runs), fileconn)
  writeLines("", fileconn)
  
  for(n in 1:total_runs)
  {
    writeLines('BEGIN', fileconn)
    writeLines(paste0('INPUT_FILENAME = ',hdfname), fileconn)
    writeLines("OBJECT_NAME = mod07", fileconn)
    writeLines(paste0('FIELD_NAME = ',varlist[n],'|'), fileconn) # field name
    writeLines(paste0('BAND_NUMBER = ',bandnumlist[n]), fileconn) # Band number
    writeLines(paste0("OUTPUT_PIXEL_SIZE_X = ", 0.05), fileconn) #res(ras)[1]
    writeLines(paste0("OUTPUT_PIXEL_SIZE_Y = ", 0.05), fileconn) #res(ras)[2]
    writeLines(paste('SPATIAL_SUBSET_UL_CORNER = (',ymax(ras)+0.1,xmin(ras)-0.1,')'), fileconn) # UL conner #add 0.1 deg edge make sure the region is fully covered
    writeLines(paste('SPATIAL_SUBSET_LR_CORNER = (',ymin(ras)-0.1,xmax(ras)+0.1,')'), fileconn) # LR conner
    writeLines("RESAMPLING_TYPE = NN", fileconn)
    writeLines("OUTPUT_PROJECTION_TYPE = GEO", fileconn)
    writeLines("ELLIPSOID_CODE = WGS84", fileconn)
    writeLines("OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0  )", fileconn)
    writeLines(paste0('OUTPUT_FILENAME = ',outdir,"/",varlist[n],"/",outfile,'.tif'), fileconn)
    writeLines("", fileconn)
    writeLines("OUTPUT_TYPE = GEO", fileconn)
    writeLines("END", fileconn)
    writeLines("",fileconn)
    writeLines("",fileconn)
  }
  close(fileconn)
}

runHEG=function(paramname){
  Sys.setenv(MRTDATADIR='/local/workdir/jw2495/HEG/heg/data')
  Sys.setenv(PGSHOME = '/local/workdir/jw2495/HEG/heg/TOOLKIT_MTD')
  # Sys.setenv(HEGUSER="wen")
  Sys.setenv(MRTBINDIR="/local/workdir/jw2495/HEG/heg/bin")
  system(paste0("/local/workdir/jw2495/HEG/heg/bin/swtif -P ",paramname))
}

#This function is to calculate Ta and Td from temperature/mositure profile and surface pressure, following Famiglietti et al., 2018
#vec - a vector of length 21, the value of 20 profiles from high to low plus surface pressure
calTair=function(vec){
  Temp_profile=as.numeric(vec[1:20])
  Pres_profile=c(5, 10, 20, 30, 50, 70, 100, 150, 200, 250, 
                 300, 400, 500, 620, 700, 780, 850, 920, 950, 1000)
  Pres_s=vec[21]
  tmp=na.omit(data.frame(Pres_profile,Temp_profile))
  if (nrow(tmp)>1 & !is.na(Pres_s)){
    Tlower=tmp[nrow(tmp),2]
    Plower=tmp[nrow(tmp),1]
    Tupper=tmp[nrow(tmp)-1,2]
    Pupper=tmp[nrow(tmp)-1,1]
    Zlower=287.053/9.8*Tlower*log(Pres_s/Plower)
    Zupper=287.053/9.8*Tupper*log(Plower/Pupper)
    T_s=Tlower+(Tlower-Tupper)*Zlower/Zupper
  }else{
    T_s=NA
  }
  return(T_s)
}

# year, doy
# sensor - "Terra" "Aqua" "both"
# ras - raster for the target region
# variable - met variables that need to be calculated
# makeplot - T or F to export plots

MOD07HEG=function(year,doy,sensor="both",ras,variable=c("Ta","Td","ea","es","vpd","rh","TPW","eps_a"),makeplot=F){
  yearstr=as.character(year)
  doystr=str_pad(doy,3,pad="0")
  
  hdffolder1=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/MOD07/MOD07_L2/",yearstr,"/",doystr)
  hdflist1=list.files(hdffolder1,full.names = T)
  hdffolder2=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/MOD07/MYD07_L2/",yearstr,"/",doystr)
  hdflist2=list.files(hdffolder2,full.names = T)
  if (sensor=="both"){
    hdflist=c(hdflist1,hdflist2)
  }else if (sensor=="Terra"){
    hdflist=hdflist1
  }else if (sensor=="Aqua"){
    hdflist=hdflist2
  }else{
    stop("sensor input is wrong")
  }
  
  for (hdfname in hdflist){
    one <- getSds(hdfname)
    #lat lon
    lat=raster(readGDAL(one$SDS4gdal[29], as.is = TRUE))
    lon=raster(readGDAL(one$SDS4gdal[30], as.is = TRUE))
    cellid=cellFromXY(ras,data.frame(lon[],lat[]))
    if (sum(!is.na(cellid))>0){
      ftemplate_prm(hdfname,ras)
      
      tmp=strsplit(hdfname,"/")[[1]]
      tmp2=tmp[length(tmp)]
      filenamestr=str_sub(tmp2,1,(nchar(tmp2)-4))
      paramname=paste0(getwd(),'/MODISmet/',filenamestr,'.prm')
      runHEG(paramname)
      file.remove(paramname)
      
      #calculate Ta and Td
      Retrieved_Temperature_Profile0 <- stack(paste0("MODISmet/Retrieved_Temperature_Profile/",filenamestr,".tif"))
      NAvalue(Retrieved_Temperature_Profile0)=-32768
      Retrieved_Temperature_Profile = (Retrieved_Temperature_Profile0+15000)*0.009999999776482582
      # plot(Retrieved_Temperature_Profile[[20]])
      Retrieved_Moisture_Profile0 <- stack(paste0("MODISmet/Retrieved_Moisture_Profile/",filenamestr,".tif"))
      NAvalue(Retrieved_Moisture_Profile0)=-32768
      Retrieved_Moisture_Profile = (Retrieved_Moisture_Profile0+15000)*0.009999999776482582
      Surface_Pressure <- raster(paste0("MODISmet/Surface_Pressure/",filenamestr,".tif"))*0.1000000014901161
      Water_Vapor <- raster(paste0("MODISmet/Water_Vapor/",filenamestr,".tif"))*0.001000000047497451
      
      mat_a=as.data.frame(stack(Retrieved_Temperature_Profile,Surface_Pressure))
      mat_d=as.data.frame(stack(Retrieved_Moisture_Profile,Surface_Pressure))
      T_a=apply(mat_a, 1, calTair)
      T_d=apply(mat_d, 1, calTair)
      T_a_ras=Surface_Pressure;T_a_ras[]=T_a-273.15 #degC
      T_d_ras=Surface_Pressure;T_d_ras[]=T_d-273.15 #degC
      
      ea=0.613753*exp(17.27*T_d_ras/(T_d_ras+237.3)) #kPa
      es=0.61121*exp(17.502*T_a_ras/(240.97+T_a_ras)) #kPa
      vpd=es-ea
      rh=ea/es
      
      eps_a=1-(1+Water_Vapor)*exp(-(1.2+3*Water_Vapor)^0.5)
      
      for (varname in variable){
        if (!dir.exists(paste0("MODISmet/",varname))){dir.create(paste0("MODISmet/",varname))}
      }
      if ("Ta" %in% variable){writeRaster(T_a_ras,filename = paste0("MODISmet/Ta/",filenamestr,".nc"),overwrite=T)}
      if ("Td" %in% variable){writeRaster(T_d_ras,filename = paste0("MODISmet/Td/",filenamestr,".nc"),overwrite=T)}
      if ("ea" %in% variable){writeRaster(ea,filename = paste0("MODISmet/ea/",filenamestr,".nc"),overwrite=T)}
      if ("es" %in% variable){writeRaster(es,filename = paste0("MODISmet/es/",filenamestr,".nc"),overwrite=T)}
      if ("vpd" %in% variable){writeRaster(vpd,filename = paste0("MODISmet/vpd/",filenamestr,".nc"),overwrite=T)}
      if ("rh" %in% variable){writeRaster(rh,filename = paste0("MODISmet/rh/",filenamestr,".nc"),overwrite=T)}
      if ("TPW" %in% variable){writeRaster(Water_Vapor,filename = paste0("MODISmet/TPW/",filenamestr,".nc"),overwrite=T)}
      if ("eps_a" %in% variable){writeRaster(eps_a,filename = paste0("MODISmet/eps_a/",filenamestr,".nc"),overwrite=T)}
      
      #plot
      if (makeplot){
        if (!dir.exists("MODISmet/maps")){dir.create("MODISmet/maps")}
        png(file=paste0("MODISmet/maps/",filenamestr,"Ta",".png"),height=300,width=300)
        plot(T_a_ras)
        dev.off()
        png(file=paste0("MODISmet/maps/",filenamestr,"Td",".png"),height=300,width=300)
        plot(T_d_ras)
        dev.off()
      }

    }
  }
}

#create a dataframe (year, doy) for MOD07HEG input
doy_df=function(startdate,enddate){
  num=0
  datenum=as.integer(enddate-startdate)
  for (dnum in 0:datenum){
    num=num+1
    current_date=startdate+dnum
    year0=as.integer(strsplit(as.character(current_date),"-")[[1]][1])
    doy0=as.integer(current_date-as.Date(paste0(year0,"-01-01"))+1)
    if (num==1){
      df=data.frame(year=year0,doy=doy0)
    }else{
      df=rbind(df,data.frame(year=year0,doy=doy0))
    }
  }
  return(df)
}



library(parallel)
cl <- makeCluster(30)
parallel::clusterExport(cl, c("ftemplate_prm","runHEG","calTair","MOD07HEG","et_tem"), envir=environment())
parallel::clusterEvalQ(cl, c(library(raster),library(rgdal),library(gdalUtils),library(MODIS),library(ncdf4),library(stringr)))
#Tair and Td for ET - Aqua and Terra - only startdate to enddate
# parApply(cl, doy_df(as.Date(startdatestr)-1,as.Date(enddatestr)+1),1,function(vec){MOD07HEG(year=vec[1],doy=vec[2],sensor="both",ras=et_tem,makeplot=T)})
#Tair and Td for Topt - Aqua and Terra - whole year
parApply(cl, doy_df(as.Date("2018-01-01"),as.Date("2018-12-31")),1,function(vec){MOD07HEG(year=vec[1],doy=vec[2],sensor="both",ras=et_tem,makeplot=F)})
stopCluster(cl)

###############################################################################################
#extract MERRA-2 diurnal cycle of Ta, Td; 
year=2018
MERRA2_diurnal=NULL
for (utc_doy in 1:365){
  print(utc_doy)
  tmp=strsplit(as.character(as.Date(paste0(year,"-01-01"))+utc_doy-1),"-")[[1]]
  utc_year=tmp[1]
  utc_monthstr=tmp[2]
  utc_datestr=tmp[3]
  
  ncfile=paste0("/local/workdir/jw2495/ECOSTRESS/DTC/MERRA2/goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2T1NXSLV.5.12.4/",utc_year,"/",utc_monthstr,"/MERRA2_400.tavg1_2d_slv_Nx.",utc_year,utc_monthstr,utc_datestr,".nc4.nc4?PS,QV2M,T2M,T2MDEW,TQV,time,lat,lon")
  ncin=nc_open(ncfile)
  lat=ncvar_get(ncin,"lat") # center of the coordinates -90 to 90
  lon=ncvar_get(ncin,"lon") #-180 to 179.375
  latid=which(abs(lat-lat0)==min(abs(lat-lat0)))
  lonid=which(abs(lon-lon0)==min(abs(lon-lon0)))
  
  Ta_arr=ncvar_get(ncin,"T2M")-273.15 #degC
  Td_arr=ncvar_get(ncin,"T2MDEW")-273.15 #degC
  Ta=Ta_arr[lonid,latid,]
  Td=Td_arr[lonid,latid,]
  ea=0.613753*exp(17.27*Td/(Td+237.3)) #kPa
  es=0.61121*exp(17.502*Ta/(240.97+Ta)) #kPa
  vpd=es-ea
  rh=ea/es
  
  utc_time=as.POSIXct(paste0(utc_year,"-",utc_monthstr,"-",utc_datestr," ",0:23,":30:00"))
  local_time=utc_time+UTC_OFFSET*3600
  local_time_mat=data.frame(t(sapply(local_time,time_decomp)))
  colnames(local_time_mat)=c("local_year","local_month","local_date","local_doy","local_hour")
  MERRA2_diurnal=rbind(MERRA2_diurnal,data.frame(local_time_mat,Ta,Td,ea,es,vpd,rh))
}
write.csv(MERRA2_diurnal,"Model/OtherData/MERRA2_diurnal.csv",row.names = F)

#############################################################################################
# interpolate MODIS Ta Td using MERRA2_diurnal.csv to same hourly interval by adding offset
library(zoo)
exportNAras=function(local_year,local_doystr,et_tem){
  for (j in 1:24){
    ras=et_tem
    ras[]=NA
    local_hourstr=str_pad(j-1,2,pad="0")
    filename0=paste0("Model/OtherData/",varname,"_MODIS_diurnal/",local_year,local_doystr,local_hourstr,"30.nc")
    writeRaster(ras,filename=filename0,overwrite=T)
  }
}
local_year=2018
varname="Td"
MERRA2_diurnal=read.csv("Model/OtherData/MERRA2_diurnal.csv")
if (!dir.exists(paste0("Model/OtherData/",varname,"_MODIS_diurnal"))){dir.create(paste0("Model/OtherData/",varname,"_MODIS_diurnal"))}
for (local_doy in 1:365){
  print(local_doy)
  local_doystr=str_pad(local_doy,3,pad="0")
  nclist=list.files(paste0("MODISmet/",varname),pattern = paste0('MOD07_L2.A[0-9]{7}.[0-9]{4}.061.[0-9]{13}.nc$'))
  overpass_time=t(sapply(nclist,ReadMOD07name,UTC_OFFSET=UTC_OFFSET))
  id=which(overpass_time[,1]==local_year & overpass_time[,4]==local_doy & overpass_time[,5]>9 & overpass_time[,5]<12)
  
  if (length(id)>0){
    for (i in 1:length(id)){
      varras=raster(paste0("MODISmet/",varname,"/",nclist[id[i]]))
      varras=projectRaster(varras,et_tem)
      
      tmp=MERRA2_diurnal[MERRA2_diurnal$local_year== local_year & MERRA2_diurnal$local_doy==local_doy,]
      if (nrow(tmp)==24){
        val_MERRA2=na.approx(tmp[,varname],tmp[,"local_hour"],xout=local_hour)
        for (j in 1:24){
          MERRA2ras=et_tem
          MERRA2ras[]=tmp[j,varname]
          MODISras=MERRA2ras-val_MERRA2+varras
          local_hourstr=str_pad(j-1,2,pad="0")
          filename0=paste0("Model/OtherData/",varname,"_MODIS_diurnal/",local_year,local_doystr,local_hourstr,"30.nc")
          if (!file.exists(filename0)){
            writeRaster(MODISras,filename=filename0)
          }else{
            print("more than 1 MODIS file")
            exist_ras=raster(filename0)
            MODISras=merge(MODISras,exist_ras,tolerance=0.5)
            writeRaster(MODISras,filename=filename0,overwrite=T)
          }
        }
      }else{exportNAras(local_year,local_doystr,et_tem)}
    }
  }else{exportNAras(local_year,local_doystr,et_tem)}
}

#calculate ea es, vpd rh
if (!dir.exists(paste0("Model/OtherData/","vpd","_MODIS_diurnal"))){dir.create(paste0("Model/OtherData/","vpd","_MODIS_diurnal"))}
if (!dir.exists(paste0("Model/OtherData/","rh","_MODIS_diurnal"))){dir.create(paste0("Model/OtherData/","rh","_MODIS_diurnal"))}
nclist=list.files("Model/OtherData/Ta_MODIS_diurnal/")
for (ncfile in nclist){
  Ta=raster(paste0("Model/OtherData/","Ta","_MODIS_diurnal/",ncfile))
  Td=raster(paste0("Model/OtherData/","Td","_MODIS_diurnal/",ncfile))
  ea=0.613753*exp(17.27*Td/(Td+237.3)) #kPa
  es=0.61121*exp(17.502*Ta/(240.97+Ta)) #kPa
  vpd=es-ea
  rh=ea/es;rh[rh>1]=1
  writeRaster(vpd,filename=paste0("Model/OtherData/","vpd","_MODIS_diurnal/",ncfile),overwrite=T)
  writeRaster(rh,filename=paste0("Model/OtherData/","rh","_MODIS_diurnal/",ncfile),overwrite=T)
}

#aggregate to daily
MODIS_daily=function(varname,local_year,local_doyrange=1:365){
  if (!dir.exists(paste0("Model/OtherData/",varname,"_MODIS_daily"))){dir.create(paste0("Model/OtherData/",varname,"_MODIS_daily"))}
  for (local_doy in 1:365){
    print(local_doy)
    local_doystr=str_pad(local_doy,3,pad="0")
    nclist=list.files(paste0("Model/OtherData/",varname,"_MODIS_diurnal/"),pattern=paste0("^",local_year,local_doystr),full.names = T)
    result=mean(stack(nclist))
    writeRaster(result,paste0("Model/OtherData/",varname,"_MODIS_daily/",local_year,local_doystr,".nc"),overwrite=T)
  }
}

variable=c("Ta","vpd","rh")

library(parallel)
cl <- makeCluster(3)
parallel::clusterExport(cl, c("MODIS_daily","variable"), envir=environment())
parallel::clusterEvalQ(cl, c(library(raster),library(ncdf4),library(stringr)))
parSapply(cl, variable,MODIS_daily,local_year=2018)
stopCluster(cl)

#2week forward average
#calculate 2-week forward average from MODIS met daily files from MODIS_daily
#varname - Ta vpd rh
#year0
#doyrange e.g. 1:365
MODIS_2week=function(varname,local_year,local_doyrange=1:365){
  if (!dir.exists(paste0("Model/OtherData/",varname,"_MODIS_2week"))){dir.create(paste0("Model/OtherData/",varname,"_MODIS_2week"))}
  rasstack=stack()
  for (doy in local_doyrange){
    print(doy)
    doystr=str_pad(doy,3,pad="0")
    rasname=paste0("Model/OtherData/",varname,"_MODIS_daily/",local_year,doystr,".nc")
    if (file.exists(rasname)){
      rasdaily=raster(rasname)
    }else{
      warning(paste0("MODIS data not available on ",local_year,doystr))
      rasdaily[]=NA
    }
    rasstack=stack(rasstack,rasdaily)
  }
  
  var_mat=as.matrix(rasstack)
  var_mat_2week=t(apply(var_mat, 1, FUN=function(x){movingFun(x,n=14,fun=mean,type="to",na.rm=T)})) #13min
  # plot(var_mat_2week[113527,],type="l",main=varname)
  for (doy in local_doyrange){
    doystr=str_pad(doy,3,pad="0")
    ras_result=rasdaily
    ras_result[]=var_mat_2week[,doy]
    writeRaster(ras_result,filename = paste0("Model/OtherData/",varname,"_MODIS_2week/",local_year,doystr,".nc"),overwrite=T)
  }
}

variable=c("Ta","vpd","rh")
library(parallel)
cl <- makeCluster(3)
parallel::clusterExport(cl, c("MODIS_2week","variable"), envir=environment())
parallel::clusterEvalQ(cl, c(library(raster),library(ncdf4),library(stringr)))
parSapply(cl, variable,function(x){MODIS_2week(varname=x,local_year=2018,local_doyrange=1:365)})
stopCluster(cl)


# #two-week forward average for Aqua daily
# library(stringr)
# MODISmet_2week=function(varname,year,et_tem,UTC_OFFSET){
#   print(varname)
#   if (!dir.exists(paste0("MODISmet/",varname,"_daily"))){dir.create(paste0("MODISmet/",varname,"_daily"))}
#   if (!dir.exists(paste0("MODISmet/",varname,"_2week"))){dir.create(paste0("MODISmet/",varname,"_2week"))}
#   rasstack=stack()
#   for (doy in 1:365){
#     print(doy)
#     doystr=str_pad(doy,3,pad="0")
#     local_time=as.POSIXct(paste0(as.character(as.Date(paste0(year,"-01-01"))-1+doy)," 13:30:00"))
#     utc_time=local_time-UTC_OFFSET*3600
#     tmp=time_decomp(utc_time)
#     utc_year=tmp[1]
#     utc_doy=tmp[4];utc_doystr=str_pad(utc_doy,3,pad='0')
#     
#     nclist=list.files(paste0("MODISmet/",varname),pattern = paste0('MYD07_L2.A[0-9]{7}.[0-9]{4}.061.[0-9]{13}.nc$'))
#     overpass_time=t(sapply(nclist,ReadMOD07name,UTC_OFFSET=UTC_OFFSET))
#     id=which(overpass_time[,1]==utc_year & overpass_time[,4]==utc_doy & overpass_time[,5]>12 & overpass_time[,5]<15)
#     if (length(id)==0 | length(id)>2){
#       varras=et_tem
#     }else if (length(id)==1){
#       varras=raster(paste0("MODISmet/",varname,"/",nclist[id]))
#     }else if (length(id)==2){
#       varras=merge(raster(paste0("MODISmet/",varname,"/",nclist[id[1]])),raster(paste0("MODISmet/",varname,"/",nclist[id[2]])),tolerance=0.5)
#     }
#     varras=projectRaster(varras,et_tem)
#     writeRaster(varras,filename = paste0("MODISmet/",varname,"_daily/",year,doystr,".nc"),overwrite=T)
#     if (sum(is.na(varras[]))>0){varras[]=NA} #if there is missing value in the raster, discard the raster, not use it for two-week average to prevent abrupt edges
#     rasstack=stack(rasstack,varras)
#   }
#   
#   var_mat=as.matrix(rasstack)
#   var_mat_2week=t(apply(var_mat, 1, FUN=function(x){movingFun(x,n=14,fun=mean,type="to",na.rm=T)})) #13min
#   plot(var_mat_2week[113527,],type="l",main=varname)
#   for (doy in 1:365){
#     doystr=str_pad(doy,3,pad="0")
#     ras_result=et_tem
#     ras_result[]=var_mat_2week[,doy]
#     writeRaster(ras_result,filename = paste0("MODISmet/",varname,"_2week/",year,doystr,".nc"),overwrite=T)
#   }
# }
# 
# variable=c("Ta","Td","ea","es","vpd","rh","TPW","eps_a")
# 
# library(parallel)
# cl <- makeCluster(8)
# parallel::clusterExport(cl, c("time_decomp","ReadMOD07name","et_tem"), envir=environment())
# parallel::clusterEvalQ(cl, c(library(raster),library(ncdf4),library(stringr)))
# parSapply(cl, variable,MODISmet_2week,year=year,et_tem=et_tem,UTC_OFFSET=UTC_OFFSET)
# stopCluster(cl)
