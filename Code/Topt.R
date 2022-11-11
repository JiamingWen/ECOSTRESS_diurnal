#calculate phenology and Topt
Topt_fun=function(vec){
  n=length(vec)
  pheo=vec[1:(n/2)]
  Ta=vec[(n/2+1):n]
  if(sum(!is.na(pheo))>0){
    return(Ta[which(pheo[]==max(pheo,na.rm=T))[1]])
  }else{
    return(NA)
  }
}

year=startdate%/%10000 #local_year
Tastack=stack()
vpdstack=stack()
savistack=stack()
Rnstack=stack()
phestack=stack()

for (doy in 1:365){ #local_doy
  print(doy)
  doystr=str_pad(doy,3,pad="0")
  
  Ta=raster(paste0('Data/',RegionName, "/Model/OtherData/Ta_MERRA2_2week/",year,doystr,".nc"))
  vpd=raster(paste0('Data/',RegionName, "/Model/OtherData/vpd_MERRA2_2week/",year,doystr,".nc"))
  
  Rn=raster(paste0('Data/',RegionName, "/Model/OtherData/Rn_MERRA2_2week/",year,doystr,".nc"))
  savi=raster(paste0('Data/',RegionName, "/Model/OtherData/Landsat_savi_2week/",year,doystr,".nc"))
  
  Tastack=stack(Tastack,Ta)
  vpdstack=stack(vpdstack,vpd)
  savistack=stack(savistack,savi)
  Rnstack=stack(Rnstack,Rn)
  phestack=stack(phestack,Rn*Ta*savi/vpd)
}

mat=as.matrix(stack(phestack,Tastack))
result=apply(mat,1,Topt_fun)
Topt=Tastack[[1]]
Topt[]=result
writeRaster(Topt,filename=paste0('Data/',RegionName, "/Model/OtherData/Topt_MERRA2_Landsat.nc"),overwrite=T)

#extract values at site
savi_2week=as.double(savistack[cellFromXY(savistack,c(lon0,lat0))])
Rn_2week=as.double(Rnstack[cellFromXY(Rnstack,c(lon0,lat0))])
vpd_2week=as.double(vpdstack[cellFromXY(vpdstack,c(lon0,lat0))])
Ta_2week=as.double(Tastack[cellFromXY(Tastack,c(lon0,lat0))])
phe_2week=as.double(phestack[cellFromXY(phestack,c(lon0,lat0))])

plot(Rn_2week,pch=20,main="Rn (2week-average)",ylab="",xlab="doy")
plot(Ta_2week,pch=20,main="Ta (2week-average)",ylab="",xlab="doy")
plot(vpd_2week,pch=20,main="vpd (2week-average)",ylab="",xlab="doy")
plot(savi_2week,pch=20,main="savi (2week-average)",ylab="",xlab="doy")
plot(phe_2week,pch=20,main="phenology",ylab="",xlab="doy")

write.csv(data.frame(year=rep(startdate%/%10000,365),doy=1:365,savi_2week,Rn_2week,vpd_2week,Ta_2week,phe_2week),paste0('Data/',RegionName, "/Model/OtherData/MERRA2_2week.csv"),row.names = F)
