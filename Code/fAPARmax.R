#calculate fAPARmax
year=startdate%/%10000
fAPARstack=stack()
for (doy in 1:365){
  doystr=str_pad(doy,3,pad="0")

  #Landsat
  ndvi=raster(paste0('Data/',RegionName, "/Model/OtherData/Landsat_NDVI_daily/",year,doystr,".nc"))
  
  #original
  # savi=0.45*ndvi+0.132
  # fAPAR=1.2*1.136*savi+1.2*(-0.04)
  #modified
  fAPAR=1.928*ndvi-0.274
  
  fAPARstack=stack(fAPARstack,fAPAR)
}
fAPARmat=as.matrix(fAPARstack)
fAPARmaxvec=apply(fAPARmat, 1, max,na.rm=T)
fAPARmaxvec[is.infinite(fAPARmaxvec)]=NA
fAPARmax=fAPARstack[[1]]
fAPARmax[]=fAPARmaxvec
fAPARmax2=projectRaster(fAPARmax,et_tem)
writeRaster(fAPARmax2,filename=paste0('Data/',RegionName, "/Model/OtherData/fAPARmax_Landsat.nc"),overwrite=T)
