#interpolate Landsat NDVI and albedo to daily
variable="NDVI";bandnum=1
# variable="albedo";bandnum=2 #2: corrected; 3: in Harverson 2018, likely incorrect due to typos

if (!dir.exists(paste0('Data/',RegionName, "/Model/OtherData/Landsat_",variable,"_daily"))){dir.create(paste0('Data/',RegionName, "/Model/OtherData/Landsat_",variable,"_daily"))}
filelist=list.files("Landsat",pattern=".tif")
datestrlist=sapply(filelist,function(x){str_sub(x,13,20)})
year=startdate%/%10000

rasstack=stack()
for (doy in 1:365){
  print(doy)
  tmp=strsplit(as.character(as.Date(paste0(year,"-01-01"))-1+doy),"-")[[1]]
  yearstr=tmp[1]
  monthstr=tmp[2]
  datestr=tmp[3]
  
  if (paste0(yearstr,monthstr,datestr) %in% datestrlist){
    ras=raster(paste0('Data/',RegionName, "/Landsat/",filelist[which(datestrlist==paste0(yearstr,monthstr,datestr))]),band=bandnum)
    ras=projectRaster(ras,et_tem)
  }else{
    ras=et_tem
  }
  rasstack=stack(rasstack,ras)
}

mat=as.matrix(rasstack)
mat_gapfilled=t(apply(mat, 1, FUN=function(x){na.approx(x,na.rm=F,maxgap=25)}))

for (doy in 1:365){
  doystr=str_pad(doy,3,pad="0")
  ras_result=et_tem
  ras_result[]=mat_gapfilled[,doy]
  writeRaster(ras_result,filename = paste0('Data/',RegionName, "/Model/OtherData/Landsat_",variable,"_daily/",year,doystr,".nc"),overwrite=T)
}


######################################################################
#2-week forward average
if (!dir.exists(paste0('Data/',RegionName, "/Model/OtherData/Landsat_savi_daily"))){dir.create(paste0('Data/',RegionName, "/Model/OtherData/Landsat_savi_daily"))}
if (!dir.exists(paste0('Data/',RegionName, "/OtherData/Landsat_savi_2week"))){dir.create(paste0('Data/',RegionName, "/Model/OtherData/Landsat_savi_2week"))}
year=startdate%/%10000 #local_year
ndvistack=stack()
savistack=stack()

for (doy in 1:365){ #local_doy
  print(doy)
  doystr=str_pad(doy,3,pad="0")

  ndvi0=raster(paste0('Data/',RegionName, "/Model/OtherData/Landsat_NDVI_daily/",year,doystr,".nc"))
  savi0=0.45*ndvi0+0.132
  ndvi=projectRaster(ndvi0,et_tem)
  savi=projectRaster(savi0,et_tem)
  writeRaster(savi,filename = paste0('Data/',RegionName, "/Model/OtherData/Landsat_savi_daily/",year,doystr,".nc"),overwrite=T)

  ndvistack=stack(ndvistack,ndvi)
  savistack=stack(savistack,savi)
}

#at tower site
ndvi_daily=data.frame(year,doy=1:365,ndvi=as.vector(ndvistack[cellFromXY(ndvistack,c(lon0,lat0))]))
write.csv(ndvi_daily,file=paste0('Data/',RegionName, "/Model/OtherData/ndvi_daily.csv"),row.names = F)

savi_mat=as.matrix(savistack)
savi_mat_2week=t(apply(savi_mat, 1, FUN=function(x){movingFun(x,n=14,fun=mean,type="to",na.rm=T)})) #13min

for (doy in 1:365){
  doystr=str_pad(doy,3,pad="0")
  ras_savi=et_tem
  ras_savi[]=savi_mat_2week[,doy]
  writeRaster(ras_savi,filename = paste0('Data/',RegionName, "/Model/OtherData/Landsat_savi_2week/",year,doystr,".nc"),overwrite=T)
}