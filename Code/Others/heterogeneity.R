#check how the model performs in a heterogeneous 2km GOES pixel with mixed 70m ECOSTRESS pixels
library(raster)
library(ncdf4)
# setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")
# lon0=-97.4888;lat0=36.6058;RegionName="us-arm-region2";sitename="us_arm";offset0=0.15;UTC_OFFSET=-6
# setwd(RegionName)
# 
# tmp_str="doy2021168211214"
# filename0=paste0("Ecostress_LST","/","ECO2LSTE.001_SDS_LST_",tmp_str,"_aid0001.tif")
# QAfile=read.csv("Ecostress_LST/ECO2LSTE-001-SDS-QC-lookup.csv")
# QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
# ECOSTRESS=raster(filename0)*0.02
# LST_QC=raster(paste0("Ecostress_LST","/","ECO2LSTE.001_SDS_QC_",tmp_str,"_aid0001.tif"))
# LST_cloudmask=raster(paste0("Ecostress_LST/ECO2CLD.001_SDS_CloudMask_",tmp_str,"_aid0001.tif"))
# ECOSTRESS[!(LST_QC[] %in% QA_good)]=NA
# ECOSTRESS[LST_cloudmask[]!=1]=NA
# 
# GOES=raster("GOES_LST/ncfile/LST_2021-06-17_21-03-54.nc")
# 
# CDL=raster("CDL/CDL_2020_reclassify_new.tif")
# # cdl2=CDL
# cdl2=crop(CDL,raster(xmn=lon0-0.01,xmx=lon0+0.01,ymn=lat0-0.01,ymx=lat0+0.01))
# landtype=c('Others',"Other crops","Corn",'Soybean','Forest','SHR&GRA','URB','WET','Water')
# cdl2=as.factor(cdl2)
# rat <- levels(cdl2)[[1]]
# rat[["landtype"]] <- landtype
# levels(cdl2) <- rat
# palette <- rev(c("#2471A3","#C0392B","#99A3A4","#F9E79F","#52BE80",
#                  "#145A32","#F1C40F","#6E2C00","#76448A"))
# levelplot(cdl2,margin=F,colorkey=T,at=0:6,col.regions=palette,xlab="",ylab="")
# 
# plot(ECOSTRESS,ext=extent(raster(xmn=lon0-0.01,xmx=lon0+0.01,ymn=lat0-0.01,ymx=lat0+0.01)))
# plot(GOES,ext=extent(raster(xmn=lon0-0.01,xmx=lon0+0.01,ymn=lat0-0.01,ymx=lat0+0.01)))
# 
# 
# plot(cdl2,ext=extent(raster(xmn=lon0-0.01,xmx=lon0+0.01,ymn=lat0-0.01,ymx=lat0+0.01)))
# 


################################################################################
# setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")
# lon0=-106.7020;lat0=34.3623;RegionName="us-seg-region4new";sitename="us_seg";offset0=0.15;UTC_OFFSET=-7
# setwd(RegionName)
# 
# # tmp_str="doy2020274211057"
# tmp_str="doy2020271215616" #2020-09-27_14-56 LT
# filename0=paste0("Ecostress_LST","/","ECO2LSTE.001_SDS_LST_",tmp_str,"_aid0001.tif")
# QAfile=read.csv("Ecostress_LST/ECO2LSTE-001-SDS-QC-lookup.csv")
# QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
# ECOSTRESS=raster(filename0)*0.02
# LST_QC=raster(paste0("Ecostress_LST","/","ECO2LSTE.001_SDS_QC_",tmp_str,"_aid0001.tif"))
# LST_cloudmask=raster(paste0("Ecostress_LST/ECO2CLD.001_SDS_CloudMask_",tmp_str,"_aid0001.tif"))
# ECOSTRESS[!(LST_QC[] %in% QA_good)]=NA
# ECOSTRESS[LST_cloudmask[]!=1 & LST_cloudmask[]!=33]=NA
# 
# ECOSTRESS_pred=raster("Model/overpass/LST/2020_9_27_14_56.nc")
# 
# # GOES=raster("GOES_LST/ncfile/LST_2020-09-30_14-03-53.nc")
# GOES=raster("GOES_LST/ncfile/LST_2020-09-27_21-03-54.nc")
# 
# library(lattice)
# library(rasterVis)
# cdl2=raster(paste0("CDL/CDL_2020_reclassify.tif"))
# landtype=c('Others','Cropland','Forest','Shrubland & Grassland','Urban','Wetland','Water')
# cdl2=as.factor(cdl2)
# rat <- levels(cdl2)[[1]]
# rat[["landtype"]] <- landtype
# levels(cdl2) <- rat
# palette <- rev(c("#4FFFFF","#75B5EE","#FF1300","#E8E87A","#5dba5c",
#                  "#BF56C3","#82342E"))
# levelplot(cdl2,margin=F,colorkey=T,at=0:6,col.regions=palette,xlab="",ylab="")
# 
# ndvi=raster("Model/OtherData/Landsat_NDVI_daily/2020271.nc")
# 
# # lon1=-106.6; lat1=34.35; radius=0.02 #forest
# lon1=-106.8; lat1=34.40; radius=0.03 #water
# ECOSTRESS_cp=crop(ECOSTRESS,raster(xmn=lon1-radius,xmx=lon1+radius,ymn=lat1-radius,ymx=lat1+radius,res=radius))
# ECOSTRESS_pred_cp=crop(ECOSTRESS_pred,ECOSTRESS_cp)
# GOES_cp=crop(GOES,ECOSTRESS_cp)
# ndvi_cp=crop(ndvi,ECOSTRESS_cp)
# 
# range0=range(c(ECOSTRESS_cp[],ECOSTRESS_pred_cp[],GOES_cp[]),na.rm=T)
# plot(GOES_cp,zlim=range0,col=terrain.colors(255),main="GOES LST")
# plot(ECOSTRESS_cp,zlim=range0,col=terrain.colors(255),main="ECOSTRESS LST")
# plot(ECOSTRESS_pred_cp,zlim=range0,col=terrain.colors(255),main="ECOSTRESS LOO predict")
# plot(ndvi_cp,main="ndvi")
# 
# cdl2_cp=crop(cdl2,ECOSTRESS_cp)
# levelplot(cdl2_cp,margin=F,colorkey=T,at=0:6,col.regions=palette,xlab="",ylab="")
# reset_par()
# 
# # cdl3=raster(ext=extent(cdl2),res=res(cdl2))
# # cdl3[]=cdl2[]
# # plot(cdl3,ext=extent(ECOSTRESS_cp))




############################################################################
#whole region
#ECOSTRESS constructed diurnal cycle for different land covers
setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")
lon0=-106.7020;lat0=34.3623;RegionName="us-seg-region4new";sitename="us_seg";offset0=0.15;UTC_OFFSET=-7
setwd(RegionName)

cdl2=raster(paste0("CDL/CDL_2020_reclassify.tif"))
landtype=c('Others','CRO','Forest','SHR&GRA','URB','WET','Water')
year0=2020
month0=9
date0=27

for (varname in c("LST","ET")){
  result=NULL
  for (hour0 in 6:17){
    for (minute0 in c(15,45)){
      result0=data.frame(year=year0,month=month0,date=date0,hour=hour0,minute=minute0)
      ras=raster(paste0("Model/diurnal/",varname,"/",year0,"_",month0,"_",date0,"_",hour0,"_",minute0,".nc"))
      ras=projectRaster(ras,cdl2)
      for (lc in 0:6){
        val=mean(ras[cdl2[]==lc],na.rm=T)
        val_sd=sd(ras[cdl2[]==lc],na.rm=T)
        tmp=data.frame(val,val_sd)
        colnames(tmp)=c(landtype[lc+1],paste0(landtype[lc+1],"_sd"))
        result0=cbind(result0,tmp)
      }
      result=rbind(result,result0)
    }
  }
  write.csv(result,file=paste0("revision/diurnal",varname,"_LCs_20200927.csv"),row.names = F)
}

#####################################################################################
jpeg("/local/workdir/jw2495/ECOSTRESS/DTC/Figures/AGU/diurnal_LC.jpg", width = 2000, height = 920,quality = 100,res=300)
op=par() #original par
par(mfrow=c(1,2),mar=c(1,0.5,1,0.5),mai=c(0.7,0.7,0.2,0.2))

#LST
varname="LST" #LST ET
result=read.csv(paste0("revision/diurnal",varname,"_LCs_20200927.csv"))
time0=result$hour+result$minute/60
plot(time0,result$SHR.GRA-273.15,type="l",col="#d4af37",lwd=2,xlab="",ylab="",
     ylim=range(c(result$`SHR.GRA`,result$Forest,result$WET,result$CRO,result$URB,result$Water)-273.15,na.rm=T))
mtext("Hour",1,line=2)
mtext("LST (\u00B0C)",2,line=2)
lines(time0,result$Forest-273.15,col="#5dba5c",lwd=2)
lines(time0,result$URB-273.15,col="#FF1300",lwd=2)
lines(time0,result$Water-273.15,col="#4FFFFF",lwd=2)
lines(time0,result$WET-273.15,col="#75B5EE",lwd=2)
lines(time0,result$CRO-273.15,col="#BF56C3",lwd=2)
legend(9,27,rev(c('Cropland','Forest','Shrubland & Grassland','Urban','Wetland','Water')),
       col=c("#4FFFFF","#75B5EE","#FF1300","#d4af37","#5dba5c","#BF56C3"),lty=1,lwd=2,bty="n",cex=0.7)

#ET
varname="ET" #LST ET
result=read.csv(paste0("revision/diurnal",varname,"_LCs_20200927.csv"))
time0=result$hour+result$minute/60
plot(time0,result$SHR.GRA,type="l",col="#d4af37",lwd=2,xlab="",ylab="",
     ylim=range(c(result$`SHR.GRA`,result$Forest,result$WET,result$CRO,result$URB,result$Water),na.rm=T))
mtext("Hour",1,line=2)
mtext(expression(paste("ET (W/m"^"2",")")),2,line=2)
lines(time0,result$Forest,col="#5dba5c",lwd=2)
lines(time0,result$URB,col="#FF1300",lwd=2)
lines(time0,result$Water,col="#4FFFFF",lwd=2)
lines(time0,result$WET,col="#75B5EE",lwd=2)
lines(time0,result$CRO,col="#BF56C3",lwd=2)

par(op) #restore par
dev.off()

####################################################################################
#identify relatively pure GOES pixels
setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")
lon0=-106.7020;lat0=34.3623;RegionName="us-seg-region4new";sitename="us_seg";offset0=0.15;UTC_OFFSET=-7
setwd(RegionName)

cdl2=raster(paste0("CDL/CDL_2020_reclassify.tif"))
landtype=c('Others','CRO','Forest','SHR&GRA','URB','WET','Water')
csvfile=read.csv("GOES_LST/csvfile/LST_2020-09-27_13-03-53.csv")
result=NULL
for (i in 1:nrow(csvfile)){
  print(i)
  lat1=csvfile[i,"lat"]
  lon1=csvfile[i,"lon"]
  offset=0.01
  if (lon1-offset>xmin(cdl2) & lon1+offset<xmax(cdl2) & lat1-offset>ymin(cdl2) & lat1+offset<ymax(cdl2)){
    cdl2_cp=crop(cdl2,raster(xmn=lon1-offset,xmx=lon1+offset,ymn=lat1-offset,ymx=lat1+offset))
    result0=data.frame(lat=lat1,lon=lon1)
    for (lc in 0:6){
      frac=sum(cdl2_cp[]==lc)/sum(!is.na(cdl2_cp[]))
      tmp=data.frame(frac)
      colnames(tmp)=paste0("Type",lc)
      result0=cbind(result0,tmp)
    }
    result=rbind(result,result0)
  }
}
write.csv(result,file="revision/GOES_LCfrac.csv",row.names = F)


#############################################################################
#focus only one GOES pixel
result=read.csv("revision/GOES_LCfrac.csv")
tmp=(result$lat-34.40)^2+(result$lon-(-106.8))^2
lat1=result[which(tmp==min(tmp)),1] #34.39493
lon1=result[which(tmp==min(tmp)),2] #-106.8062
radius=0.01
tmp=raster(xmn=lon1-radius,xmx=lon1+radius,ymn=lat1-radius,ymx=lat1+radius,res=radius)

library(lattice)
library(rasterVis)
cdl2=raster(paste0("CDL/CDL_2020_reclassify.tif"))
landtype=c('Others','Cropland','Forest','Shrubland & Grassland','Urban','Wetland','Water')
cdl2=as.factor(cdl2)
rat <- levels(cdl2)[[1]]
rat[["landtype"]] <- landtype
levels(cdl2) <- rat
palette <- rev(c("#4FFFFF","#75B5EE","#FF1300","#E8E87A","#5dba5c",
                 "#BF56C3","#82342E"))
# levelplot(cdl2,margin=F,colorkey=T,at=0:6,col.regions=palette,xlab="",ylab="")
cdl2_cp=crop(cdl2,tmp)
levelplot(cdl2_cp,margin=F,colorkey=T,at=0:6,col.regions=palette,xlab="",ylab="")
reset_par()

##################################################
#extract diurnal data
#constructed ECOSTRESS
year0=2020
month0=9
date0=30
result=NULL
for (hour0 in 6:20){
  for (minute0 in c(15,45)){
    result0=data.frame(year=year0,month=month0,date=date0,hour=hour0,minute=minute0)
    LST=raster(paste0("Model/diurnal/LST/",year0,"_",month0,"_",date0,"_",hour0,"_",minute0,".nc"))
    LST=projectRaster(LST,cdl2_cp)
    for (lc in 0:6){
      val=mean(LST[cdl2_cp[]==lc],na.rm=T)
      val_sd=sd(LST[cdl2_cp[]==lc],na.rm=T)
      tmp=data.frame(val,val_sd)
      colnames(tmp)=c(landtype[lc+1],paste0(landtype[lc+1],"_sd"))
      result0=cbind(result0,tmp)
    }
    result=rbind(result,result0)
  }
}
write.csv(result,file="revision/diurnalLST_LCs_20200930_singlepixel.csv",row.names = F)
time0=result$hour+result$minute/60
plot(time0,result$`Shrubland & Grassland`-273.15,type="l",col="darkgoldenrod",xlab="", ylab="",xlim=c(6,18))
mtext("Hour",1,line=2)
mtext("LST (\u00B0C)",2,line=2)
lines(time0,result$Wetland-273.15,col="#75B5EE")

#ECOSTRESS obs
tmp_str="doy2020274211057" #2020-09-30 14:10
filename0=paste0("Ecostress_LST","/","ECO2LSTE.001_SDS_LST_",tmp_str,"_aid0001.tif")
QAfile=read.csv("Ecostress_LST/ECO2LSTE-001-SDS-QC-lookup.csv")
QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
ECOSTRESS=raster(filename0)*0.02
LST_QC=raster(paste0("Ecostress_LST","/","ECO2LSTE.001_SDS_QC_",tmp_str,"_aid0001.tif"))
LST_cloudmask=raster(paste0("Ecostress_LST/ECO2CLD.001_SDS_CloudMask_",tmp_str,"_aid0001.tif"))
ECOSTRESS[!(LST_QC[] %in% QA_good)]=NA
ECOSTRESS[LST_cloudmask[]!=1 & LST_cloudmask[]!=33]=NA
ECOSTRESS=projectRaster(ECOSTRESS,cdl2_cp)
ECOSTRESS_cp=crop(ECOSTRESS,cdl2_cp)
LSTcol=colorRampPalette(c("blue", "cyan","yellow", "red"))(255)
plot(ECOSTRESS_cp-273.15,col=LSTcol,main="",ext=extent(cdl2_cp))
points(14+10/60,mean(ECOSTRESS[cdl2_cp[]==3]-273.15,na.rm=T),pch=8,col="darkgoldenrod") #SHR/GRA
points(14+10/60,mean(ECOSTRESS[cdl2_cp[]==5]-273.15,na.rm=T),pch=8,col="#75B5EE") #WET

#GOES
filelist=list.files("GOES_LST/csvfile/",full.names = T)[110:124]
filelist
GOES_LST=NULL
for (filename0 in filelist){
  csvfile=read.csv(filename0)
  csvfile$LST[csvfile$DQF!=0]=NA
  GOES_LST0=csvfile[csvfile$lat==lat1 & csvfile$lon==lon1 ,"LST"]
  print(GOES_LST0)
  GOES_LST=c(GOES_LST,GOES_LST0)
}
points(6:20,GOES_LST-273.15,col="blue",pch=20)
legend("bottomright",c("Constructed LST (Shrubland & Grassland)", "Constructed LST (Wetland)",
                       "ECOSTRESS LST (Shrubland & Grassland)", "ECOSTRESS LST (Wetland)",
                       "GOES LST (Shrubland & Grassland)"),
       col=c("darkgoldenrod","#75B5EE","darkgoldenrod","#75B5EE","blue"),
       lty=c(1,1,NA,NA,NA),
       pch=c(NA,NA,8,8,20),bty="n",cex=0.6)