###################################################################################
#checking DTC_region results
library(stringr)
if(!dir.exists(paste0("Data/", RegionName,"/Model/overpass/LST/maps"))){dir.create(paste0("Data/", RegionName,"/Model/overpass/LST/maps"))}
QAfile=read.csv(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE-001-SDS-QC-lookup.csv"))
# summary(QAfile$Mandatory.QA.flags)
QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
filelist=list.files(paste0("Data/", RegionName,"/Ecostress_LST/"),pattern = "^ECO2LSTE.001_SDS_LST_doy")
num=0
for (i in 1:length(filelist)){
  filename0=filelist[i]
  tmp=strsplit(filename0,"_")[[1]]
  utc_year=as.integer(substr(tmp[4],4,7))
  doy0=as.integer(substr(tmp[4],8,10))
  tmp2=strsplit(as.character(as.Date(paste0(utc_year,"-01-01"))+doy0-1),split = "-")[[1]]
  utc_month=as.integer(tmp2[2])
  utc_date=as.integer(tmp2[3])
  utc_hour=as.integer(substr(tmp[4],11,12))
  utc_minute=as.integer(substr(tmp[4],13,14))
  local_time=as.POSIXlt(paste0(as.Date(doy0-1,origin=paste0(utc_year,"-01-01"))," ",utc_hour,":",utc_minute,":00"))+UTC_OFFSET*3600
  time_tmp=time_decomp(local_time)
  local_year=time_tmp[1]
  local_month=time_tmp[2]
  local_date=time_tmp[3]
  local_hour=floor(time_tmp[5])
  local_minute=round((time_tmp[5]-floor(time_tmp[5]))*60)
  timestamp=local_year*10000+local_month*100+local_date
  fit_time=as.numeric(difftime(local_time,starttime,units="hour"))
  if (timestamp>=startdate & timestamp<=enddate & fit_time>=0){
    LST=raster(paste0("Data/", RegionName,"/Ecostress_LST/",filename0))*0.02
    LST_QC=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2LSTE.001_SDS_QC_",tmp[4],"_aid0001.tif"))
    LST_cloudmask=raster(paste0("Data/", RegionName,"/Ecostress_LST/ECO2CLD.001_SDS_CloudMask_",tmp[4],"_aid0001.tif"))
    LST[!(LST_QC[] %in% QA_good)]=NA
    LST[LST_cloudmask[]!=1 & LST_cloudmask[]!=33]=NA
    if (sum(!is.na(LST[]))!=0){
      num=num+1
      writeRaster(LST,filename =paste0("Data/", RegionName,"/Model/overpass/LST/",local_year,"_",local_month,"_",local_date,"_",local_hour,"_",local_minute,"obs.nc"),overwrite=T )
      LSTpred=raster(paste0("Data/", RegionName,"/Model/overpass/LST/",local_year,"_",local_month,"_",local_date,"_",local_hour,"_",local_minute,".nc"))
      LSTpred[LSTpred[]<273.16]=NA
      LST=projectRaster(LST,LSTpred)
      LST[is.na(LSTpred)]=NA
      LSTpred[is.na(LST)]=NA
      timestr=paste0(local_year,"-",str_pad(local_month,2,pad="0"),"-",str_pad(local_date,2,pad="0"),"_",str_pad(local_hour,2,pad="0"),"-",str_pad(local_minute,2,pad="0"))
      
      
      png(file=paste0("Data/", RegionName,"/Model/overpass/LST/maps/",timestr,"scatter.png"),height=300,width=250)
      df=na.omit(data.frame(obs=LST[],pred=LSTpred[]))
      range2=range(df)
      plot_colorByDensity(df$obs,df$pred,ylab="observations",xlab="predictions",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
      legend("topleft",legend = c(paste0("Pearson r=",round(cor(df$obs,df$pred),2)),
                                  paste0("MEF=",round(calMEF(df$obs,df$pred),2)),
                                  paste0("RMSE=",round(calRMSE(df$obs,df$pred),2)),
                                  paste0("Bias=",round(calBias(df$obs,df$pred),2))),bty = "n")
      abline(0,1,lty=2,col="red")
      dev.off()
      
      if (num==1){
        result_all=df
      }else{
        result_all=rbind(result_all,df)
      }
      
      png(file=paste0("Data/", RegionName,"/Model/overpass/LST/maps/",timestr,".png"),height=300,width=1200)
      par(mfrow=c(1,4),mar=c(2, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
      range0=range(c(LST[]),na.rm=T)
      plot(LST,axes=F,legend=FALSE,main="Ecostress LST",colNA="grey",zlim=range0)
      # plot(LST, legend.only=TRUE,
      #      legend.width=3, legend.shrink=0.75,
      #      axis.args=list(cex.axis=1.5),
      #      legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      LSTpred[LSTpred[]<range0[1]]=range0[1]
      LSTpred[LSTpred[]>range0[2]]=range0[2]
      plot(LSTpred,axes=F,legend=FALSE,main="LST predict",colNA="grey",zlim=range0)
      plot(LSTpred, legend.only=TRUE,
           legend.width=3, legend.shrink=0.75,
           axis.args=list(cex.axis=1.5),
           legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      
      dif=LSTpred-LST
      pal=colorRampPalette(c("red","white", "blue"))(100)
      range1=max(abs(dif[]),na.rm=T)*0.5
      dif[dif[]<(-range1)]=-range1
      dif[dif[]>range1]=range1
      plot(dif,axes=F,legend=FALSE,main="Residual: pred-obs",col=pal,zlim=c(-range1,range1),colNA="grey")
      plot(dif, legend.only=TRUE,
           legend.width=3, legend.shrink=0.75,
           axis.args=list(cex.axis=1.5),
           legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2),col=pal,zlim=c(-range1,range1))
      GOES=ReadGOESLST(utc_year,utc_month,utc_date,utc_hour,utc_minute)
      if(!is.null(GOES)){
        plot(GOES,axes=F,legend=FALSE,main="GOES LST",colNA="grey")
        plot(GOES, legend.only=TRUE,
             legend.width=3, legend.shrink=0.75,
             axis.args=list(cex.axis=1.5),
             legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      }
      par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
      dev.off()
      
      
    }
    # if (!dir.exists("Ecostress_LST/maps/")){dir.create("Ecostress_LST/maps/")}
    # timestr=paste0(local_year,"-",str_pad(local_month,2,pad="0"),"-",str_pad(local_date,2,pad="0"),"_",str_pad(local_hour,2,pad="0"),"-",str_pad(local_minute,2,pad="0"))
    # png(file=paste0("Ecostress_LST/maps/",timestr,".png"),height=300,width=300)
    # plot(LST)
    # dev.off()
  }
}
write.csv(result_all,file=paste0("Data/", RegionName,"/Model/overpass/LST/scatter.csv"),row.names = F)

png(file=paste0("Data/", RegionName,"/Model/overpass/LST/maps/scatter.png"),height=400,width=350)
range2=range(result_all)
plot_colorByDensity(result_all$obs,result_all$pred,ylab="observations",xlab="predictions",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
legend("topleft",legend = c(paste0("Pearson r=",round(cor(result_all$obs,result_all$pred),2)),
                            paste0("MEF=",round(calMEF(result_all$obs,result_all$pred),2)),
                            paste0("RMSE=",round(calRMSE(result_all$obs,result_all$pred),2)),
                            paste0("Bias=",round(calBias(result_all$obs,result_all$pred),2))),bty = "n")
abline(0,1,lty=2,col="red")
dev.off()