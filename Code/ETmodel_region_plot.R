#check ETmodel_region results
################################################################
#plot
obs_str=""
library(stringr)
if(!dir.exists(paste0("Data/", RegionName,"/Model/overpass/ET/maps"))){dir.create(paste0("Data/", RegionName,"/Model/overpass/ET/maps"))}
filelist=list.files(paste0("Data/", RegionName,"/Ecostress_ET/"),pattern = "^ECO3ETPTJPL.001_EVAPOTRANSPIRATION_PT_JPL_ETinst_doy")
num=0
for (i in 1:length(filelist)){
  filename0=filelist[i]
  tmp=strsplit(filename0,"_")[[1]]
  utc_year=as.integer(substr(tmp[6],4,7))
  doy0=as.integer(substr(tmp[6],8,10))
  tmp2=strsplit(as.character(as.Date(paste0(utc_year,"-01-01"))+doy0-1),split = "-")[[1]]
  utc_month=as.integer(tmp2[2])
  utc_date=as.integer(tmp2[3])
  utc_hour=as.integer(substr(tmp[6],11,12))
  utc_minute=as.integer(substr(tmp[6],13,14))
  local_time=as.POSIXlt(paste0(as.Date(doy0-1,origin=paste0(utc_year,"-01-01"))," ",utc_hour,":",utc_minute,":00"),tz = "GMT")+UTC_OFFSET*3600
  time_tmp=time_decomp(local_time)
  local_year=time_tmp[1]
  local_month=time_tmp[2]
  local_date=time_tmp[3]
  local_hour=floor(time_tmp[5])
  local_minute=round((time_tmp[5]-floor(time_tmp[5]))*60)
  timestamp=local_year*10000+local_month*100+local_date
  fit_time=as.numeric(difftime(local_time,starttime,units="hour"))
  predname=paste0("Data/", RegionName,"/Model/overpass/ET/",local_year,"_",local_month,"_",local_date,"_",local_hour,"_",local_minute,obs_str,".nc")
  if (timestamp>=startdate & timestamp<=enddate & fit_time>=0 & file.exists(predname)){
    print(filename0)
    ET=raster(paste0("Data/", RegionName,"/Ecostress_ET/",filename0))
    writeRaster(ET,filename = paste0("Data/", RegionName,"/Model/overpass/ET/",local_year,"_",local_month,"_",local_date,"_",local_hour,"_",local_minute,"observation.nc"),overwrite=T)
    
    shortstr=c("c","i","s")
    longstr=c("ETcanopy","ETinterception","ETsoil")
    for (i in 1:3){
      ETobs_comp0=raster(paste0("Data/", RegionName,"/Ecostress_ET/",paste(tmp[1],tmp[2],tmp[3],tmp[4],longstr[i],tmp[6],tmp[7],sep = "_")))
      ETobs_comp=ET*ETobs_comp0/100
      writeRaster(ETobs_comp,filename = paste0("Data/", RegionName,"/Model/overpass/ET/ET",shortstr[i],local_year,"_",local_month,"_",local_date,"_",local_hour,"_",local_minute,"observation.nc"),overwrite=T)
    }
    
    ETpred=raster(predname)
    if (sum(!is.na(ET[]))!=0 & sum(!is.na(ETpred[]))!=0){
      num=num+1
      
      ET=projectRaster(ET,ETpred)
      
      ET[is.na(ETpred[])]=NA
      ETpred[is.na(ET[])]=NA
      timestr=paste0(local_year,"-",str_pad(local_month,2,pad="0"),"-",str_pad(local_date,2,pad="0"),"_",str_pad(local_hour,2,pad="0"),"-",str_pad(local_minute,2,pad="0"))
      
      #scatter plot
      png(file=paste0("Data/", RegionName,"/Model/overpass/ET/maps/",timestr,"scatter.png"),height=300,width=250)
      df=na.omit(data.frame(obs=ET[],pred=ETpred[]))
      range2=range(df)
      plot_colorByDensity(df$obs,df$pred,ylab="observations",xlab="predictions",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
      legend("bottomright",legend = c(paste0("Pearson r=",round(cor(df$obs,df$pred),2)),
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
      
      #spatial maps
      png(file=paste0("Data/", RegionName,"/Model/overpass/ET/maps/",timestr,".png"),height=300,width=1200)
      par(mfrow=c(1,4),mar=c(2, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
      
      #in different legends
      plot(ET,axes=F,legend=FALSE,main="Ecostress ET",colNA="grey")
      plot(ET, legend.only=TRUE,
           legend.width=3, legend.shrink=0.75,
           axis.args=list(cex.axis=1.5),
           legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      plot(ETpred,axes=F,legend=FALSE,main="ET predict",colNA="grey")
      plot(ETpred, legend.only=TRUE,
           legend.width=3, legend.shrink=0.75,
           axis.args=list(cex.axis=1.5),
           legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      
      #in the ET obs legend
      # range0=range(c(ET[]),na.rm=T)
      # plot(ET,axes=F,legend=FALSE,main="Ecostress",colNA="grey",zlim=range0)
      # plot(ET, legend.only=TRUE,
      #      legend.width=3, legend.shrink=0.75,
      #      axis.args=list(cex.axis=1.5),
      #      legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      # ETpred[ETpred[]<range0[1]]=range0[1]
      # ETpred[ETpred[]>range0[2]]=range0[2]
      # plot(ETpred,axes=F,legend=FALSE,main="Ecostress predict",colNA="grey",zlim=range0)
      # plot(ETpred, legend.only=TRUE,
      #      legend.width=3, legend.shrink=0.75,
      #      axis.args=list(cex.axis=1.5),
      #      legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      
      dif=ETpred-ET
      pal=colorRampPalette(c("red","white", "blue"))(100)
      range1=max(abs(dif[]),na.rm=T)*0.75
      dif[dif[]<(-range1)]=-range1
      dif[dif[]>range1]=range1
      plot(dif,axes=F,legend=FALSE,main="Residual: pred-obs",col=pal,zlim=c(-range1,range1),colNA="grey")
      plot(dif, legend.only=TRUE,
           legend.width=3, legend.shrink=0.75,
           axis.args=list(cex.axis=1.5),
           legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2),col=pal,zlim=c(-range1,range1))
      
      uncer_name=paste0("Ecostress_ET","/","ECO3ETPTJPL.001_EVAPOTRANSPIRATION_PT_JPL_ETinstUncertainty_",tmp[6],"_aid0001.tif")
      if(file.exists(uncer_name)){
        ETuncer=raster(uncer_name)
        plot(ETuncer,axes=F,legend=FALSE,main="Ecostress ET uncertainty",colNA="grey")
        plot(ETuncer, legend.only=TRUE,
             legend.width=3, legend.shrink=0.75,
             axis.args=list(cex.axis=1.5),
             legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
      }
      par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
      dev.off()
      
    #   #ET components
    #   shortstr=c("c","i","s")
    #   longstr=c("ETcanopy","ETinterception","ETsoil")
    #   for (i in 1:3){
    #     ETpred_comp=raster(paste0("Data/", RegionName,"/Model/overpass/ET/ET",shortstr[i],local_year,"_",local_month,"_",local_date,"_",local_hour,"_",local_minute,obs_str,".nc"))
    #     ETpred_comp=projectRaster(ETpred_comp,ETpred)
    #     ETobs_comp0=raster(paste0("Data/", RegionName,"/Ecostress_ET/",paste(tmp[1],tmp[2],tmp[3],tmp[4],longstr[i],tmp[6],tmp[7],sep = "_")))
    #     ETobs_comp0=projectRaster(ETobs_comp0,ETpred)
    #     ETobs_comp=ET*ETobs_comp0/100
    # 
    #     ETobs_comp[is.na(ETpred_comp[])]=NA
    #     ETpred_comp[is.na(ETobs_comp[])]=NA
    # 
    #     #scatter plot
    #     png(file=paste0("Data/", RegionName,"/Model/overpass/ET/maps/",timestr,"ET",shortstr[i],"scatter.png"),height=300,width=250)
    #     df=na.omit(data.frame(obs=ETobs_comp[],pred=ETpred_comp[]))
    #     range2=range(df)
    #     plot_colorByDensity(df$obs,df$pred,ylab="observations",xlab="predictions",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
    #     legend("bottomright",legend = c(paste0("Pearson r=",round(cor(df$obs,df$pred),2)),
    #                                     paste0("MEF=",round(calMEF(df$obs,df$pred),2)),
    #                                     paste0("RMSE=",round(calRMSE(df$obs,df$pred),2)),
    #                                     paste0("Bias=",round(calBias(df$obs,df$pred),2))),bty = "n")
    #     abline(0,1,lty=2,col="red")
    #     dev.off()
    #     
    #     #spatial map
    #     png(file=paste0("Data/", RegionName,"/Model/overpass/ET/maps/",timestr,"ET",shortstr[i],".png"),height=300,width=900)
    #     par(mfrow=c(1,3),mar=c(2, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
    #     #in different legends
    #     plot(ETobs_comp,axes=F,legend=FALSE,main=paste0("Ecostress ET",shortstr[i]),colNA="grey")
    #     plot(ETobs_comp, legend.only=TRUE,
    #          legend.width=3, legend.shrink=0.75,
    #          axis.args=list(cex.axis=1.5),
    #          legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
    #     plot(ETpred_comp,axes=F,legend=FALSE,main=paste0("ET",shortstr[i], "predict"),colNA="grey")
    #     plot(ETpred_comp, legend.only=TRUE,
    #          legend.width=3, legend.shrink=0.75,
    #          axis.args=list(cex.axis=1.5),
    #          legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
    #     dif=ETpred_comp-ETobs_comp
    #     pal=colorRampPalette(c("red","white", "blue"))(100)
    #     range1=max(abs(dif[]),na.rm=T)*0.75
    #     dif[dif[]<(-range1)]=-range1
    #     dif[dif[]>range1]=range1
    #     plot(dif,axes=F,legend=FALSE,main="Residual: pred-obs",col=pal,zlim=c(-range1,range1),colNA="grey")
    #     plot(dif, legend.only=TRUE,
    #          legend.width=3, legend.shrink=0.75,
    #          axis.args=list(cex.axis=1.5),
    #          legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2),col=pal,zlim=c(-range1,range1))
    #     par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
    #     dev.off()
    #   }
    }
  }
}
write.csv(result_all,file=paste0("Data/", RegionName,"/Model/overpass/ET/scatter.csv"),row.names = F)

png(file=paste0("Data/", RegionName,"/Model/overpass/ET/maps/scatter.png"),height=400,width=350)
range2=range(result_all)
plot_colorByDensity(result_all$obs,result_all$pred,ylab="observations",xlab="predictions",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
legend("bottomright",legend = c(paste0("Pearson r=",round(cor(result_all$obs,result_all$pred),2)),
                                paste0("MEF=",round(calMEF(result_all$obs,result_all$pred),2)),
                                paste0("RMSE=",round(calRMSE(result_all$obs,result_all$pred),2)),
                                paste0("Bias=",round(calBias(result_all$obs,result_all$pred),2))),bty = "n")
abline(0,1,lty=2,col="red")
dev.off()

##############################################################
#ET prediction comparison - prediction using original ECOSTRESS LST and leave-one-out LST prediction
if(!dir.exists(paste0("Data/", RegionName,"/Model/overpass/ET/pred_comp"))){dir.create(paste0("Data/", RegionName,"/Model/overpass/ET/pred_comp"))}
nclist=list.files(paste0("Data/", RegionName,"/Model/overpass/ET/"),pattern="^20.*\\obs.nc$")
num=0
for (ncname in nclist){
  filestr=strsplit(ncname,"obs.nc")[[1]]
  pred_LSTpred=raster(paste0("Data/", RegionName,"/Model/overpass/ET/",filestr,".nc"))
  pred_LSTobs=raster(paste0("Data/", RegionName,"/Model/overpass/ET/",ncname))
  if (sum(!is.na(pred_LSTobs[]))!=0 & sum(!is.na(pred_LSTpred[]))!=0){
    num=num+1
    
    pred_LSTpred=projectRaster(pred_LSTpred,pred_LSTobs)
    pred_LSTobs[is.na(pred_LSTpred[])]=NA
    pred_LSTpred[is.na(pred_LSTobs[])]=NA
    
    #scatter plot
    png(file=paste0("Data/", RegionName,"/Model/overpass/ET/pred_comp/",filestr,"scatter.png"),height=300,width=250)
    df=na.omit(data.frame(obs=pred_LSTobs[],pred=pred_LSTpred[]))
    range2=range(df)
    plot_colorByDensity(df$obs,df$pred,ylab="ETpred_LSTobs",xlab="ETpred_LSTpred",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
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
    
    #spatial maps
    png(file=paste0("Data/", RegionName,"/Model/overpass/ET/pred_comp/",filestr,".png"),height=300,width=900)
    par(mfrow=c(1,3),mar=c(2, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
    
    #in different legends
    plot(pred_LSTobs,axes=F,legend=FALSE,main="ETpred_LSTobs",colNA="grey")
    plot(pred_LSTobs, legend.only=TRUE,
         legend.width=3, legend.shrink=0.75,
         axis.args=list(cex.axis=1.5),
         legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
    plot(pred_LSTpred,axes=F,legend=FALSE,main="ETpred_LSTpred",colNA="grey")
    plot(pred_LSTpred, legend.only=TRUE,
         legend.width=3, legend.shrink=0.75,
         axis.args=list(cex.axis=1.5),
         legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2))
    
    dif=pred_LSTpred-pred_LSTobs
    pal=colorRampPalette(c("red","white", "blue"))(100)
    range1=max(abs(dif[]),na.rm=T)*0.75
    dif[dif[]<(-range1)]=-range1
    dif[dif[]>range1]=range1
    plot(dif,axes=F,legend=FALSE,main="ETpred_LSTpred - ETpred_LSTobs",col=pal,zlim=c(-range1,range1),colNA="grey")
    plot(dif, legend.only=TRUE,
         legend.width=3, legend.shrink=0.75,
         axis.args=list(cex.axis=1.5),
         legend.args=list(text='', side=4, font=2, line=4.5, cex=1.2),col=pal,zlim=c(-range1,range1))
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
    dev.off()
  }
}
write.csv(result_all,file=paste0("Data/", RegionName,"/Model/overpass/ET/scatter_pred_comp.csv"),row.names = F)

png(file=paste0("Data/", RegionName,"/Model/overpass/ET/pred_comp/scatter.png"),height=400,width=350)
range2=range(result_all)
plot_colorByDensity(result_all$obs,result_all$pred,ylab="ETpred_LSTobs",xlab="ETpred_LSTpred",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
legend("topleft",legend = c(paste0("Pearson r=",round(cor(result_all$obs,result_all$pred),2)),
                            paste0("MEF=",round(calMEF(result_all$obs,result_all$pred),2)),
                            paste0("RMSE=",round(calRMSE(result_all$obs,result_all$pred),2)),
                            paste0("Bias=",round(calBias(result_all$obs,result_all$pred),2))),bty = "n")
abline(0,1,lty=2,col="red")
dev.off()
