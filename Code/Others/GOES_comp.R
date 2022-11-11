#Compare ECOSTRESS LST with GOES LST (interpolated spatially and temporally)

if(!dir.exists("GOES_LST/compare")){dir.create("GOES_LST/compare")}

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


library(stringr)
QAfile=read.csv("Ecostress_LST/ECO2LSTE-001-SDS-QC-lookup.csv")
# summary(QAfile$Mandatory.QA.flags)
QA_good=QAfile$Value[QAfile$Mandatory.QA.flags %in% c('Pixel produced, best quality','Pixel produced, nominal quality')]
filelist=list.files("Ecostress_LST/",pattern = "^ECO2LSTE.001_SDS_LST_doy")

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
    LST=raster(paste0("Ecostress_LST/",filename0))*0.02
    LST_QC=raster(paste0("Ecostress_LST/ECO2LSTE.001_SDS_QC_",tmp[4],"_aid0001.tif"))
    LST_cloudmask=raster(paste0("Ecostress_LST/ECO2CLD.001_SDS_CloudMask_",tmp[4],"_aid0001.tif"))
    LST[!(LST_QC[] %in% QA_good)]=NA
    LST[LST_cloudmask[]!=1 & LST_cloudmask[]!=33]=NA
    if (sum(!is.na(LST[]))!=0){
      num=num+1
      # if (num==1){
      #   cdl=raster("../us-seg-region/CDL/CDL_2018_reclassify.tif")
      #   landtype=c('Others','Cropland','Forest','Shrubland & Grassland','Urban','Wetland','Water')
      #   cdl=projectRaster(cdl,LST,method='ngb')
      # }
      
      timestr=paste0(local_year,"-",str_pad(local_month,2,pad="0"),"-",str_pad(local_date,2,pad="0"),"_",str_pad(local_hour,2,pad="0"),"-",str_pad(local_minute,2,pad="0"))
      GOES=ReadGOESLST(utc_year,utc_month,utc_date,utc_hour,utc_minute,method="2ndpoly")
      GOES=projectRaster(GOES,LST)
      
      df=na.omit(data.frame(ECOSTRESS=LST[]-273.15,GOES=GOES[]-273.15)) #cdl

      if (num==1){
        result_all=df
      }else{
        result_all=rbind(result_all,df)
      }
      
    }
  }
}
write.csv(result_all,file="GOES_LST/compare/result.csv",row.names = F)

range2=range(c(result_all$ECOSTRESS,result_all$GOES))
plot_colorByDensity(result_all$GOES,result_all$ECOSTRESS,ylab="GOES",xlab="ECOSTRESS",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
# palette <- c("#4FFFFF","#75B5EE","#FF1300","#E8E87A","#5dba5c",
#                  "#BF56C3","#82342E")
# for (i in 0:6){
#   if (i==0){
#     plot(result_all$ECOSTRESS[result_all$cdl==i],result_all$GOES[result_all$cdl==i],pch=20,col=palette[i],xlim=range2,ylim=range2,xlab="ECOSTRESS LST",ylab="GOES LST")
#   }else{
#     points(result_all$ECOSTRESS[result_all$cdl==i],result_all$GOES[result_all$cdl==i],pch=20,col=palette[i])
#   }
# }
lmfit=lm(GOES~ECOSTRESS,data=result_all)
summary(lmfit)
abline(lmfit$coefficients,lty=2,col="red")
legend("topleft",c(paste0("R2=",format(summary(lmfit)$r.square,digit=2,nsmall = 2)),
                   paste0("y=",format(lmfit$coefficients[2],digit=2,nsmall = 2),"x+",format(lmfit$coefficients[1],digit=2,nsmall = 2))),
                   pch=NA,lty=c(2,NA),col="red",text.col = "red",bty="n")
abline(0,1,lty=2)

library(ggplot2)
ggplot(result_all, aes(x=ECOSTRESS, y=GOES) ) + xlim(range2) + ylim(range2) +
  xlab("ECOSTRESS LST (\u00B0C)") + ylab("GOES LST (\u00B0C)") +
  geom_bin2d(binwidth = c(0.1, 0.1)) +
  scale_fill_continuous(type = "viridis") +
  # scale_fill_gradient(low = "grey", high = "black") +
  geom_abline(intercept = 0, slope = 1, color="black", 
                linetype="dashed", size=1) +
  geom_abline(intercept = lmfit$coefficients[1], slope = lmfit$coefficients[2], color="red", 
              linetype="dashed", size=1) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text.x = element_text(face="bold", size=10),
          axis.text.y = element_text(face="bold",size=10))