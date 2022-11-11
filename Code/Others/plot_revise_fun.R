#functions used in making plots in plot_revised.R plot_revised_us_arm.R

library(raster)
library(ncdf4)
library(stringr)
library(ggplot2)
library(cowplot)

#################################################################################
#statistics
calRMSE=function(x,y){
  tmp=na.omit(data.frame(x,y))
  x=tmp$x
  y=tmp$y
  error=x-y
  return(sqrt(sum(error^2)/length(error)))
}
calMEF=function(obs,pred){ #the Nash and Sutcliffe model efficiency (MEF)
  tmp=na.omit(data.frame(obs,pred))
  obs=tmp$obs
  pred=tmp$pred
  1-sum((pred-obs)^2)/sum((obs-mean(obs))^2)
}
calBias=function(obs,pred){
  tmp=na.omit(data.frame(obs,pred))
  obs=tmp$obs
  pred=tmp$pred
  error=pred-obs
  return(sum(error)/length(error))
}
reset_par <- function(){
  op <- structure(list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE,
                       ask = FALSE, bg = "transparent", bty = "o", cex = 1, 
                       cex.axis = 1, cex.lab = 1, cex.main = 1.2, cex.sub = 1, 
                       col = "black", col.axis = "black", col.lab = "black", 
                       col.main = "black", col.sub = "black", crt = 0, err = 0L, 
                       family = "", fg = "black", fig = c(0, 1, 0, 1), 
                       fin = c(6.99999895833333, 6.99999895833333), font = 1L, 
                       font.axis = 1L, font.lab = 1L, font.main = 2L, 
                       font.sub = 1L, lab = c(5L, 5L, 7L), las = 0L, 
                       lend = "round", lheight = 1, ljoin = "round", lmitre = 10, 
                       lty = "solid", lwd = 1, mai = c(1.02, 0.82, 0.82, 0.42), 
                       mar = c(5.1, 4.1, 4.1, 2.1), mex = 1, mfcol = c(1L, 1L), 
                       mfg = c(1L, 1L, 1L,1L), mfrow = c(1L, 1L), 
                       mgp = c(3, 1, 0), mkh = 0.001, new = FALSE, 
                       oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), 
                       omi = c(0, 0, 0,0), pch = 1L, 
                       pin = c(5.75999895833333, 5.15999895833333),
                       plt = c(0.117142874574832, 0.939999991071427, 
                               0.145714307397962, 0.882857125425167), 
                       ps = 12L, pty = "m", smo = 1, srt = 0, tck = NA_real_, 
                       tcl = -0.5, usr = c(0.568, 1.432, 0.568, 1.432), 
                       xaxp = c(0.6, 1.4, 4), xaxs = "r", xaxt = "s", 
                       xpd = FALSE, yaxp = c(0.6, 1.4, 4), yaxs = "r", 
                       yaxt = "s", ylbias = 0.2), 
                  .Names = c("xlog", "ylog", "adj", "ann", "ask", "bg", 
                             "bty", "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", 
                             "col", "col.axis", "col.lab", "col.main", "col.sub", "crt", 
                             "err", "family", "fg", "fig", "fin", "font", "font.axis", 
                             "font.lab", "font.main", "font.sub", "lab", "las", "lend", 
                             "lheight", "ljoin", "lmitre", "lty", "lwd", "mai", "mar", 
                             "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", "oma",
                             "omd", "omi", "pch", "pin", "plt", "ps", "pty", "smo", 
                             "srt", "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt", "xpd", 
                             "yaxp", "yaxs", "yaxt", "ylbias"))
  par(op)
}
#################################################################################
#scatterplot between ECOSTRESS and GOES LST
GOES_ECOSTRESS_scatter_plot=function(title,plotid){
  range2=range(c(result_all$ECOSTRESS,result_all$GOES))
  lmfit=lm(ECOSTRESS~GOES,data=result_all)
  # summary(lmfit)
  
  scatterplot = ggplot(result_all, aes(x=GOES, y=ECOSTRESS) ) + xlim(range2) + ylim(range2) +
    xlab("GOES LST (\u00B0C)") + ylab("ECOSTRESS LST (\u00B0C)") + 
    ggtitle(title) +
    geom_bin2d(binwidth = c(1, 1)) +
    # scale_fill_continuous(type = "viridis") +
    # scale_fill_gradient(low = "grey", high = "black") +
    scale_fill_gradientn(colors=c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100")) +
    
    geom_abline(intercept = 0, slope = 1, color="red", 
                linetype="dashed", size=0.5) +
    # geom_abline(intercept = lmfit$coefficients[1], slope = lmfit$coefficients[2], color="blue", 
    #             linetype="dashed", size=0.5) +
    annotate('text', x=range2[1]+(range2[2]-range2[1])/15, y=range2[2]-(range2[2]-range2[1])/15, label= plotid, size = 6) + 
    # annotate('text', x=range2[2]-(range2[2]-range2[1])/5, y=range2[1]+(range2[2]-range2[1])/10, label= paste0("R^2 ==",format(summary(lmfit)$r.square,digit=2,nsmall = 2)),parse=TRUE,colour = "red", size = 3) + 
    # annotate('text', x=range2[2]-(range2[2]-range2[1])/5, y=range2[1]+(range2[2]-range2[1])/5, label= paste0("y=",format(lmfit$coefficients[2],digit=2,nsmall = 2),"x+",format(lmfit$coefficients[1],digit=2,nsmall = 2)),colour = "red", size = 3) + 
    annotate('text', x=range2[2]-(range2[2]-range2[1])/3, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13*4, label= paste0("r=",format(cor(result_all$ECOSTRESS,result_all$GOES),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 4) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/3, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13*3, label= paste0("MEF=",format(calMEF(result_all$ECOSTRESS,result_all$GOES),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 4) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/3, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13*2, label= paste0("RMSE=",format(calRMSE(result_all$ECOSTRESS,result_all$GOES),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 4) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/3, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13, label= paste0("Bias=",format(calBias(result_all$ECOSTRESS,result_all$GOES),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 4) +
    
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text.x = element_text(face="bold", size=10),
          axis.text.y = element_text(face="bold",size=10),
          plot.title = element_text(hjust = 0.5))
  return(scatterplot)
}


####################################################################################
#LST-site- time series
LST_site_plot=function(){
  library(suncalc)
  sunrise=time_decomp(getSunlightTimes(as.Date(startdatestr),lat0,lon0)$sunrise+UTC_OFFSET*3600)
  t_sr=sunrise[5]
  starttime=as.POSIXlt(paste0(startdatestr," ",floor(t_sr),":",(t_sr-floor(t_sr))*60))
  
  result_ts1=read.csv(paste0(RegionName,"/Tower/DTC_ts.csv"))
  
  #exclude rainy days
  if (RegionName=="us-seg-region2new"){
    result_ts1[result_ts1$local_year==2018 & result_ts1$local_month==8 & result_ts1$local_date %in% c(10,11),"Tower_ts"]=NA
  }else if (RegionName=="us-seg-region5new"){
    result_ts1[result_ts1$local_year==2020 & result_ts1$local_month==4 & result_ts1$local_date==13,"Tower_ts"]=NA
  }
  
  print("comparison with tower:")
  print("r")
  print(cor(result_ts1$Tower_ts,result_ts1$Eco_ts,use="complete.obs"))
  print("RMSE")
  print(calRMSE(result_ts1$Tower_ts,result_ts1$Eco_ts))
  print("MEF")
  print(calMEF(result_ts1$Tower_ts,result_ts1$Eco_ts))
  print("Bias")
  print(calBias(result_ts1$Tower_ts,result_ts1$Eco_ts))
  
  result_loo1=read.csv(paste0(RegionName,"/Tower/DTC_loo.csv"))
  print("comparison with ECOSTRESS loo:")
  print("r")
  print(cor(result_loo1$Ecostress_obs,result_loo1$leave_one_out_pred,use="complete.obs"))
  print("RMSE")
  print(calRMSE(result_loo1$Ecostress_obs,result_loo1$leave_one_out_pred))
  print("MEF")
  print(calMEF(result_loo1$Ecostress_obs,result_loo1$leave_one_out_pred))
  print("Bias")
  print(calBias(result_loo1$Ecostress_obs,result_loo1$leave_one_out_pred))
  
  GOES_tower=na.omit(read.csv(paste0(RegionName,"/Tower/GOES_tower.csv")))
  GOES_tower$local_time=as.POSIXlt(paste0(GOES_tower$local_year,"-",GOES_tower$local_month,"-",GOES_tower$local_date," ",GOES_tower$local_hour,":",GOES_tower$local_minute))
  GOES_tower$t0_GOES=as.numeric(difftime(GOES_tower$local_time,starttime,units="hour"))
  t0_GOES=GOES_tower$t0_GOES[GOES_tower$t0_GOES>=0]
  temps0_GOES=GOES_tower$GOES_LST_tower[GOES_tower$t0_GOES>=0]-273.15
  
  Tower_time=difftime(as.POSIXct(paste0(result_ts1$local_year,"-",result_ts1$local_month,"-",result_ts1$local_date," ",result_ts1$local_hour,":",result_ts1$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  Eco_time=difftime(as.POSIXct(paste0(result_loo1$local_year,"-",result_loo1$local_month,"-",result_loo1$local_date," ",result_loo1$local_hour,":",result_loo1$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  
  range0=range(c(temps0_GOES,result_ts1$GOES_ts,result_loo1$Ecostress_obs,result_ts1$Eco_ts,result_loo1$leave_one_out_pred,result_ts1$Tower_ts),na.rm=T)
  plot(Tower_time,result_ts1$Tower_ts,type="l",pch=1,col="black",lwd=2,ylim=range0+c(-5,2),xlab="",ylab="",axes = F,frame=T)
  axis(2)
  mtext("LST (\u00B0C)",2,line=2)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label)
  
  # lines(Tower_time, result_ts1$GOES_ts,col="blue")
  points((t0_GOES+t_sr)/24, temps0_GOES,pch=20,col="deepskyblue",cex=1)
  points(Eco_time, result_loo1$leave_one_out_pred,pch=9,col="darkgreen",cex=1,lwd=1.5)
  lines(Tower_time, result_ts1$Eco_ts,col="red")
  points(Eco_time, result_loo1$Ecostress_obs,pch=20,col="red",cex=1)
  
  if (RegionName %in% c("us-seg-region2new","us-seg-region5new","us-seg-region4new")){
    #cloud shading
    cloud_data=read.csv(paste0(RegionName,"/Tower/cloud.csv"))
    if (nrow(result_ts1)!=nrow(cloud_data)){
      stop("Check the data")
    }
    for (i in 1:nrow(cloud_data)){
      if (is.na(cloud_data$cloud_fraction[i])){
        if (cloud_data$SW_IN_pot[i]<10){
          rect(Tower_time[i]-15/60/24, par('usr')[3], Tower_time[i]+15/60/24, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'black', border = "transparent")
        }
      }else if (cloud_data$cloud_fraction[i]<=0.2){
        rect(Tower_time[i]-15/60/24, par('usr')[3], Tower_time[i]+15/60/24, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'gold', border = "transparent")
      }else if (cloud_data$cloud_fraction[i]>0.2){
        rect(Tower_time[i]-15/60/24, par('usr')[3], Tower_time[i]+15/60/24, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'gray40', border = "transparent")
      }
    }
    
    #rain shading
    if (RegionName=="us-seg-region2new"){
      rain_start=difftime(as.POSIXct('2018-08-10 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rain_end=difftime(as.POSIXct('2018-08-12 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rect(rain_start, par('usr')[3], rain_end, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'blue', border = "transparent")
    }else if(RegionName=="us-seg-region5new"){
      rain_start=difftime(as.POSIXct('2020-04-13 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rain_end=difftime(as.POSIXct('2020-04-14 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rect(rain_start, par('usr')[3], rain_end, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'blue', border = "transparent")
    }
  }
}


###############################################################
#constructed LST at tower site - scatterplot
DTC_site_scatterplot=function(){
  result_ts1=read.csv(paste0(RegionName,"/Tower/DTC_ts.csv"))
  Tower_data=Tower_data0[Tower_data0$TIMESTAMP_START>=startdate*10000 & Tower_data0$TIMESTAMP_START<(enddate+1)*10000,]
  
  if (nrow(result_ts1)!=nrow(Tower_data)){
    stop("Check the data")
  }
  
  local_time=as.character(Tower_data$TIMESTAMP_START)
  local_year=as.integer(substr(local_time,1,4))
  local_month=as.integer(substr(local_time,5,6))
  local_date=as.integer(substr(local_time,7,8))
  local_hour=as.integer(substr(local_time,9,10))
  local_minute=as.integer(substr(local_time,11,12))+15
  
  #exclude rainy days
  if (RegionName=="us-seg-region2new"){
    Tower_data[local_year==2018 & local_month==8 & local_date %in% c(10,11),]=NA
    result_ts1[local_year==2018 & local_month==8 & local_date %in% c(10,11),]=NA
  }else if (RegionName=="us-seg-region5new"){
    Tower_data[local_year==2020 & local_month==4 & local_date==13,]=NA
    result_ts1[local_year==2020 & local_month==4 & local_date==13,]=NA
  }
  
  range0=range(c(result_ts1$Tower_ts,result_ts1$Eco_ts),na.rm=T)
  plot(result_ts1$Tower_ts,result_ts1$Eco_ts,pch=20,xlab="", ylab="",xlim=range0,ylim=range0)
  abline(0,1,lty=2,col="red")
  
  legend('bottomright',legend = c(paste0("r=",format(cor(result_ts1$Tower_ts,result_ts1$Eco_ts,use = "complete.obs"),digits = 2,nsmall=2)),
                                  paste0("MEF=",format(calMEF(result_ts1$Tower_ts,result_ts1$Eco_ts),digits = 2,nsmall=2)),
                                  paste0("RMSE=",format(calRMSE(result_ts1$Tower_ts,result_ts1$Eco_ts),digits = 2,nsmall=2),"\u00B0C"),
                                  paste0("Bias=",format(calBias(result_ts1$Tower_ts,result_ts1$Eco_ts),digits = 2,nsmall=2),"\u00B0C")),bty = "n",cex=1) #,adj=c(0,-0.3)
  mtext("Constructed LST (\u00B0C)",2,line=2,cex=0.8) #ylab
  mtext("Tower LST (\u00B0C)",1,line=2.4,cex=0.8) #xlab
}

###########################################################################################
#ET-site time series
ET_site_plot=function(){
  Tower_data=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  ECOSTRESS_data=read.csv(paste0(RegionName,"/Tower/ECOSTRESS_ET.csv"))
  Tower_time=difftime(as.POSIXlt(paste0(Tower_data$local_year,"-",Tower_data$local_month,"-",Tower_data$local_date," ",Tower_data$local_hour,":",Tower_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  ECOSTRESS_time=difftime(as.POSIXlt(paste0(ECOSTRESS_data$local_year,"-",ECOSTRESS_data$local_month,"-",ECOSTRESS_data$local_date," ",ECOSTRESS_data$local_hour,":",ECOSTRESS_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  
  #exclude rainy days
  if (RegionName=="us-seg-region2new"){
    Tower_data[Tower_data$local_year==2018 & Tower_data$local_month==8 & Tower_data$local_date %in% c(10,11),"ET_Tower"]=NA
  }else if (RegionName=="us-seg-region5new"){
    Tower_data[Tower_data$local_year==2020 & Tower_data$local_month==4 & Tower_data$local_date==13,"ET_Tower"]=NA
  }
  
  print("comparison with tower:")
  print("r")
  print(cor(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM,use="complete.obs"))
  print("RMSE")
  print(calRMSE(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM))
  print("MEF")
  print(calMEF(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM))
  print("Bias")
  print(calBias(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM))
  print("Tower daytime mean")
  print(mean(na.omit(Tower_data)$ET_Tower))
  
  range0=range(c(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfM,Tower_data$ET_MERRA2_modfgfMfSM,ECOSTRESS_data$ETobs),na.rm=T)
  range0=range0+c(-range0[2]/15,0)
  plot(Tower_time,Tower_data$ET_Tower,type="l",pch=1,col="black",ylim=range0,xlab="",ylab='',main="",axes = F,frame=T)
  axis(2)
  mtext(expression(paste("ET (W/m"^"2",")")),2,line = 2)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+3)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label)
  lines(Tower_time, Tower_data$ET_MERRA2_modfgfM,col="blue")
  if (RegionName %in% c("us-seg-region2new","us-seg-region5new","us-seg-region4new")){
    lines(Tower_time, Tower_data$ET_Tower_modfgfMfSM,col="limegreen")
  }
  lines(Tower_time, Tower_data$ET_MERRA2_modfgfMfSM,col="red")
  points(ECOSTRESS_time,ECOSTRESS_data$ETobs,pch=20,col="red",cex=1.5)
  points(ECOSTRESS_time,ECOSTRESS_data$ET_star,pch=18,col="darkgreen",cex=1.5)
  
  if (RegionName %in% c("us-seg-region2new","us-seg-region5new","us-seg-region4new")){
    #cloud shading
    cloud_data=read.csv(paste0(RegionName,"/Tower/cloud.csv"))
    if (nrow(Tower_data)!=nrow(cloud_data)){
      stop("Check the data")
    }
    for (i in 1:nrow(cloud_data)){
      if (is.na(cloud_data$cloud_fraction[i])){
        if (cloud_data$SW_IN_pot[i]<10){
          rect(Tower_time[i]-15/60/24, par('usr')[3], Tower_time[i]+15/60/24, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'black', border = "transparent")
        }
      }else if (cloud_data$cloud_fraction[i]<=0.2){
        rect(Tower_time[i]-15/60/24, par('usr')[3], Tower_time[i]+15/60/24, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'gold', border = "transparent")
      }else if (cloud_data$cloud_fraction[i]>0.2){
        rect(Tower_time[i]-15/60/24, par('usr')[3], Tower_time[i]+15/60/24, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'gray40', border = "transparent")
      }
    }
    
    #rain shading
    if (RegionName=="us-seg-region2new"){
      rain_start=difftime(as.POSIXct('2018-08-10 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rain_end=difftime(as.POSIXct('2018-08-12 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rect(rain_start, par('usr')[3], rain_end, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'blue', border = "transparent")
    }else if(RegionName=="us-seg-region5new"){
      rain_start=difftime(as.POSIXct('2020-04-13 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rain_end=difftime(as.POSIXct('2020-04-14 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
      rect(rain_start, par('usr')[3], rain_end, par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, col = 'blue', border = "transparent")
    }
  }
}

###############################################################
#ET-site-scatter
ET_site_scatterplot=function(){
  Tower_data=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  
  #exclude rainy days
  if (RegionName=="us-seg-region2new"){
    Tower_data[Tower_data$local_year==2018 & Tower_data$local_month==8 & Tower_data$local_date %in% c(10,11),]=NA
  }else if (RegionName=="us-seg-region5new"){
    Tower_data[Tower_data$local_year==2020 & Tower_data$local_month==4 & Tower_data$local_date==13,]=NA
  }
  
  range0=range(c(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM),na.rm=T)
  plot(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM,pch=20,xlab="", ylab="",xlim=range0,ylim=range0)
  abline(0,1,lty=2,col="red")
  
  text(x=range0[2]-(range0[2]-range0[1])/2, y=range0[1]-(range0[2]-range0[1])/15+(range0[2]-range0[1])/10*4,bquote("r="*.(format(cor(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM,use = "complete.obs"),digits = 2,nsmall=2))),pos=4)
  text(x=range0[2]-(range0[2]-range0[1])/2, y=range0[1]-(range0[2]-range0[1])/15+(range0[2]-range0[1])/10*3,bquote("MEF="*.(format(calMEF(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM),digits = 2,nsmall=2))),pos=4)
  text(x=range0[2]-(range0[2]-range0[1])/2, y=range0[1]-(range0[2]-range0[1])/15+(range0[2]-range0[1])/10*2,bquote("RMSE="*.(format(calRMSE(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM),digits = 2,nsmall=2))*"W/"*m^2),pos=4)
  text(x=range0[2]-(range0[2]-range0[1])/2, y=range0[1]-(range0[2]-range0[1])/15+(range0[2]-range0[1])/10,bquote("Bias="*.(format(calBias(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM),digits = 2,nsmall=2))*"W/"*m^2),pos=4)
  # legend('bottomright',legend = c(bquote("r="*.(format(cor(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM,use = "complete.obs"),digits = 2,nsmall=2))),
  #                                 bquote("MEF="*.(format(calMEF(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM),digits = 2,nsmall=2))),
  #                                 bquote("RMSE="*.(format(calRMSE(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM),digits = 2,nsmall=2))*"W/"*m^2),
  #                                 bquote("Bias="*.(format(calBias(Tower_data$ET_Tower,Tower_data$ET_MERRA2_modfgfMfSM),digits = 2,nsmall=2))*"W/"*m^2)),bty = "n",cex=1) #,adj=c(0,-0.3)
  mtext(expression(paste("Constructed ET (modified ","f"["SM"],", W/m"^"2",")")),2,line=2,cex=0.8) #ylab
  mtext(expression(paste("Tower ET (W/m"^"2",")")),1,line=2.4,cex=0.8) #xlab
}


###################################################################################
ET_site_sup_plot=function(){
  Tower_data=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  Tower_time=difftime(as.POSIXlt(paste0(Tower_data$local_year,"-",Tower_data$local_month,"-",Tower_data$local_date," ",Tower_data$local_hour,":",Tower_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  
  #exclude rainy days
  if (RegionName=="us-seg-region2new"){
    Tower_data[Tower_data$local_year==2018 & Tower_data$local_month==8 & Tower_data$local_date %in% c(10,11),"ET_Tower"]=NA
  }else if (RegionName=="us-seg-region5new"){
    Tower_data[Tower_data$local_year==2020 & Tower_data$local_month==4 & Tower_data$local_date==13,"ET_Tower"]=NA
  }
  
  range0=range(c(Tower_data$ET_Tower,Tower_data$ET_Tower_modfgfMfSM,Tower_data$ET_MERRA2_modfgfMfSM),na.rm=T)
  plot(Tower_time,Tower_data$ET_Tower,type="l",pch=1,col="black",ylim=range0,xlab="",ylab='',main="",axes = F,frame=T)
  axis(2)
  mtext(expression(paste("ET (W/m"^"2",")")),2,line = 2)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+3)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label)
  lines(Tower_time, Tower_data$ET_Tower_modfgfMfSM,col="purple")
  lines(Tower_time, Tower_data$ET_MERRA2_modfgfMfSM,col="blue")
}

#######################################################################################
#LST-region
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

#######################################################################################
#LST-region-scatter
LST_region_scatter_plot=function(title,plotid){
  result_all=result_all-273.15
  range2=range(result_all)
  
  scatterplot = ggplot(result_all, aes(x=pred, y=obs) ) + xlim(range2) + ylim(range2) +
    xlab("Constructed LST (\u00B0C)") + ylab("ECOSTRESS LST (\u00B0C)") +
    ggtitle(title) +
    geom_bin2d(binwidth = c(0.2, 0.2)) +
    scale_fill_gradientn(colors=c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100")) +
    
    geom_abline(intercept = 0, slope = 1, color="red",
                linetype="dashed", size=0.5) +
    
    annotate('text', x=range2[1]+(range2[2]-range2[1])/15, y=range2[2]-(range2[2]-range2[1])/15, label= plotid,size=5) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*4, label= paste0("r=",format(cor(result_all$obs,result_all$pred),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 3) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*3, label= paste0("MEF=",format(calMEF(result_all$obs,result_all$pred),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 3) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*2, label= paste0("RMSE=",format(calRMSE(result_all$obs,result_all$pred),digits = 2,nsmall=2),"\u00B0C"),hjust=0,parse=F,colour = "black", size = 3) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10, label= paste0("Bias=",format(calBias(result_all$obs,result_all$pred),digits = 2,nsmall=2),"\u00B0C"),hjust=0,parse=F,colour = "black", size = 3) +
    
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text= element_text(size=10, colour="black"),
          axis.title=element_text(size=12),
          plot.title = element_text(size=14,hjust = 0.5,face = "bold"),
          legend.key.width= unit(0.4, 'cm'))
  return(scatterplot)
}


#########################################################################################
#ET-region-scatter
ET_region_scatter_plot=function(title,plotid){
  range2=range(result_all)
  scatterplot = ggplot(result_all, aes(x=pred, y=obs) ) + xlim(range2) + ylim(range2) +
    xlab(expression(paste("Constructed ET (W/m"^"2",")"))) + ylab(expression(paste("ECOSTRESS ET* (W/m"^"2",")"))) +
    ggtitle(title) +
    geom_bin2d(binwidth = c(5, 5)) +
    scale_fill_gradientn(colors=c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100")) +
    
    geom_abline(intercept = 0, slope = 1, color="red",
                linetype="dashed", size=0.5) +
    
    annotate('text', x=range2[1]+(range2[2]-range2[1])/15, y=range2[2]-(range2[2]-range2[1])/15, label= plotid,size=5) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*4, label= bquote("r="*.(format(cor(result_all$obs,result_all$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 3) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*3, label= bquote("MEF="*.(format(calMEF(result_all$obs,result_all$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 3) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*2, label= bquote("RMSE="*.(format(calRMSE(result_all$obs,result_all$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 3) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10, label= bquote("Bias="*.(format(calBias(result_all$obs,result_all$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 3) +
    
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text= element_text(size=10, colour="black"),
          axis.title=element_text(size=12),
          plot.title = element_text(size=14,hjust = 0.5,face = "bold"),
          legend.key.width= unit(0.4, 'cm'))
  return(scatterplot)
}


###############################################################################
#met-comp
plot_timeseries=function(timestamps,rs,tower,startdatestr,enddatestr,main="",ylab=""){
  range0=range(c(tower,rs),na.rm=T)
  plot(timestamps,tower,type="l",pch=1,col="black",ylim=range0+c(0,(range0[2]-range0[1])/3),xlab="",ylab="",main=main,axes = F,frame=T)
  lines(timestamps, rs,col="red")
  axis(2)
  mtext(ylab,2,2)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+3)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label)
}

met_comp_plot=function(){
  Tower_input=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  plot_time=difftime(as.POSIXlt(paste0(Tower_input$local_year,"-",Tower_input$local_month,"-",Tower_input$local_date," ",Tower_input$local_hour,":",Tower_input$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  
  jpeg(paste0("Figures_revised/met_comp",figid,".jpg"), width = 2100, height = 2100,quality = 100,res=300)
  op=par() #original par
  
  par(fig=c(0.1,1,0.76,1),mai = c(0.3, 1.5, 0.2, 0.5),oma=c(0,0,0,0),mar=c(1.5, 2, 1, 1.2))
  plot_timeseries(plot_time,Tower_input$Rn_MERRA2,Tower_input$Rn_Tower,startdatestr,enddatestr,ylab=expression(paste("Rn (W/m"^"2",")")))
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")
  legend("topright",legend = c("Tower","MERRA-2"),col=c("black","red"),lty=c(1,1),pt.cex=1,bty = "n",cex=0.8,ncol = 2,seg.len=1.2,x.intersp = 0.5)
  
  par(fig=c(0.1,1,0.52,0.76), new=TRUE)
  plot_timeseries(plot_time,Tower_input$Ta_MERRA2,Tower_input$Ta_Tower,startdatestr,enddatestr,ylab="Tair (\u00B0C)")
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")
  
  par(fig=c(0.1,1,0.28,0.52), new=TRUE)
  plot_timeseries(plot_time,Tower_input$vpd_MERRA2,Tower_input$vpd_Tower,startdatestr,enddatestr,ylab="VPD (kPa)")
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)")
  
  par(fig=c(0.1,1,0.04,0.28), new=TRUE)
  plot_timeseries(plot_time,Tower_input$rh_MERRA2,Tower_input$rh_Tower,startdatestr,enddatestr,ylab="RH")
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(d)")
  
  par(op) #restore par
  dev.off()
}

################################################################################################
#ET-site-heatmap
ET_site_heatmap=function(df,ylabel,figid){
  range2=range(df)
  p1=ggplot(df, aes(x=pred, y=obs) ) + xlim(range2) + ylim(range2) +
    xlab(expression(paste("Constructed ET (W/m"^"2",")"))) +
    ylab(bquote("ECOSTRESS "*.(ylabel)*" (W/"*m^2*")")) +
    # ggtitle(title) +
    geom_bin2d(binwidth = c(5, 5)) +
    
    scale_fill_gradientn(colors=c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100")) +
    
    geom_abline(intercept = 0, slope = 1, color="red",
                linetype="dashed", size=0.5) +
    
    annotate('text', x=range2[1]+(range2[2]-range2[1])/15, y=range2[2]-(range2[2]-range2[1])/15, label= figid,size=5) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13*4, label= bquote("r="*.(format(cor(df$obs,df$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 2.4) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13*3, label= bquote("MEF="*.(format(calMEF(df$obs,df$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 2.4) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13*2, label= bquote("RMSE="*.(format(calRMSE(df$obs,df$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 2.4) +
    annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/13, label= bquote("Bias="*.(format(calBias(df$obs,df$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 2.4) +
    
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text= element_text(size=9,color="black"),
          axis.title=element_text(size=10),
          plot.title = element_text(hjust = 0.5),
          # legend.key.size = unit(0.7, 'cm'),
          legend.key.height= unit(0.7, 'cm'),
          legend.key.width= unit(0.2, 'cm'))
}