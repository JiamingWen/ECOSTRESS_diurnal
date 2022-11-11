#for AGU meeting
setwd("C:/Users/jw2495/Downloads/ECOSTRESS/DTC")
setwd("/local/workdir/jw2495/ECOSTRESS/DTC")
lon0=-106.7020;lat0=34.3623;sitename="us_seg";offset0=0.15;UTC_OFFSET=-7

###########################################################################
#framework figure
source("DTC.R")

t.eval <- seq(0, 24*6-0.5, by=1)
t_max=8
t_ss=14
set.seed(1)
temps1 <- diurn(T_sr=10+runif(7,min=-1,max=1)*5, T_max=50+runif(6,min=-1,max=1)*5, t_max=t_max, t_ss=t_ss, t=t.eval)

num=0
num=num+1
jpeg(paste0("Figures/AGU/framework",num,".jpg"), width = 1500, height = 700,quality = 100,res=300)
op=par() #original par
par(oma=c(0.5,0.5,0.5,0.5),mar=c(0, 0, 0, 0))
eco_id=c(5,24*2+14,24*5+10)
plot(t.eval[eco_id]+5,temps1[eco_id]+3,xlab="",ylab='',frame=F,axes=F,col="red",pch=20,xlim=c(0,24*6),ylim=c(0,60),cex=1.5) #ECOSTRESS
axis(1,seq(0,168,24),labels = F,line = -1.4, lwd = 0, lwd.ticks = 1)
text(12,5.5,"Day1");text(60,5.5,"Day3");text(132,5.5,"Day6")
arrows(0,3,24*6+5,3,length=0.08,col="black")
arrows(0,3,0,60,length=0.08,col="black")
# lines(t.eval+5, temps1+3,col="red",lty=2) #ECOSTRESS fit dashed
lines(t.eval+5, temps1+3,col="red") #ECOSTRESS fit solid
points(t.eval+5, temps1,xlab="",ylab='',frame=F,axes=F,col="blue",pch=20,xlim=c(0,24*6),ylim=c(0,60),cex=1.5) #GOES
lines(t.eval+5, temps1,col="blue") #GOES fit
par(op) #restore par
dev.off()

plot(NULL,frame=F,axes=F,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1)) #empty plot
legend("center",legend = c("ECOSTRESS obs", "ECOSTRESS fit", "GOES obs", "GOES fit"), 
       lty=c(NA,1,NA,1),pch=c(20,NA,20,NA),pt.cex=c(1.5,NA,1.5,NA),col=c("red","red","blue","blue"),bty="n",ncol=2)

jpeg("Figures/AGU/framework_ET.jpg", width = 1500, height = 700,quality = 100,res=300)
op=par() #original par
par(oma=c(0.5,0.5,0.5,0.5),mar=c(0, 0, 0, 0))
eco_id=c(5,24*2+14,24*5+10)+5
diurnal_ET=c(rep(0,6),sin(seq(0,pi,length.out = 12)), rep(0,6))
ET1=c(diurnal_ET*50,diurnal_ET*49,diurnal_ET*48,diurnal_ET*46,diurnal_ET*55,diurnal_ET*48)+10
plot(t.eval[eco_id],ET1[eco_id],xlab="",ylab='',frame=F,axes=F,col="darkgreen",pch=20,xlim=c(0,24*6),ylim=c(0,70),cex=1.5)
axis(1,seq(0,168,24),labels = F,line = -1.4, lwd = 0, lwd.ticks = 1)
arrows(0,3,24*6+5,3,length=0.08,col="black")
arrows(0,3,0,60,length=0.08,col="black")
lines(t.eval, ET1,col="darkgreen")
par(op) #restore par
dev.off()


######################################################################################
# LST-region-revised
ReadGOESLST=function(year0,month0,date0,hour0,minute0){ #in UTC
  monthstr=str_pad(month0,2,pad="0")
  datestr=str_pad(date0,2,pad="0")
  current_time=as.POSIXlt(paste0(year0,"-",monthstr,"-",datestr,"_",hour0,"-",minute0),tz = "GMT",format="%Y-%m-%d_%H-%M")
  
  LSTfilelist=list.files("GOES_LST/ncfile",pattern = ".nc",full.names = F)
  LSTtimelist=as.POSIXlt(unlist(lapply(LSTfilelist,FUN = function(x){substr(x,5,23)})),tz = "GMT",format="%Y-%m-%d_%H-%M-%OS")
  LST_timedif=as.double(difftime(LSTtimelist,current_time,units="mins"))
  LST_two_bd=which((LST_timedif>-60)&(LST_timedif<=60))
  
  if (length(LST_two_bd)==2){
    LST1=raster(paste0("GOES_LST/ncfile/",LSTfilelist[LST_two_bd[1]]))
    LST2=raster(paste0("GOES_LST/ncfile/",LSTfilelist[LST_two_bd[2]]))
    LST=(LST1*abs(LST_timedif[LST_two_bd[2]])+LST2*abs(LST_timedif[LST_two_bd[1]]))/(abs(LST_timedif[LST_two_bd[2]])+abs(LST_timedif[LST_two_bd[1]]))
    return(LST)
  }else{
    return(NULL)
  }
  
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


library(raster)
library(ncdf4)
library(stringr)

# 2020-09-27 14:56
LST=raster("us-seg-region4new/Model/overpass/LST/2020_9_27_14_56obs.nc")-273.15
LSTpred=raster("us-seg-region4new/Model/overpass/LST/2020_9_27_14_56.nc")-273.15
# LSTpred[LSTpred[]<273.16]=NA
LST=projectRaster(LST,LSTpred)
# LST[is.na(LSTpred)]=NA
# LSTpred[is.na(LST)]=NA
df=na.omit(data.frame(obs=LST[],pred=LSTpred[]))
dif=LSTpred-LST

setwd("us-seg-region4new/")
GOES=ReadGOESLST(2020,9,27,21,56)-273.15
setwd("..")

jpeg("Figures/AGU/LST_region-updated.jpg", width = 2300, height = 1400,quality = 100,res=300)
# jpeg("Figures/LST_region-updated.jpg", width = 2000, height = 1400,quality = 100,res=300)
par(mfrow=c(2,3),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))

# LSTcolors=terrain.colors(10)[1:8]
# LSTcol=colorRampPalette(LSTcolors)(255)
# LSTcol=colorRampPalette(c("#000066", "blue", "cyan","yellow", "red", "#660000"))(255)
LSTcol=colorRampPalette(c("blue", "cyan","yellow", "red"))(255)

#LST
range0=range(c(LST[]),na.rm=T)+c(0,0.5)
breaks=seq(ceiling(range0[1]),floor(range0[2]),4)
plot(LST,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=LSTcol)
title("ECOSTRESS LST",line=0.5,cex.main = 1.5)
plot(LST,col=LSTcol, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2,at=breaks,labels=breaks),
     legend.args=list(text='\u00B0C', side=3, font=1, line=0, cex=1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels="(a)",cex=2)

LSTpred[LSTpred[]<range0[1]]=range0[1]
LSTpred[LSTpred[]>range0[2]]=range0[2]
plot(LSTpred,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=LSTcol)
title("Constructed LST",line=0.5,cex.main = 1.5)
plot(LSTpred,zlim=range0,col=LSTcol, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2,at=breaks,labels=breaks),
     legend.args=list(text='\u00B0C', side=3, font=1, line=0, cex=1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels="(b)",cex=2)

#GOES
if(!is.null(GOES)){
  GOES[GOES[]<range0[1]]=range0[1]
  GOES[GOES[]>range0[2]]=range0[2]
  plot(GOES,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=LSTcol)
  title("GOES LST",line=0.5,cex.main = 1.5)
  plot(GOES,zlim=range0,col=LSTcol, legend.only=TRUE,
       legend.width=3, legend.shrink=0.75,
       axis.args=list(cex.axis=1.2,at=breaks,labels=breaks),
       legend.args=list(text='\u00B0C', side=3, font=1, line=0, cex=1))
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels="(c)",cex=2)
}

#Residual

pal=colorRampPalette(c("red","white", "blue"))(100)
# range1=max(abs(dif[]),na.rm=T)*0.5
range1=6
dif[dif[]<(-range1)]=-range1
dif[dif[]>range1]=range1
plot(dif,axes=F,legend=FALSE,main="",col=pal,zlim=c(-range1,range1),colNA="grey")
title("Residual",line=0.5,cex.main = 1.5)
# par(mar=c(0, 2, 2, 4) + 0.1)
plot(dif, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text='\u00B0C', side=3, font=1, line=0, cex=1),col=pal,zlim=c(-range1,range1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels="(d)",cex=2)

#scatterplot
par(mar=c(0.5, 5, 0.5, 1) + 0.1)
range2=range(df)
# par(fig=c(0.78,1,0.6,0.98), new=TRUE)
par(fig=c(0.33,0.68,0.05,0.45), new=TRUE)
plot_colorByDensity(df$obs,df$pred,ylab="",xlab="",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
legend(12,58,legend = c(paste0("r=",format(cor(df$obs,df$pred),digits = 2,nsmall=2)),
                        paste0("MEF=",format(calMEF(df$obs,df$pred),digits = 2,nsmall=2)),
                        paste0("RMSE=",format(calRMSE(df$obs,df$pred),digits = 2,nsmall=2)),
                        paste0("Bias=",format(calBias(df$obs,df$pred),digits = 2,nsmall=2))),bty = "n",cex=1.1) #,adj=c(0,-0.3)
mtext("ECOSTRESS LST (\u00B0C)",2,line=2,cex=1) #ylab
mtext("Constructed LST (\u00B0C)",1,line=2.2,cex=1) #xlab
abline(0,1,lty=2,col="red")
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels="(e)",cex=2)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()


############################################################################
#ET-region-poster
ET=raster("us-seg-region4new/Model/overpass/ET/2020_9_27_14_56obs.nc")
ETpred=raster("us-seg-region4new/Model/overpass/ET/2020_9_27_14_56.nc")

# ET=projectRaster(ET,ETpred) #somehow different order shows a little different results in statistics
ETpred=projectRaster(ETpred,ET)
ET[is.na(ETpred[])]=NA
ETpred[is.na(ET[])]=NA
df=na.omit(data.frame(obs=ET[],pred=ETpred[]))
dif=ETpred-ET

jpeg("Figures/AGU/ET-region.jpg", width = 1800, height = 1500,quality = 100,res=300)
par(mfrow=c(2,2),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))

ETcolors = c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5")
ETcol=colorRampPalette(ETcolors)(255)


#ET
range0=range(c(ET[]),na.rm=T)
ET[ET[]<range0[1]]=range0[1] #use previous result in ET-region-top
ET[ET[]>range0[2]]=range0[2]
ET[1]=range0[1];ET[2]=range0[2]
plot(ET,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
title("ECOSTRESS ET*",line=0.5,cex.main = 1.2)
plot(ET,col=ETcol, legend.only=TRUE,
     legend.width=2, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(a)',cex=1.5)

#ETpred
ETpred[ETpred[]<range0[1]]=range0[1]
ETpred[ETpred[]>range0[2]]=range0[2]
ETpred[1]=range0[1];ETpred[2]=range0[2]
plot(ETpred,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
title("Constructed ET",line=0.5,cex.main = 1.2)
plot(ETpred,col=ETcol, legend.only=TRUE,
     legend.width=2, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(b)',cex=1.5)

#Residual
pal=colorRampPalette(c("red","white", "blue"))(100)
# range1=max(abs(dif[]),na.rm=T)*0.75
range1=20
dif[dif[]<(-range1)]=-range1
dif[dif[]>range1]=range1
plot(dif,axes=F,legend=FALSE,main="",col=pal,zlim=c(-range1,range1),colNA="grey")
title("Residual",line=0.5,cex.main = 1.2)
plot(dif, legend.only=TRUE,
     legend.width=2, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8),col=pal,zlim=c(-range1,range1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(c)',cex=1.5)

#scatterplot
par(mar=c(0.5, 5, 0.5, 1) + 0.1)
range2=range(df)
par(fig=c(0.5,1,0.05,0.45), new=TRUE)
plot_colorByDensity(df$obs,df$pred,ylab="",xlab="",xlim=range2+c(-10,10),ylim=range2+c(-10,10),main="")
legend(-50,750,legend = c(paste0("r=",format(cor(df$obs,df$pred),digits = 2,nsmall=2)),
                          paste0("MEF=",format(calMEF(df$obs,df$pred),digits = 2,nsmall=2)),
                          paste0("RMSE=",format(calRMSE(df$obs,df$pred),digits = 2,nsmall=2)),
                          paste0("Bias=",format(calBias(df$obs,df$pred),digits = 2,nsmall=2))),bty = "n",cex=0.9) #,adj=c(0,-0.3)
mtext(expression(paste("ECOSTRESS ET* (W/m"^"2",")")),2,line=2,cex=0.9) #ylab
mtext(expression(paste("Constructed ET (W/m"^"2",")")),1,line=2.5,cex=0.9) #xlab
abline(0,1,lty=2,col="red")
text(par('usr')[2]-(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(d)',cex=1.5)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()


###############################################################################
#animation of diurnal variation
varname="LST" #LST ET
hourrange=7:18
datestr="2020_9_27"
for (hour in hourrange){
  jpeg(paste0("Figures/AGU/animation/",varname,"/",varname,hour,".jpg"), width = 750, height = 650,quality = 100,res=300)
  op=par() #original par
  par(mar=c(1, 1, 2, 1) + 0.1,oma = c(0, 0, 0, 0))
  
  ras=raster(paste0("us-seg-region4new/Model/diurnal/",varname,"/",datestr,"_",hour,"_0.nc"))
  if (varname=="LST"){
    ras=ras-273.15
    col=colorRampPalette(c("blue", "cyan","yellow", "red"))(255)
    range0=c(15,50)
    breaks=seq(ceiling(range0[1]),floor(range0[2]),8)
    legend_label='\u00B0C'
  }else{
    ras[is.na(ras)]=0
    ETcolors = c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5")
    col=colorRampPalette(ETcolors)(255)
    range0=c(0,400)
    breaks=seq(0,400,100)
    legend_label=expression(paste("W/m"^"2"))
  }
  ras[ras[]<range0[1]]=range0[1]
  ras[ras[]>range0[2]]=range0[2]
  plot(ras,col=col,axes=F,legend=F,main="",colNA="grey",zlim=range0)
  title(paste0(varname," 2020/9/27 ",hour,":00"),line=0.5,cex.main = 0.7)
  # plot(ras,col=col, legend.only=TRUE,zlim=range0,
  #      legend.width=0.5, legend.shrink=0.5,
  #      axis.args=list(cex.axis=0.5,at=breaks,labels=breaks),
  #      legend.args=list(text=legend_label, side=3, font=1, line=0, cex=0.5))
  
  par(op) #restore par
  dev.off()
}

jpeg(paste0("Figures/AGU/animation/",varname,"_legend.jpg"), width = 750, height = 650,quality = 100,res=300)
op=par() #original par
par(mar=c(1, 1, 2, 1) + 0.1,oma = c(0, 0, 0, 0))
plot(ras,col=col, legend.only=TRUE,zlim=range0,
     legend.width=0.5, legend.shrink=0.5,
     axis.args=list(cex.axis=0.5,at=breaks,labels=breaks),
     legend.args=list(text=legend_label, side=3, font=1, line=0, cex=0.5))

par(op) #restore par
dev.off()


############################################################################
#reconstructed LST at tower site
source("time_decomp.R")
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

########################################################
DTC_site_plot=function(){
  library(suncalc)
  sunrise=time_decomp(getSunlightTimes(as.Date(startdatestr),lat0,lon0)$sunrise+UTC_OFFSET*3600)
  t_sr=sunrise[5]
  starttime=as.POSIXlt(paste0(startdatestr," ",floor(t_sr),":",(t_sr-floor(t_sr))*60))
  
  result_ts1=read.csv(paste0(RegionName,"/Tower/DTC_ts.csv"))
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
  plot(Tower_time,result_ts1$Tower_ts,type="l",pch=1,col="black",lwd=2,ylim=range0+c(0,2),xlab="",ylab="",axes = F,frame=T)
  axis(2)
  mtext("LST (\u00B0C)",2,line=2)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label)
  
  # lines(Tower_time, result_ts1$GOES_ts,col="blue")
  # points((t0_GOES+t_sr)/24, temps0_GOES,pch=20,col="deepskyblue",cex=1)
  lines(Tower_time, result_ts1$Eco_ts,col="red",lwd=2)
  points(Eco_time, result_loo1$Ecostress_obs,pch=20,col="red",cex=2)
  # points(Eco_time, result_loo1$leave_one_out_pred,pch=9,col="darkgreen",cex=1,lwd=1.5)
}

jpeg("Figures/AGU/LST_site.jpg", width = 2200, height = 2100,quality = 100,res=300)
op=par() #original par
# par(mfrow=c(3,1),mai = c(0.3, 0.5, 0.1, 0.7),mar=par()$mar+c(0,0,0,5), xpd=TRUE)

#August 4-14, 2018
par(fig=c(0.1,1,0.7,0.95),mai = c(0.3, 0.9, 0.2, 0.5),oma=c(0,0,0,0),mar=c(1.5, 1, 1, 1.2))
RegionName="us-seg-region2"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
DTC_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")

#April 7-17, 2020
par(fig=c(0.1,1,0.45,0.7), new=TRUE)
RegionName="us-seg-region5"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
DTC_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")

#September 26-October 5, 2020
par(fig=c(0.1,1,0.2,0.45), new=TRUE)
RegionName="us-seg-region4"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
DTC_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)")

par(fig=c(0.1,1,0,0.2), new=TRUE)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F)
# legend("topleft",legend = c("Tower LST","GOES LST","ECOSTRESS LST","GOES fitted","ECOSTRESS fitted","ECOSTRESS leave-one-out prediction"),col=c("black","blue","red","blue","red","red"),lty=c(1,NA,NA,1,1,NA),pch=c(NA,20,20,NA,NA,8),bty = "n",cex=0.9,text.width = c(0.3,0.3),ncol = 2,seg.len=2)
# legend("top",legend = c("GOES LST","ECOSTRESS LST","Leave-one-out prediction","Tower LST","Constructed LST"),col=c("deepskyblue","red","darkgreen","black","red"),lty=c(NA,NA,NA,1,1),pch=c(20,20,9,NA,NA),lwd=c(1,1,1.5,2,1),pt.cex=c(1,1.5,1,NA,NA),cex=0.9,text.width = c(0.3,0.3),ncol = 2,seg.len=2)
legend("top",legend = c("ECOSTRESS LST","Tower LST","Constructed LST"),col=c("red","black","red"),lty=c(NA,1,1),pch=c(20,NA,NA),lwd=c(1,2,2),pt.cex=c(2,NA,NA),cex=0.9,ncol = 1,seg.len=2)


par(op) #restore par
dev.off()


###########################################################################################
#ET-site
ET_site_plot=function(){
  Tower_data=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  ECOSTRESS_data=read.csv(paste0(RegionName,"/Tower/ECOSTRESS_ET.csv"))
  Tower_time=difftime(as.POSIXlt(paste0(Tower_data$local_year,"-",Tower_data$local_month,"-",Tower_data$local_date," ",Tower_data$local_hour,":",Tower_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  ECOSTRESS_time=difftime(as.POSIXlt(paste0(ECOSTRESS_data$local_year,"-",ECOSTRESS_data$local_month,"-",ECOSTRESS_data$local_date," ",ECOSTRESS_data$local_hour,":",ECOSTRESS_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  
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
  plot(Tower_time,Tower_data$ET_Tower,type="l",pch=1,col="black",ylim=range0,xlab="",ylab='',main="",axes = F,frame=T)
  axis(2)
  mtext(expression(paste("ET (W/m"^"2",")")),2,line = 2)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+3)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label)
  lines(Tower_time, Tower_data$ET_MERRA2_modfgfM,col="blue",lwd=1,lty=2)
  lines(Tower_time, Tower_data$ET_MERRA2_modfgfMfSM,col="red",lwd=2)
  # lines(Tower_time, Tower_data$ET_Tower_modfgfMfSM,col="purple")
  points(ECOSTRESS_time,ECOSTRESS_data$ETobs,pch=20,col="red",cex=2)
}

jpeg("Figures/AGU/ET_site.jpg", width = 2100, height = 2100,quality = 100,res=300)
op=par() #original par

#August 4-14, 2018
par(fig=c(0.1,1,0.7,0.95),mai = c(0.3, 0.9, 0.2, 0.5),oma=c(0,0,0,0),mar=c(1.5, 1, 1, 1.2))
RegionName="us-seg-region2"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
ET_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")

#April 7-17, 2020
par(fig=c(0.1,1,0.45,0.7), new=TRUE)
RegionName="us-seg-region5"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
ET_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")

#September 26-October 5, 2020
par(fig=c(0.1,1,0.2,0.45), new=TRUE)
RegionName="us-seg-region4"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
ET_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)")

par(fig=c(0.1,1,0,0.2), new=TRUE)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F)
legend(x = c(0.23, 0.68), y = c(0, 1),legend = c("ECOSTRESS ET","Constructed ET","Constructed ET(modified)","Tower ET"),col=c("red","blue","red","black"),lty=c(NA,2,1,1),lwd=c(NA,1,2,2),pch=c(20,NA,NA,NA),pt.cex=c(2,NA,NA,NA),cex=0.9,ncol = 1,seg.len=2)

par(op) #restore par
dev.off()
