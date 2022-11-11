#plot for revised manuscript 
#us-seg
setwd("C:/Users/jw2495/Downloads/ECOSTRESS/DTC/")
setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")
lon0=-106.7020;lat0=34.3623;sitename="us_seg";offset0=0.15;UTC_OFFSET=-7

library(raster)
library(ncdf4)
library(stringr)
library(ggplot2)
library(cowplot)

source("time_decomp.R")
source("plot_revise_fun.R")

#######################################################################################
#plot of land cover map
library(lattice)
library(rasterVis)
# cdl2=raster("us-seg-region2new/CDL/CDL_2018_reclassify.tif")
cdl2=raster("us-seg-region5new/CDL/CDL_2020_reclassify.tif")
landtype=c('Others','Cropland','Forest','Shrubland & Grassland','Urban','Wetland','Water')
cdl2=as.factor(cdl2)
rat <- levels(cdl2)[[1]]
rat[["landtype"]] <- landtype
levels(cdl2) <- rat

palette <- rev(c("#4FFFFF","#75B5EE","#FF1300","#E8E87A","#5dba5c",
                 "#BF56C3","#82342E"))

site <- data.frame(x=lon0, y=lat0)
coordinates(site) <- ~x+y

GOESlat=34.39493
GOESlon=-106.8062
GOESres=0.01 #half
x_coor=c(GOESlon-GOESres,GOESlon+GOESres,GOESlon+GOESres,GOESlon-GOESres)
y_coor=c(GOESlat+GOESres,GOESlat+GOESres,GOESlat-GOESres,GOESlat-GOESres)
xym <- cbind(x_coor, y_coor)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

jpeg("Figures_revised/CDL.jpg", width = 1350, height = 800,quality = 100,res=300)
op=par() #original par
par(mar=c(0,0,0,0),oma=c(0.2,0.2,0.2,0.2))
levelplot(cdl2,margin=F,colorkey=list(height=1, width=1.4),
          at=0:8,col.regions=palette,xlab="",ylab="")+
  # latticeExtra::layer(sp.points(site, pch=3, cex=1,lwd=2, col="red"), columns=1)+
  latticeExtra::layer(sp.points(site, pch=16, cex=0.8,lwd=2, col="black"), columns=1)+
  latticeExtra::layer(sp.text(c(lon0,lat0+0.02), cex=0.9, txt='US-Seg',font=list(face="bold")), columns=1) +
  latticeExtra::layer(sp.text(c(lon0-0.12,lat0-0.12), cex=1, txt='(a)',font=list(face="bold")), columns=1) +
  latticeExtra::layer(sp.points(sps, lwd=1, col="red"), columns=1)

par(op) #restore par
dev.off()


#################################################################################
#scatterplot between ECOSTRESS and GOES LST
jpeg(paste0("Figures_revised/GOES_ECOSTRESS.jpg"), width = 2500, height = 2000,quality = 100,res=300)
par(mfrow=c(1,3),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))

result_all=read.csv("us-seg-region2new/GOES_LST/compare/result.csv")
plot1=GOES_ECOSTRESS_scatter_plot(title="4-14 August 2018",plotid='(a)')

result_all=read.csv("us-seg-region5new/GOES_LST/compare/result.csv")
plot2=GOES_ECOSTRESS_scatter_plot(title="7-17 April 2020",plotid='(b)')

result_all=read.csv("us-seg-region4new/GOES_LST/compare/result.csv")
plot3=GOES_ECOSTRESS_scatter_plot(title="26 September to 5 October 2020",plotid='(c)')

result_all=read.csv("us-arm-region2new/GOES_LST/compare/result.csv")
plot4=GOES_ECOSTRESS_scatter_plot(title="15-19 June 2021",plotid='(d)')

plot_grid(plot1,plot2,plot3,plot4, nrow=2)
          
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()


####################################################################################
#LST-site - time series
jpeg("Figures_revised/LST_site.jpg", width = 2200, height = 2100,quality = 100,res=300)
op=par() #original par
# par(mfrow=c(3,1),mai = c(0.3, 0.5, 0.1, 0.7),mar=par()$mar+c(0,0,0,5), xpd=TRUE)

#August 4-14, 2018
par(fig=c(0.1,1,0.7,0.95),mai = c(0.3, 0.9, 0.2, 0.5),oma=c(0,0,0,0),mar=c(1.5, 1, 1, 1.2))
RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
LST_site_plot()
# rain_start=difftime(as.POSIXct('2018-08-10 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rain_end=difftime(as.POSIXct('2018-08-12 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rect(rain_start, 10, rain_end, 70, col = rgb(0.82,0.91,1,alpha=0.5), border = "transparent")
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")

#April 7-17, 2020
par(fig=c(0.1,1,0.45,0.7), new=TRUE)
RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
LST_site_plot()
# rain_start=difftime(as.POSIXct('2020-04-13 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rain_end=difftime(as.POSIXct('2020-04-14 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rect(rain_start, -20, rain_end, 70, col = rgb(0.82,0.91,1,alpha=0.5), border = "transparent")
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")

#September 26-October 5, 2020
par(fig=c(0.1,1,0.2,0.45), new=TRUE)
RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
LST_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)")

par(fig=c(0.1,1,0,0.2), new=TRUE)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F)
# legend("topleft",legend = c("Tower LST","GOES LST","ECOSTRESS LST","GOES fitted","ECOSTRESS fitted","ECOSTRESS leave-one-out prediction"),col=c("black","blue","red","blue","red","red"),lty=c(1,NA,NA,1,1,NA),pch=c(NA,20,20,NA,NA,8),bty = "n",cex=0.9,text.width = c(0.3,0.3),ncol = 2,seg.len=2)
legend("top",legend = c("GOES LST","ECOSTRESS LST","Leave-one-out estimation","Tower LST","Constructed LST"),col=c("deepskyblue","red","darkgreen","black","red"),lty=c(NA,NA,NA,1,1),pch=c(20,20,9,NA,NA),lwd=c(1,1,1.5,2,1),pt.cex=c(1,1,1,NA,NA),cex=0.9,text.width = c(0.3,0.3),ncol = 2,seg.len=2)


par(op) #restore par
dev.off()

###############################################################
#LST-site-scatter
fluxfolder=list.dirs("us-seg-region5new/Tower",full.names = T,recursive=F)
fluxfile=list.files(fluxfolder,pattern = 'AMF_.*\\.csv',full.names = T)
Tower_data0=read.csv(file=fluxfile,header = T,skip=2,na.strings = -9999)

jpeg(paste0("Figures_revised/LST_site_scatter.jpg"), width = 2500, height = 800,quality = 100,res=300)
par(mfrow=c(1,3),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))

RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
DTC_site_scatterplot()
title("4-14 August 2018",line=0.5,cex.main = 1.3)
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(a)',cex=1.5)

RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
DTC_site_scatterplot()
title("7-17 April 2020",line=0.5,cex.main = 1.3)
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(b)',cex=1.5)

RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
DTC_site_scatterplot()
title("26 September to 5 October 2020",line=0.5,cex.main = 1.3)
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(c)',cex=1.5)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()


###########################################################################################
#ET-site time series
jpeg("Figures_revised/ET_site.jpg", width = 2100, height = 2100,quality = 100,res=300)
op=par() #original par

#August 4-14, 2018
par(fig=c(0.1,1,0.7,0.95),mai = c(0.3, 0.9, 0.2, 0.5),oma=c(0,0,0,0),mar=c(1.5, 1, 1, 1.2))
RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
ET_site_plot()
# rain_start=difftime(as.POSIXct('2018-08-10 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rain_end=difftime(as.POSIXct('2018-08-12 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rect(rain_start, -100, rain_end, 700, col = rgb(0.82,0.91,1,alpha=0.5), border = "transparent")
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")

#April 7-17, 2020
par(fig=c(0.1,1,0.45,0.7), new=TRUE)
RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
ET_site_plot()
# rain_start=difftime(as.POSIXct('2020-04-13 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rain_end=difftime(as.POSIXct('2020-04-14 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
# rect(rain_start, -100, rain_end, 700, col = rgb(0.82,0.91,1,alpha=0.5), border = "transparent")
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")

#September 26-October 5, 2020
par(fig=c(0.1,1,0.2,0.45), new=TRUE)
RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
ET_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)")

par(fig=c(0.1,1,0.02,0.20), new=TRUE)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F,frame=T)
# legend(x = c(0.05, 0.97), y = c(0.2, 1),legend = c("ECOSTRESS ET","ECOSTRESS ET*",NA,"Tower ET","Constructed ET",expression('Constructed ET (modified f'[SM]*')')),col=c("red","darkgreen",NA,"black","blue","red"),lty=c(NA,NA,NA,1,1,1),pch=c(20,18,NA,NA,NA,NA),cex=0.9,text.width = 0,ncol = 2,seg.len=2)
# legend(x = c(0.0, 1), y = c(0.2, 1),legend = c("ECOSTRESS ET","ECOSTRESS ET*","Tower ET","Constructed ET",expression('Constructed ET (modified f'[SM]*')'),expression('Constructed ET (modified f'[SM]*', tower met)')),col=c("red","darkgreen","black","blue","red","purple"),lty=c(NA,NA,1,1,1,1),pch=c(20,18,NA,NA,NA,NA),cex=0.9,text.width = c(10,20),ncol = 2,seg.len=2)
legend(x = c(0.0, 0.2), y = c(0.2, 1),legend = c("ECOSTRESS ET","ECOSTRESS ET*","Tower ET"),col=c("red","darkgreen","black"),lty=c(NA,NA,1),pch=c(20,18,NA),cex=0.9,ncol = 1,seg.len=2,bty="n")
legend(x = c(0.4, 1), y = c(0.2, 1),legend = c("Constructed ET",expression('Constructed ET (modified f'[SM]*')'),expression('Constructed ET (modified f'[SM]*', tower met)')),col=c("blue","red","limegreen"),lty=c(1,1,1),pch=c(NA,NA,NA),cex=0.9,ncol = 1,seg.len=2,bty="n")


par(op) #restore par
dev.off()

###############################################################
#ET-site-scatter
jpeg(paste0("Figures_revised/ET_site_scatter.jpg"), width = 2500, height = 800,quality = 100,res=300)
par(mfrow=c(1,3),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))

RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
ET_site_scatterplot()
title("4-14 August 2018",line=0.5,cex.main = 1.3)
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(a)',cex=1.5)

RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
ET_site_scatterplot()
title("7-17 April 2020",line=0.5,cex.main = 1.3)
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(b)',cex=1.5)

RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
ET_site_scatterplot()
title("26 September to 5 October 2020",line=0.5,cex.main = 1.3)
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(c)',cex=1.5)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

########################################################################################
jpeg("Figures_revised/ET_site_sup.jpg", width = 2100, height = 2100,quality = 100,res=300)
op=par() #original par

#August 4-14, 2018
par(fig=c(0.1,1,0.7,0.95),mai = c(0.3, 0.9, 0.2, 0.5),oma=c(0,0,0,0),mar=c(1.5, 1, 1, 1.2))
RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
ET_site_sup_plot()
rain_start=difftime(as.POSIXct('2018-08-10 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
rain_end=difftime(as.POSIXct('2018-08-12 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
rect(rain_start, -100, rain_end, 700, col = rgb(0.82,0.91,1,alpha=0.5), border = "transparent")
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")

#April 7-17, 2020
par(fig=c(0.1,1,0.45,0.7), new=TRUE)
RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
ET_site_sup_plot()
rain_start=difftime(as.POSIXct('2020-04-13 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
rain_end=difftime(as.POSIXct('2020-04-14 00:00'),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
rect(rain_start, -100, rain_end, 700, col = rgb(0.82,0.91,1,alpha=0.5), border = "transparent")
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")

#September 26-October 5, 2020
par(fig=c(0.1,1,0.2,0.45), new=TRUE)
RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
ET_site_sup_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)")

par(fig=c(0.1,1,0,0.2), new=TRUE)
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F)
legend(x = c(0.23, 0.68), y = c(0.2, 1),legend = c("Tower ET","Constructed ET(MERRA-2)","Constructed ET(Tower)"),col=c("black","blue","saddlebrown"),lty=c(1,1,1),pch=c(NA,NA,NA),cex=0.9,text.width = c(0.3,0.3),ncol = 1,seg.len=2)

par(op) #restore par
dev.off()


#######################################################################################
#LST-region
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

jpeg("Figures_revised/LST_region.jpg", width = 2300, height = 1400,quality = 100,res=300)
par(mfrow=c(2,3),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))

LSTcol=colorRampPalette(c("blue", "cyan","yellow", "red"))(255)

#LST
range0=range(c(LST[]),na.rm=T)
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

library(gridBase)
library(grid)
par(fig=c(0.33,0.75,0,0.5),mar=c(0, 5, 0.1, 1) , new=TRUE)
# plot.new()
vps <- baseViewports()
pushViewport(vps$figure)
vp1 <-plotViewport(c(1.8,1,0,1))
# vp1 <-viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), 
#                just=c("left","top"), 
#                y=0.5, x=0.5)

range2=range(df)
p=ggplot(df, aes(x=pred, y=obs) ) + xlim(range2) + ylim(range2) +
  xlab("Constructed LST (\u00B0C)") + ylab("ECOSTRESS LST (\u00B0C)") +
  # ggtitle(title) +
  geom_bin2d(binwidth = c(0.1, 0.1)) +

  scale_fill_gradientn(colors=c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100")) +
  
  geom_abline(intercept = 0, slope = 1, color="red",
              linetype="dashed", size=0.5) +

  annotate('text', x=range2[1]+(range2[2]-range2[1])/15, y=range2[2]-(range2[2]-range2[1])/15, label= '(e)',size=5) +
  annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*4, label= paste0("r=",format(cor(df$obs,df$pred),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 2.5) +
  annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*3, label= paste0("MEF=",format(calMEF(df$obs,df$pred),digits = 2,nsmall=2)),hjust=0,parse=F,colour = "black", size = 2.5) +
  annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*2, label= paste0("RMSE=",format(calRMSE(df$obs,df$pred),digits = 2,nsmall=2),"\u00B0C"),hjust=0,parse=F,colour = "black", size = 2.5) +
  annotate('text', x=range2[2]-(range2[2]-range2[1])/2.4, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10, label= paste0("Bias=",format(calBias(df$obs,df$pred),digits = 2,nsmall=2),"\u00B0C"),hjust=0,parse=F,colour = "black", size = 2.5) +
  
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text= element_text(size=10, colour = "black"),
        axis.title=element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        legend.key.width= unit(0.4, 'cm'))
print(p,vp = vp1) 

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

#######################################################################################
#LST-region-scatter
jpeg(paste0("Figures_revised/LST_region_scatter.jpg"), width = 3000, height = 800,quality = 100,res=300)
# par(mfrow=c(1,3),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))

result_all=read.csv("us-seg-region2new/Model/overpass/LST/scatter.csv")
plot1=LST_region_scatter_plot(title="4-14 August 2018",plotid='(a)')

result_all=read.csv("us-seg-region5new/Model/overpass/LST/scatter.csv")
result_all[result_all>60+273.15]=NA
result_all=na.omit(result_all)
plot2=LST_region_scatter_plot(title="7-17 April 2020",plotid='(b)')

result_all=read.csv("us-seg-region4new/Model/overpass/LST/scatter.csv")
plot3=LST_region_scatter_plot(title="26 September to 5 October 2020",plotid='(c)')

plot_grid(plot1,plot2,plot3, nrow=1)
# par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()


#########################################################################################
#ET-region-scatter
jpeg(paste0("Figures_revised/ET_region_scatter.jpg"), width = 3200, height = 800,quality = 100,res=300)
# par(mfrow=c(1,3),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))

result_all=read.csv("us-seg-region2new/Model/overpass/ET/scatter_pred_comp.csv")
plot1=ET_region_scatter_plot(title="4-14 August 2018",plotid='(a)')

result_all=read.csv("us-seg-region5new/Model/overpass/ET/scatter_pred_comp.csv")
# result_all[result_all>60+273.15]=NA
# result_all=na.omit(result_all)
plot2=ET_region_scatter_plot(title="7-17 April 2020",plotid='(b)')

result_all=read.csv("us-seg-region4new/Model/overpass/ET/scatter_pred_comp.csv")
plot3=ET_region_scatter_plot(title="26 September to 5 October 2020",plotid='(c)')

plot_grid(plot1,plot2,plot3, nrow=1)
# par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

# ############################################################################################
# #ET-region
# #2018_8_7_13_20
# ETcolors = c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5")
# ETcol=colorRampPalette(ETcolors)(255)
# 
# # ET-region-top
# ET=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20observation.nc")
# ETpred=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20.nc")
# 
# ET=projectRaster(ET,ETpred)
# ET[is.na(ETpred[])]=NA
# ETpred[is.na(ET[])]=NA
# df=na.omit(data.frame(obs=ET[],pred=ETpred[]))
# dif=ETpred-ET
# 
# jpeg(paste0("Figures_revised/ET_region1.jpg"), width = 3000, height = 750,quality = 100,res=300)
# par(mfrow=c(1,4),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
# 
# #in the ET obs legend
# range0=range(c(ET[]),na.rm=T)
# plot(ET,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
# title("ECOSTRESS ET",line=1,cex.main = 1.5)
# plot(ET,col=ETcol, legend.only=TRUE,
#      legend.width=3, legend.shrink=0.75,
#      axis.args=list(cex.axis=1.2),
#      legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
# text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(a)',cex=2)
# 
# ETpred[ETpred[]<range0[1]]=range0[1]
# ETpred[ETpred[]>range0[2]]=range0[2]
# ETpred[1]=range0[1];ETpred[2]=range0[2]
# plot(ETpred,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
# title("Constructed ET",line=1,cex.main = 1.5)
# plot(ETpred,col=ETcol, legend.only=TRUE,
#      legend.width=3, legend.shrink=0.75,
#      axis.args=list(cex.axis=1.2),
#      legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
# text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(b)',cex=2)
# 
# #Residual
# pal=colorRampPalette(c("red","white", "blue"))(100)
# # range1=max(abs(dif[]),na.rm=T)*0.75
# range1=200
# dif[dif[]<(-range1)]=-range1
# dif[dif[]>range1]=range1
# plot(dif,axes=F,legend=FALSE,main="",col=pal,zlim=c(-range1,range1),colNA="grey")
# title("Residual",line=1,cex.main = 1.5)
# plot(dif, legend.only=TRUE,
#      legend.width=3, legend.shrink=0.75,
#      axis.args=list(cex.axis=1.2),
#      legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8),col=pal,zlim=c(-range1,range1))
# text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(c)',cex=2)
# 
# #scatterplot
# library(gridBase)
# library(grid)
# par(fig=c(0.73,1,0,0.85),mar=c(0, 1.8, 0, 0),oma=c(0,0,0,0) , new=TRUE)
# # plot.new()
# vps <- baseViewports()
# pushViewport(vps$figure)
# vp1 <-plotViewport(c(1.8,1,0,1))
# # vp1 <-viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), 
# #                just=c("left","top"), 
# #                y=0.5, x=0.5)
# 
# range2=range(df)
# p=ggplot(df, aes(x=pred, y=obs) ) + xlim(range2) + ylim(range2) +
#   xlab(expression(paste("Constructed ET (W/m"^"2",")"))) + ylab(expression(paste("ECOSTRESS ET (W/m"^"2",")"))) +
#   # ggtitle(title) +
#   geom_bin2d(binwidth = c(5, 5)) +
#   
#   scale_fill_gradientn(colors=c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100")) +
#   
#   geom_abline(intercept = 0, slope = 1, color="red",
#               linetype="dashed", size=0.5) +
#   
#   annotate('text', x=range2[1]+(range2[2]-range2[1])/15, y=range2[2]-(range2[2]-range2[1])/15, label= '(d)',size=5) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/3.2, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*4, label= bquote("r="*.(format(cor(df$obs,df$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 2) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/3.2, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*3, label= bquote("MEF="*.(format(calMEF(df$obs,df$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 2) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*2, label= bquote("RMSE="*.(format(calRMSE(df$obs,df$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 2) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10, label= bquote("Bias="*.(format(calBias(df$obs,df$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 2) +
#   
#   theme_bw() + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
#   theme(axis.text= element_text(size=7,color="black"),
#         axis.title=element_text(size=9),
#         plot.title = element_text(hjust = 0.5),
#         # legend.key.size = unit(0.7, 'cm'),
#         legend.key.height= unit(0.7, 'cm'),
#         legend.key.width= unit(0.2, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(0,0,0,0),
#         plot.margin = unit(c(0,0,0,0), "cm"))
# print(p,vp = vp1) 
# 
# par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
# dev.off()
# 
# #########################################
# #ET-region-bottom
# ET=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20obs.nc")
# ETpred=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20.nc")
# 
# # ET=projectRaster(ET,ETpred) #somehow different order shows a little different results in statistics
# ETpred=projectRaster(ETpred,ET)
# ET[is.na(ETpred[])]=NA
# ETpred[is.na(ET[])]=NA
# df=na.omit(data.frame(obs=ET[],pred=ETpred[]))
# dif=ETpred-ET
# 
# jpeg(paste0("Figures_revised/ET_region2.jpg"), width = 3000, height = 750,quality = 100,res=300)
# par(mfrow=c(1,4),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
# 
# #in the ET obs legend
# ET[ET[]<range0[1]]=range0[1] #use previous result in ET-region-top
# ET[ET[]>range0[2]]=range0[2]
# ET[1]=range0[1];ET[2]=range0[2]
# plot(ET,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
# title("ECOSTRESS ET*",line=1,cex.main = 1.5)
# plot(ET,col=ETcol, legend.only=TRUE,
#      legend.width=3, legend.shrink=0.75,
#      axis.args=list(cex.axis=1.2),
#      legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
# text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(e)',cex=2)
# 
# plot(NULL,frame=F,axes=F,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1)) #empty plot
# 
# #Residual
# pal=colorRampPalette(c("red","white", "blue"))(100)
# # range1=max(abs(dif[]),na.rm=T)*0.75
# range1=100
# dif[dif[]<(-range1)]=-range1
# dif[dif[]>range1]=range1
# plot(dif,axes=F,legend=FALSE,main="",col=pal,zlim=c(-range1,range1),colNA="grey")
# title("Residual",line=1,cex.main = 1.5)
# plot(dif, legend.only=TRUE,
#      legend.width=3, legend.shrink=0.75,
#      axis.args=list(cex.axis=1.2),
#      legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8),col=pal,zlim=c(-range1,range1))
# text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(f)',cex=2)
# 
# #scatterplot
# library(gridBase)
# library(grid)
# par(fig=c(0.73,1,0,0.85),mar=c(0, 2, 0, 0),oma=c(0,0,0,0) , new=TRUE)
# # plot.new()
# vps <- baseViewports()
# pushViewport(vps$figure)
# vp1 <-plotViewport(c(1.8,1,0,1))
# # vp1 <-viewport(height=unit(.5, "npc"), width=unit(0.5, "npc"), 
# #                just=c("left","top"), 
# #                y=0.5, x=0.5)
# 
# range2=range(df)
# RMSE=format(calRMSE(df$obs,df$pred),digits = 2,nsmall=2) #31.46
# RMSE
# Bias=format(calBias(df$obs,df$pred),digits = 2,nsmall=2) #-14.07
# Bias
# p=ggplot(df, aes(x=pred, y=obs) ) + xlim(range2) + ylim(range2) +
#   xlab(expression(paste("Constructed ET (W/m"^"2",")"))) + ylab(expression(paste("ECOSTRESS ET* (W/m"^"2",")"))) +
#   # ggtitle(title) +
#   geom_bin2d(binwidth = c(5, 5)) +
#   
#   scale_fill_gradientn(colors=c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100")) +
#   
#   geom_abline(intercept = 0, slope = 1, color="red",
#               linetype="dashed", size=0.5) +
#   
#   annotate('text', x=range2[1]+(range2[2]-range2[1])/15, y=range2[2]-(range2[2]-range2[1])/15, label= '(g)',size=5) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/3.2, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*4, label= bquote("r="*.(format(cor(df$obs,df$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 2) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/3.2, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*3, label= bquote("MEF="*.(format(calMEF(df$obs,df$pred),digits = 2,nsmall=2))),hjust=0,parse=F,colour = "black", size = 2) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10*2, label= bquote("RMSE="*.(format(calRMSE(df$obs,df$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 2) +
#   annotate('text', x=range2[2]-(range2[2]-range2[1])/1.9, y=range2[1]-(range2[2]-range2[1])/15+(range2[2]-range2[1])/10, label= bquote("Bias="*.(format(calBias(df$obs,df$pred),digits = 2,nsmall=2))*"W/"*m^2),hjust=0,parse=F,colour = "black", size = 2) +
#   
#   theme_bw() + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
#   theme(axis.text= element_text(size=7,color="black"),
#         axis.title=element_text(size=9),
#         plot.title = element_text(hjust = 0.5),
#         # legend.key.size = unit(1, 'cm'),
#         legend.key.height= unit(0.6, 'cm'),
#         legend.key.width= unit(0.2, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(0,0,0,0),
#         plot.margin = unit(c(0,0,0,0), "cm"))
# print(p,vp = vp1) 
# 
# par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
# dev.off()


###################################################################################################
#ET-region-new (left for maps and right for scatterplots) #2018_8_7_13_20
#########################################################
#ET-region-left
ETcolors = c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5")
ETcol=colorRampPalette(ETcolors)(255)

#top
ET=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20observation.nc")
ETpred=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20.nc")

ET=projectRaster(ET,ETpred)
ET[is.na(ETpred[])]=NA
ETpred[is.na(ET[])]=NA
df1=na.omit(data.frame(obs=ET[],pred=ETpred[]))
dif=ETpred-ET

jpeg(paste0("Figures_revised/ET_region_left.jpg"), width = 2200, height = 1300,quality = 100,res=300)
par(mfrow=c(2,3),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))

#in the ET obs legend
range0=range(c(ET[]),na.rm=T)
plot(ET,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
title("ECOSTRESS ET",line=0.5,cex.main = 1.5)
plot(ET,col=ETcol, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(a)',cex=2)

ETpred[ETpred[]<range0[1]]=range0[1]
ETpred[ETpred[]>range0[2]]=range0[2]
ETpred[1]=range0[1];ETpred[2]=range0[2]
plot(ETpred,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
title("Constructed ET",line=0.5,cex.main = 1.5)
plot(ETpred,col=ETcol, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(b)',cex=2)

#Residual
pal=colorRampPalette(c("red","white", "blue"))(100)
# range1=max(abs(dif[]),na.rm=T)*0.75
range1=200
dif[dif[]<(-range1)]=-range1
dif[dif[]>range1]=range1
plot(dif,axes=F,legend=FALSE,main="",col=pal,zlim=c(-range1,range1),colNA="grey")
title("Residual",line=0.5,cex.main = 1.5)
plot(dif, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8),col=pal,zlim=c(-range1,range1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(c)',cex=2)


#########################################
#bottom
ET=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20obs.nc")
ETpred=raster("us-seg-region2new/Model/overpass/ET/2018_8_7_13_20.nc")

# ET=projectRaster(ET,ETpred) #somehow different order shows a little different results in statistics
ETpred=projectRaster(ETpred,ET)
ET[is.na(ETpred[])]=NA
ETpred[is.na(ET[])]=NA
df2=na.omit(data.frame(obs=ET[],pred=ETpred[]))
dif=ETpred-ET

#in the ET obs legend
ET[ET[]<range0[1]]=range0[1] #use previous result in #top
ET[ET[]>range0[2]]=range0[2]
ET[1]=range0[1];ET[2]=range0[2]
plot(ET,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
title("ECOSTRESS ET*",line=0.5,cex.main = 1.5)
plot(ET,col=ETcol, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(e)',cex=2)

plot(NULL,frame=F,axes=F,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1)) #empty plot

#Residual
pal=colorRampPalette(c("red","white", "blue"))(100)
# range1=max(abs(dif[]),na.rm=T)*0.75
range1=100
dif[dif[]<(-range1)]=-range1
dif[dif[]>range1]=range1
plot(dif,axes=F,legend=FALSE,main="",col=pal,zlim=c(-range1,range1),colNA="grey")
title("Residual",line=0.5,cex.main = 1.5)
plot(dif, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8),col=pal,zlim=c(-range1,range1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(f)',cex=2)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

#########################################################
#ET-region-right
jpeg(paste0("Figures_revised/ET_region_right.jpg"), width = 900, height = 1300,quality = 100,res=300)
par(mfrow=c(2,1),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
plot1=ET_site_heatmap(df1,"ET","(d)")
plot2=ET_site_heatmap(df2,"ET*","(g)")
plot_grid(plot1,plot2, nrow=2)
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()
       
###################################################################################
#met-comp
#August 4-14, 2018
RegionName="us-seg-region2"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
figid=1
met_comp_plot()

#April 7-17, 2020
RegionName="us-seg-region5"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
figid=2
met_comp_plot()

#Sept 26 - Oct 5, 2020
RegionName="us-seg-region4"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
figid=3
met_comp_plot()

#######################################################################################
#SW_IN, cloud fraction, precipitation

cloud_plot=function(){
  # par(mar=c(4,4,4,4))
  result_ts1=read.csv(paste0(RegionName,"/Tower/DTC_ts.csv"))
  cloud_data=read.csv(paste0(RegionName,"/Tower/cloud.csv"))
  
  if (nrow(result_ts1)!=nrow(cloud_data)){
    stop("Check the data")
  }
  
  Tower_time=difftime(as.POSIXct(paste0(result_ts1$local_year,"-",result_ts1$local_month,"-",result_ts1$local_date," ",result_ts1$local_hour,":",result_ts1$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  # plot(Tower_time,Tower_data$SW_IN,type="l",pch=1,col="red",lwd=2,xlab="",ylab="",axes = F,frame=T)
  # axis(2,col="red")
  # mtext(expression(paste("Shortwave incoming radiation (W/m"^"2",")")),2,line=2,col="red")
  # par(new = TRUE)
  plot(Tower_time,cloud_data$cloud_fraction,type="l",pch=1,col="black",lwd=2,xlab="",ylab="",ylim=c(0,1.7),axes = F,frame=T)
  axis(2,col="black", col.axis = "black",at=seq(0,1,0.5),las=2)
  mtext("Cloud Fraction",2,line=2.5,col="black",at=0.5)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label)
  abline(h = 0.2,lty=2,col="orange")
  
  if (RegionName %in% c("us-seg-region2new","us-seg-region5new")){
    par(new = TRUE)
    precip_range=range(c(0,cloud_data$precip,1),na.rm=T)
    plot(NULL,lwd=2,xlab="",ylab="",xlim=range(Tower_time),ylim=rev(precip_range)*3,axes = F,frame=T, yaxs = "i")
    segments(Tower_time,rep(0,length(cloud_data$precip)),Tower_time,cloud_data$precip,col="dodgerblue")
    axis(4,col="dodgerblue", col.axis = "dodgerblue", at=seq(0,floor(precip_range[2]),length.out = 3),las=2)
    mtext("Precipitation (mm)",4,line=2.5,col="dodgerblue",at=precip_range[2])
  }
}


jpeg("Figures_revised/cloud.jpg", width = 2100, height = 2100,quality = 100,res=300)
op=par() #original par

#August 4-14, 2018
par(fig=c(0.1,1,0.7,1),oma=c(0,0,0,0),mar=c(1,2,2,4))
RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
cloud_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")

#April 7-17, 2020
par(fig=c(0.1,1,0.4,0.7), new=TRUE)
RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
cloud_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")

#September 26-October 5, 2020
par(fig=c(0.1,1,0.1,0.4), new=TRUE)
RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
cloud_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)")

par(op) #restore par
dev.off()


#####################################################################################
#diurnal variation of the constructed LST and ET for US-Seg region
jpeg("/local/workdir/jw2495/ECOSTRESS/DTC/Figures_revised/diurnal_LC.jpg", width = 2000, height = 920,quality = 100,res=300)
op=par() #original par
par(mfrow=c(1,2),mar=c(1,0.5,1,0.5),mai=c(0.7,0.7,0.2,0.2))

#LST
varname="LST" #LST ET
result=read.csv(paste0("us-seg-region4new/revision/diurnal",varname,"_LCs_20200927.csv"))
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
text(7,42,"(a)",cex=1.2)
legend(9,27,rev(c('Cropland','Forest','Shrubland & Grassland','Urban','Wetland','Water')),
       col=c("#4FFFFF","#75B5EE","#FF1300","#d4af37","#5dba5c","#BF56C3"),lty=1,lwd=2,bty="n",cex=0.7)

#ET
varname="ET" #LST ET
result=read.csv(paste0("us-seg-region4new/revision/diurnal",varname,"_LCs_20200927.csv"))
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
text(7,210,"(b)",cex=1.2)

par(op) #restore par
dev.off()


###########################################################################################
#diurnal cycle of constructed LST within a coarse GOES pixel
library(lattice)
library(rasterVis)
lon0=-106.7020;lat0=34.3623;RegionName="us-seg-region4new";sitename="us_seg";offset0=0.15;UTC_OFFSET=-7
setwd(RegionName)

#pixel coor
result=read.csv("revision/GOES_LCfrac.csv")
tmp=(result$lat-34.40)^2+(result$lon-(-106.8))^2
lat1=result[which(tmp==min(tmp)),1] #34.39493
lon1=result[which(tmp==min(tmp)),2] #-106.8062
radius=0.01
tmp=raster(xmn=lon1-radius,xmx=lon1+radius,ymn=lat1-radius,ymx=lat1+radius,res=radius)

###############################################################
#left
#CDL
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

jpeg(paste0("../Figures_revised/diurnalLST_withinGOES1.jpg"), width = 1350, height = 800,quality = 100,res=300)
op=par() #original par
levelplot(cdl2_cp,margin=F,colorkey=list(height=1, width=1.4),
          at=0:6,col.regions=palette,xlab="",ylab="") +
  latticeExtra::layer(sp.text(c(lon1-0.008,lat1-0.008), cex=1, txt='(a)'), columns=1)

par(op) #restore par
dev.off()
reset_par()

###############################################################
#right
jpeg(paste0("../Figures_revised/diurnalLST_withinGOES2.jpg"), width = 1460, height = 800,quality = 100,res=300)
op=par() #original par
par(mfrow=c(1,2),mar=c(2, 1, 1, 3) + 0.1,oma = c(1,0,0,0))

#ECOSTRESS LST
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

plot(ECOSTRESS_cp-273.15,axes=F,legend=FALSE,main="",colNA="grey",col=LSTcol)
# title("ECOSTRESS LST",line=0.5,cex.main = 1.5)
plot(ECOSTRESS_cp-273.15,col=LSTcol, legend.only=TRUE,
     legend.width=2, legend.shrink=0.75,
     axis.args=list(cex.axis=0.8),
     legend.args=list(text='\u00B0C', side=3, font=1, line=0, cex=1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels="(b)",cex=1.2)


#diurnal cycle
par(fig=c(0.5,1,0.1,1),mai = c(0,0.3,0,0),oma=c(2,3,1,0), new=TRUE)

result=read.csv("revision/diurnalLST_LCs_20200930_singlepixel.csv")
time0=result$hour+result$minute/60
plot(time0,result$`Shrubland...Grassland`-273.15,type="l",col="darkgoldenrod",xlab="", ylab="",xlim=c(6,18),cex.axis=0.8)
mtext("Hour",1,line=2,cex=0.8)
mtext("LST (\u00B0C)",2,line=2,cex=0.8)
lines(time0,result$Wetland-273.15,col="#75B5EE")
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
legend("bottomright",c("Constructed LST (SHR/GRA)", "Constructed LST (WET)",
                       "ECOSTRESS LST (SHR/GRA)", "ECOSTRESS LST (WET)",
                       "Pure GOES LST (SHR/GRA)"),
       col=c("darkgoldenrod","#75B5EE","darkgoldenrod","#75B5EE","blue"),
       lty=c(1,1,NA,NA,NA),
       pch=c(NA,NA,8,8,20),bty="n",cex=0.5)
text(7,40,"(c)",cex=1.2)

par(op) #restore par
dev.off()

setwd("..")

########################################################################################
#DTC schematic graph
source("DTC.R")
jpeg("Figures_revised/DTC_graph.jpg", width = 1200, height = 800,quality = 100,res=300)
op=par() #original par

par(oma=c(1,2,0,0.5),mar=c(1, 1, 1, 2))
t.eval <- seq(0, 24-0.5, by=0.5)
T_sr_d0=11
T_sr_d1=13
T_max=50
t_max=8
t_ss=14
temps1 <- diurn(T_sr=c(T_sr_d0,T_sr_d1), T_max=c(T_max), t_max=t_max, t_ss=t_ss, t=t.eval)
plot(t.eval, temps1, type='l',xlab="",ylab='',frame=T,axes=F)
axis(1,cex.axis=0.8,tck=-0.02,mgp=c(3,0.2,0))
axis(2,cex.axis=0.8,tck=-0.02,mgp=c(3,0.5,0),las=2)
mtext("Time after the sunrise (hr)",1,1,cex=1)
mtext("LST (\u00B0C)",2,1.5,cex=1)

abline(h=T_max,col="black",lwd=0.5)
abline(h=T_sr_d0,col="black",lwd=0.5)
abline(h=T_sr_d1,col="black",lwd=0.5)
abline(v=0,col="black",lwd=0.5)
abline(v=t_max,col="black",lwd=0.5)
abline(v=t_ss-1,col="black",lwd=0.5)
abline(v=t_ss,col="black",lwd=0.5)
abline(v=24-0.5,col="black",lwd=0.5)

T_a <- (T_max - T_sr_d0) / (cos(pi/4)+1)
T_0 <- T_sr_d0 + T_a * cos(pi/4)

abline(h=T_0,col="black",lty=2,lwd=0.5)

axis(4,at=c(T_sr_d0-1,T_sr_d1,T_0,T_max),tick=F,las=2,line=-0.8,cex.axis=0.8,
     labels = c(expression('T'['sr,d']),expression('T'['sr,d+1']),expression('T'[0]),expression('T'['max'])))
axis(3,at=c(0,t_max,t_ss-1,t_ss,24-0.5),tick=F,las=1,line=-1,cex.axis=0.8,
     labels = c(expression('t'['sr,d']),expression('t'['max']),expression('t'['s']),expression('t'['ss']),expression('t'['sr,d+1'])))

arrows(t_max,T_0,t_max,T_max,length=0.06,code=3,col="blue",lwd=1)
arrows(t_max-5.2,T_0,t_max+5.2,T_0,length=0.06,code=3,col="blue",lwd=1)
arrows(t_max+2,T_sr_d1,t_max+2,T_0,length=0.06,code=3,col="blue",lwd=1)
# segments(t_ss-1,29.5,t_max+5.2,T_0,lty=3)
text(7,40,expression('T'['a']),cex=0.8,col="blue")
text(17,20,"k",col="blue",cex=0.8)
text(t_max-2,T_0-1.5,expression(omega),col="blue",cex=0.8)
text(t_max+1,20,expression(paste(delta,"T")),col="blue",cex=0.8)

lines(t.eval, temps1) #replot to make it on top

par(op) #restore par
dev.off()