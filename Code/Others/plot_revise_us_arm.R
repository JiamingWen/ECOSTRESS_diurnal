##plot for revised manuscript
#us-arm
setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")
setwd("C:/Users/jw2495/Downloads/ECOSTRESS/DTC")
lon0=-97.4888;lat0=36.6058;RegionName="us-arm-region2new";sitename="us_arm";offset0=0.15;UTC_OFFSET=-6

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

cdl2=raster("us-arm-region2new/CDL/CDL_2020_reclassify.tif")
landtype=c('Others','Grassland/Pasture','Cotton','Sorghum','Corn','Dbl Crop WinWht/Soybeans','Soybeans','Winter Wheat','Water/Wetlands')
cdl2=as.factor(cdl2)
rat <- levels(cdl2)[[1]]
rat[["landtype"]] <- landtype
levels(cdl2) <- rat
palette <- c("#999999","#e8ffbf","#ff2626","#ff9e0a","#ffd300",
             "#707000","#267000","#a57000","#4970a3")

site <- data.frame(x=lon0, y=lat0)
coordinates(site) <- ~x+y

jpeg("Figures_revised/CDL_us_arm.jpg", width = 1350, height = 800,quality = 100,res=300)
op=par() #original par
par(mar=c(0,0,0,0),oma=c(0.2,0.2,0.2,0.2))
levelplot(cdl2,margin=F,colorkey=list(height=1, width=1.4),
          at=0:8,col.regions=palette,xlab="",ylab="")+
  # latticeExtra::layer(sp.points(site, pch=3, cex=1,lwd=2, col="red"), columns=1)+
  latticeExtra::layer(sp.points(site, pch=16, cex=0.8,lwd=2, col="black"), columns=1)+
  latticeExtra::layer(sp.text(c(lon0,lat0+0.02), cex=0.9, txt='US-ARM',font=list(face="bold")), columns=1) +
  latticeExtra::layer(sp.text(c(lon0-0.12,lat0-0.12), cex=1, txt='(b)',font=list(face="bold")), columns=1)
par(op) #restore par
dev.off()

#######################################################################################
#plot of land cover map for two sites
library(gridExtra)
jpeg("Figures_revised/CDL_all.jpg", width = 2500, height = 800,quality = 100,res=300)
op=par() #original par
par(mar=c(0,0,0,0),oma=c(0.2,0.2,0.2,0.2))

#us-seg
cdl2=raster("us-seg-region2new/CDL/CDL_2018_reclassify.tif")
landtype=c('Others','Cropland','Forest','Shrubland & Grassland','Urban','Wetland','Water')
cdl2=as.factor(cdl2)
rat <- levels(cdl2)[[1]]
rat[["landtype"]] <- landtype
levels(cdl2) <- rat

palette <- rev(c("#4FFFFF","#75B5EE","#FF1300","#E8E87A","#5dba5c",
                 "#BF56C3","#82342E"))

site <- data.frame(x=lon0, y=lat0)
coordinates(site) <- ~x+y

lv1=levelplot(cdl2,margin=F,colorkey=list(height=1, width=1.4),
          at=0:8,col.regions=palette,xlab="",ylab="")+
  latticeExtra::layer(sp.points(site, pch=3, cex=1,lwd=2, col="red"), columns=1)+
  latticeExtra::layer(sp.text(c(lon0,lat0+0.02), cex=0.7, txt='US-Seg',font=list(face="bold")), columns=1)

#us-arm
cdl2=raster("us-arm-region2new/CDL/CDL_2020_reclassify.tif")
landtype=c('Others','Grassland/Pasture','Cotton','Sorghum','Corn','Dbl Crop WinWht/Soybeans','Soybeans','Winter Wheat','Water/Wetlands')
cdl2=as.factor(cdl2)
rat <- levels(cdl2)[[1]]
rat[["landtype"]] <- landtype
levels(cdl2) <- rat
palette <- c("#999999","#e8ffbf","#ff2626","#ff9e0a","#ffd300",
             "#707000","#267000","#a57000","#4970a3")

site <- data.frame(x=lon0, y=lat0)
coordinates(site) <- ~x+y

lv2=levelplot(cdl2,margin=F,colorkey=list(height=1, width=1.4),
          at=0:8,col.regions=palette,xlab="",ylab="")+
  latticeExtra::layer(sp.points(site, pch=3, cex=1,lwd=2, col="red"), columns=1)+
  latticeExtra::layer(sp.text(c(lon0,lat0+0.02), cex=0.7, txt='US-ARM',font=list(face="bold")), columns=1)

grid.arrange(lv1, lv2,ncol=2)

par(op) #restore par
dev.off()


#######################################################################################
#LST-region
# 2021-06-18 14:24
LST=raster("us-arm-region2new/Model/overpass/LST/2021_6_18_14_24obs.nc")-273.15
LSTpred=raster("us-arm-region2new/Model/overpass/LST/2021_6_18_14_24.nc")-273.15
# LSTpred[LSTpred[]<273.16]=NA
LST=projectRaster(LST,LSTpred)
# LST[is.na(LSTpred)]=NA
# LSTpred[is.na(LST)]=NA
df=na.omit(data.frame(obs=LST[],pred=LSTpred[]))
dif=LSTpred-LST

setwd("us-arm-region2new/")
GOES=ReadGOESLST(2021,6,18,20,24)-273.15
setwd("..")

jpeg("Figures_revised/LST_region_us_arm.jpg", width = 2300, height = 1400,quality = 100,res=300)
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

############################################################################################
#ET-region
# 2021-06-18 14:24
ETcolors = c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5")
ETcol=colorRampPalette(ETcolors)(255)

ET=raster("us-arm-region2new/Model/overpass/ET/2021_6_18_14_24obs.nc")
ETpred=raster("us-arm-region2new/Model/overpass/ET/2021_6_18_14_24.nc")

ET=projectRaster(ET,ETpred)
ET[is.na(ETpred[])]=NA
ETpred[is.na(ET[])]=NA
df=na.omit(data.frame(obs=ET[],pred=ETpred[]))
dif=ETpred-ET

####################################################
#left
jpeg(paste0("Figures_revised/ET_region_us_arm_left.jpg"), width = 2200, height = 700,quality = 100,res=300)
par(mfrow=c(1,3),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))

#in the ET obs legend
range0=range(c(ET[]),na.rm=T)
plot(ET,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=ETcol)
title("ECOSTRESS ET*",line=0.5,cex.main = 1.5)
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
range1=50
dif[dif[]<(-range1)]=-range1
dif[dif[]>range1]=range1
plot(dif,axes=F,legend=FALSE,main="",col=pal,zlim=c(-range1,range1),colNA="grey")
title("Residual",line=0.5,cex.main = 1.5)
plot(dif, legend.only=TRUE,
     legend.width=3, legend.shrink=0.75,
     axis.args=list(cex.axis=1.2),
     legend.args=list(text=expression(paste("W/m"^"2")), side=3, font=1, line=0, cex=0.8),col=pal,zlim=c(-range1,range1))
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels='(c)',cex=2)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

####################################################
#right
jpeg(paste0("Figures_revised/ET_region_us_arm_right.jpg"), width = 900, height = 700,quality = 100,res=300)
par(mfrow=c(1,1),mar=c(0, 2, 2, 4) + 0.1,oma = c(2, 2, 0.2, 2))
print(ET_site_heatmap(df,"ET*","(d)"))
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

#######################################################################################
#LST-ET-region-scatter
jpeg(paste0("Figures_revised/LST_ET_region_scatter_us_arm.jpg"), width = 2100, height = 850,quality = 100,res=300)
# par(mfrow=c(1,3),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))

result_all=read.csv("us-arm-region2new/Model/overpass/LST/scatter.csv")
plot1=LST_region_scatter_plot(title="",plotid='(a)')

result_all=read.csv("us-arm-region2new/Model/overpass/ET/scatter_pred_comp.csv")
plot2=ET_region_scatter_plot(title="",plotid='(b)')

plot_grid(plot1,plot2, nrow=1)
# par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()



####################################################################################
#LST-ET-site - time series
jpeg("Figures_revised/LST_ET_site_us_arm.jpg", width = 2200, height = 1800,quality = 100,res=300)
op=par() #original par
# par(mfrow=c(3,1),mai = c(0.3, 0.5, 0.1, 0.7),mar=par()$mar+c(0,0,0,5), xpd=TRUE)
RegionName="us-arm-region2new"
startdate=20210615;startdatestr="2021-06-15" #2021166
enddate=20210619;enddatestr="2021-06-19" #2021170

#LST
par(fig=c(0.1,1,0.68,0.98),oma=c(0,0,0,0),mar=c(1.5, 1, 1, 1.2))
LST_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)")

par(fig=c(0.1,1,0.48,0.68), new=TRUE,mar=c(1, 1, 1, 1))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F)
legend(x = c(0.05, 0.97), y = c(0.1, 1),legend = c("GOES LST","ECOSTRESS LST","Leave-one-out estimation","Tower LST","Constructed LST"),col=c("deepskyblue","red","darkgreen","black","red"),lty=c(NA,NA,NA,1,1),pch=c(20,20,9,NA,NA),lwd=c(1,1,1.5,2,1),pt.cex=c(1,1.5,1,NA,NA),cex=0.9,text.width = c(0.3,0.3),ncol = 2,seg.len=2)

par(fig=c(0.1,1,0.22,0.52), new=TRUE,mar=c(1.5, 1, 1, 1.2))
ET_site_plot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)")

par(fig=c(0.1,1,0.02,0.22), new=TRUE,mar=c(1, 1, 1, 1))
plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F)
legend(x = c(0.05, 0.97), y = c(0.1, 1),legend = c("ECOSTRESS ET","ECOSTRESS ET*",NA,"Tower ET","Constructed ET",expression('Constructed ET (modified f'[SM]*')')),col=c("red","darkgreen",NA,"black","blue","red"),lty=c(NA,NA,NA,1,1,1),pch=c(20,18,NA,NA,NA,NA),cex=0.9,text.width = c(0.3,0.3),ncol = 2,seg.len=2)

par(op) #restore par
dev.off()

###############################################################
#LST-ET-site-scatter
fluxfolder=list.dirs("us-arm-region2new/Tower",full.names = T,recursive=F)
fluxfile=list.files(fluxfolder,pattern = 'AMF_.*\\.csv',full.names = T)
Tower_data0=read.csv(file=fluxfile,header = T,skip=2,na.strings = -9999)

jpeg(paste0("Figures_revised/LST_ET_site_scatter_us_arm.jpg"), width = 2500, height = 1200,quality = 100,res=300)
par(mfrow=c(1,2),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))

RegionName="us-arm-region2new"
startdate=20210615;startdatestr="2021-06-15" #2021166
enddate=20210619;enddatestr="2021-06-19" #2021170

DTC_site_scatterplot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(a)',cex=1.5)

ET_site_scatterplot()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/10,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels='(b)',cex=1.5)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

###################################################################################
#met-comp
#August 4-14, 2018
RegionName="us-arm-region2new"
startdate=20210615;startdatestr="2021-06-15" #2021166
enddate=20210619;enddatestr="2021-06-19" #2021170
figid=4
met_comp_plot()