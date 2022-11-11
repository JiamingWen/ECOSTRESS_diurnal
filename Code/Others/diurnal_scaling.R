##############################################################################
#compare with scaling ETinst to ET daily using Rn
setwd("/local/workdir/jw2495/ECOSTRESS/DTC/")

#############################################################################
#plot ET/SW_in ratio
ET_radiation_ratio = function(){
  Tower_data=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  Tower_time=difftime(as.POSIXlt(paste0(Tower_data$local_year,"-",Tower_data$local_month,"-",Tower_data$local_date," ",Tower_data$local_hour,":",Tower_data$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")
  
  #exclude rainy days
  if (RegionName=="us-seg-region2new"){
    Tower_data[Tower_data$local_year==2018 & Tower_data$local_month==8 & Tower_data$local_date %in% c(10,11),c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA
  }else if (RegionName=="us-seg-region5new"){
    Tower_data[Tower_data$local_year==2020 & Tower_data$local_month==4 & Tower_data$local_date==13,c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA
  }
  
  #exclude nighttime
  Tower_data[Tower_data$SW_IN_MERRA2<=0 | is.na(Tower_data$SW_IN_MERRA2),c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA #no need, because SW_IN_MERRA2 is only daytime available
  Tower_data[Tower_data$SW_IN_Tower<=0 | is.na(Tower_data$SW_IN_Tower),c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA
  
  Tower_ratio = Tower_data$ET_Tower / Tower_data$SW_IN_Tower
  Model_ratio = Tower_data$ET_MERRA2_modfgfMfSM / Tower_data$SW_IN_MERRA2
  
  range0=c(0, max(Model_ratio,na.rm=T)*1.5)
  
  plot(Tower_time,Tower_ratio,type="p",pch=20,col="black",ylim=range0,xlab="",ylab='',main="",axes = F,frame=T)
  points(Tower_time,Model_ratio, col="red",pch=20)
  
  axis(2)
  mtext(expression(paste("ET / SW"["in"])),2,line = 2, cex=1)
  time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+3)
  axis(1,at=0:time_intv,labels = F,tck=-0.05)
  xtick_loc=seq(0,time_intv,round(time_intv/5))
  xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
  axis(1,at=xtick_loc,labels = xtick_label,cex=1.2)
  # mtext(xtick_label,side=1,line=1,at=xtick_loc,cex=1.2)
  points(Tower_time,Model_ratio, col="red")
}


###########################################################################################
#ET-site time series
jpeg("Figures_revised/ET_radiation_ratio.jpg", width = 2100, height = 2100,quality = 100,res=300)
op=par() #original par

#August 4-14, 2018
par(fig=c(0,1,0.75,1),mai = c(0.4, 0.9, 0.2, 0.5),oma=c(0,0,0,0))
RegionName="us-seg-region2new"
startdate=20180804;startdatestr="2018-08-04" #2018216
enddate=20180814;enddatestr="2018-08-14" #2018226
ET_radiation_ratio()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(a)",cex=1)
legend(3,0.45, c(expression(paste("Tower ET / Tower SW"["in"])),expression(paste("Constructed ET / MERRA-2 SW"["in"]))), pch=20, col=c("black","red"),bty="n",ncol=1,cex=0.8)

#April 7-17, 2020
par(fig=c(0,1,0.5,0.75), new=TRUE)
RegionName="us-seg-region5new"
startdate=20200407;startdatestr="2020-04-07" #2020098
enddate=20200417;enddatestr="2020-04-17" #2020108
ET_radiation_ratio()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(b)",cex=1)

#September 26-October 5, 2020
par(fig=c(0,1,0.25,0.5), new=TRUE)
RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
ET_radiation_ratio()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(c)",cex=1)

#June 15-19, 2021
par(fig=c(0,0.6,0,0.25), new=TRUE)
RegionName="us-arm-region2new"
startdate=20210615;startdatestr="2021-06-15" #2021166
enddate=20210619;enddatestr="2021-06-19" #2021170
ET_radiation_ratio()
text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/15,par('usr')[4]-(par('usr')[4]-par('usr')[3])/10,labels="(d)",cex=1)

par(op) #restore par
dev.off()


# #############################################################################
# #plot scatterplot ET vs SW_in
# radiation_ET_scatter = function(RegionName, data_source, title, plotid){
#   Tower_data=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
#   
#   #exclude rainy days
#   if (RegionName=="us-seg-region2new"){
#     Tower_data[Tower_data$local_year==2018 & Tower_data$local_month==8 & Tower_data$local_date %in% c(10,11),c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA
#   }else if (RegionName=="us-seg-region5new"){
#     Tower_data[Tower_data$local_year==2020 & Tower_data$local_month==4 & Tower_data$local_date==13,c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA
#   }
#   
#   #exclude nighttime
#   Tower_data[Tower_data$SW_IN_MERRA2<=0 | is.na(Tower_data$SW_IN_MERRA2),c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA #no need, because SW_IN_MERRA2 is only daytime available
#   Tower_data[Tower_data$SW_IN_Tower<=0 | is.na(Tower_data$SW_IN_Tower),c("ET_Tower","ET_MERRA2_modfgfMfSM")]=NA
#   
#   if (data_source == "Tower"){
#     data1 = data.frame(SW_IN=Tower_data$SW_IN_Tower,ET=Tower_data$ET_Tower)
#   }else if(data_source == "Model"){
#     data1 = data.frame(SW_IN=Tower_data$SW_IN_MERRA2,ET=Tower_data$ET_MERRA2_modfgfMfSM)
#   }
#   
#   lmfit = lm(ET~SW_IN+0, data = data1)
#   
#   plot(data1$SW_IN, data1$ET, xlab="", ylab="", pch=20)
#   mtext(title,3,line=0.5,cex=0.8)
#   
#   if (data_source == "Tower"){
#     mtext(expression(paste("Tower SW"["in"]*" (W/m"^"2",")")),1,line=2.4,cex=0.6)
#     mtext(expression(paste("Tower ET (W/m"^"2",")")),2,line=2,cex=0.6)
#   }else if(data_source == "Model"){
#     mtext(expression(paste("MERRA-2 SW"["in"]*" (W/m"^"2",")")),1,line=2.4,cex=0.6)
#     mtext(expression(paste("Constructed ET (modified ","f"["SM"],", W/m"^"2",")")),2,line=2,cex=0.6)
#   }
# 
#   abline(0, coef(lmfit),lty=2,col="red")
#   text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/15,bquote("r = "*.(format(cor(data1$ET,data1$SW_IN,use = "complete.obs"),digits = 2,nsmall=2))),pos=4)
#   text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/30,par('usr')[4]-(par('usr')[4]-par('usr')[3])/6,bquote("ET = "*.(format(coef(lmfit),digits = 2,nsmall=2))*" * SW"["in"]),pos=4)
#   
#   text(par('usr')[2]-(par('usr')[2]-par('usr')[1])/5,par('usr')[3]+(par('usr')[4]-par('usr')[3])/11,plotid,pos=4,cex=1.2)
# }
# 
# jpeg(paste0("Figures_revised/radiation_ET_scatter.jpg"), width = 2500, height = 1300,quality = 100,res=300)
# par(mfrow=c(2,4),mar=c(2, 3, 2, 1) + 0.1,oma = c(2, 2, 0.2, 2))
# 
# radiation_ET_scatter("us-seg-region2new","Tower","4-14 August 2018","(a)")
# radiation_ET_scatter("us-seg-region5new","Tower","7-17 April 2020","(b)")
# radiation_ET_scatter("us-seg-region4new","Tower","26 September to 5 October 2020","(c)")
# radiation_ET_scatter("us-arm-region2new","Tower","15-19 June 2021","(d)")
# 
# radiation_ET_scatter("us-seg-region2new","Model","","(e)")
# radiation_ET_scatter("us-seg-region5new","Model","","(f)")
# radiation_ET_scatter("us-seg-region4new","Model","","(g)")
# radiation_ET_scatter("us-arm-region2new","Model","","(h)")
# 
# par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
# dev.off()

# ###########################################################################
# #check individual days
# #August 4-14, 2018
# RegionName="us-seg-region2new"
# startdate=20180804;startdatestr="2018-08-04" #2018216
# enddate=20180814;enddatestr="2018-08-14" #2018226
# 
# #April 7-17, 2020
# RegionName="us-seg-region5new"
# startdate=20200407;startdatestr="2020-04-07" #2020098
# enddate=20200417;enddatestr="2020-04-17" #2020108
# 
# #Sept 26 - Oct 5, 2020
# RegionName="us-seg-region4new"
# startdate=20200926;startdatestr="2020-09-26" #2020270
# enddate=20201005;enddatestr="2020-10-05" #2020279
# 
# #Jun 15 - 19, 2021
# RegionName="us-arm-region2new"
# startdate=20210615;startdatestr="2021-06-15" #2021166
# enddate=20210619;enddatestr="2021-06-19" #2021170
# 
# Tower_data=read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
# 
# #20200415
# plot(Tower_data$SW_IN_MERRA2[Tower_data$local_date==15],Tower_data$ET_MERRA2_modfgfMfSM[Tower_data$local_date==15])
# 
# #20180807
# plot(Tower_data$SW_IN_MERRA2[Tower_data$local_date==7],Tower_data$ET_MERRA2_modfgfMfSM[Tower_data$local_date==7])
# 
# # #########################################################################
# plot(Tower_data$SW_IN_Tower,Tower_data$ET_Tower,main="Observation_all")
# plot(Tower_data$SW_IN_MERRA2,Tower_data$ET_MERRA2_modfgfMfSM,main="Prediction_all")
# 
# datelist = Tower_data$local_date[!duplicated(Tower_data$local_date)]
# for (date0 in datelist){
#   plot(Tower_data$SW_IN_Tower[Tower_data$local_date==date0 & Tower_data$local_hour<12],Tower_data$ET_Tower[Tower_data$local_date==date0 & Tower_data$local_hour<12],col="blue",main=paste0("Observation",date0))
#   points(Tower_data$SW_IN_Tower[Tower_data$local_date==date0 & Tower_data$local_hour>12],Tower_data$ET_Tower[Tower_data$local_date==date0 & Tower_data$local_hour>12],col="red")
#   plot(Tower_data$SW_IN_MERRA2[Tower_data$local_date==date0 & Tower_data$local_hour<12],Tower_data$ET_MERRA2_modfgfMfSM[Tower_data$local_date==date0 & Tower_data$local_hour<12],col="blue",main=paste0("Prediction",date0))
#   points(Tower_data$SW_IN_MERRA2[Tower_data$local_date==date0 & Tower_data$local_hour>12],Tower_data$ET_MERRA2_modfgfMfSM[Tower_data$local_date==date0 & Tower_data$local_hour>12],col="red")
# }


# #########################################################################################
# #scaling
# #August 4-14, 2018
# RegionName="us-seg-region2new"
# startdate=20180804;startdatestr="2018-08-04" #2018216
# enddate=20180814;enddatestr="2018-08-14" #2018226
# # slope0 = 0.1325228
# 
# #April 7-17, 2020
# RegionName="us-seg-region5new"
# startdate=20200407;startdatestr="2020-04-07" #2020098
# enddate=20200417;enddatestr="2020-04-17" #2020108
# # slope0 = 0.05041815
# 
# #Sept 26 - Oct 5, 2020
# RegionName="us-seg-region4new"
# startdate=20200926;startdatestr="2020-09-26" #2020270
# enddate=20201005;enddatestr="2020-10-05" #2020279
# # slope0 = 0.0202059
# 
# #Jun 15 - 19, 2021
# RegionName="us-arm-region2new"
# startdate=20210615;startdatestr="2021-06-15" #2021166
# enddate=20210619;enddatestr="2021-06-19" #2021170
# # slope0 = 0.1901809
# 
# Tower_data = read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
# ECOSTRESS_data = read.csv(paste0(RegionName,"/Tower/ECOSTRESS_ET.csv"))
# ECOSTRESS_data = ECOSTRESS_data[order(ECOSTRESS_data$local_month + ECOSTRESS_data$local_date/31),]
# for (i in 1:nrow(ECOSTRESS_data)){
#   ECOSTRESS_date = ECOSTRESS_data$local_date[i]
#   ECOSTRESS_hour = ECOSTRESS_data$local_hour[i]
#   ECOSTRESS_minute = ECOSTRESS_data$local_minute[i]
#   
#   Tower_data0 = Tower_data[Tower_data$local_date == ECOSTRESS_date,]
#   # ECOSTRESS_SW_IN = approx(x = Tower_data0$local_hour + Tower_data0$local_minute/60, y = Tower_data0$SW_IN_MERRA2, xout = ECOSTRESS_hour+ECOSTRESS_minute/60)
#   Tower_timestamps = Tower_data0$local_hour + Tower_data0$local_minute/60
#   timeid = which.min(abs(Tower_timestamps - (ECOSTRESS_hour+ECOSTRESS_minute/60)))
#   ECOSTRESS_SW_IN = Tower_data0$SW_IN_MERRA2[timeid] #use the nearest timestamp
#   ECOSTRESS_ET = Tower_data0$ET_MERRA2_modfgfMfSM[timeid] #use the constructed ET (modified fSM)
#   
#   Tower_data0$ET_scaled = ECOSTRESS_ET/ECOSTRESS_SW_IN*Tower_data0$SW_IN_MERRA2 #scale with ET/SW_IN that day
#   # Tower_data0$ET_scaled = slope0*Tower_data0$SW_IN_MERRA2 #scale with ET/SW_IN during the 10-day period
#   
#   range0 = range(c(Tower_data0$ET_MERRA2_modfgfMfSM, Tower_data0$ET_scaled),na.rm=T)
#   plot(Tower_timestamps, Tower_data0$ET_MERRA2_modfgfMfSM,pch=20, xlim=c(5,20), ylim=range0, xlab="Hour of day", ylab=expression(paste("ET (W/m"^"2",")")))
#   points(Tower_timestamps, Tower_data0$ET_scaled, pch=20, col="red", ylim=range0)
#   
#   print(summary(Tower_data0$ET_MERRA2_modfgfMfSM-Tower_data0$ET_scaled))
#   print(sum(Tower_data0$ET_MERRA2_modfgfMfSM-Tower_data0$ET_scaled,na.rm=T)/(2.5*1e6)*30*60) #daytime ET diff in mm (?)
#   print((sum(Tower_data0$ET_MERRA2_modfgfMfSM-Tower_data0$ET_scaled,na.rm=T)/(2.5*1e6)*30*60) / (sum(Tower_data0$ET_MERRA2_modfgfMfSM,na.rm=T)/(2.5*1e6)*30*60)) #daytime ET diff in percentage
# }


##############################################################################
#plot
library(stringr)
diurnal_scaling_plot = function(RegionName, obsid, plotid){
  Tower_data = read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  ECOSTRESS_data = read.csv(paste0(RegionName,"/Tower/ECOSTRESS_ET.csv"))
  ECOSTRESS_data = ECOSTRESS_data[order(ECOSTRESS_data$local_month + ECOSTRESS_data$local_date/31),]
  
  i = obsid
  ECOSTRESS_date = ECOSTRESS_data$local_date[i]
  ECOSTRESS_hour = ECOSTRESS_data$local_hour[i]
  ECOSTRESS_minute = ECOSTRESS_data$local_minute[i]
  
  Tower_data0 = Tower_data[Tower_data$local_date == ECOSTRESS_date,]
  # ECOSTRESS_SW_IN = approx(x = Tower_data0$local_hour + Tower_data0$local_minute/60, y = Tower_data0$SW_IN_MERRA2, xout = ECOSTRESS_hour+ECOSTRESS_minute/60)
  Tower_timestamps = Tower_data0$local_hour + Tower_data0$local_minute/60
  timeid = which.min(abs(Tower_timestamps - (ECOSTRESS_hour+ECOSTRESS_minute/60)))
  ECOSTRESS_SW_IN = Tower_data0$SW_IN_MERRA2[timeid] #use the nearest timestamp
  ECOSTRESS_ET = Tower_data0$ET_MERRA2_modfgfMfSM[timeid] #use the constructed ET (modified fSM)
  
  slope1 = ECOSTRESS_ET/ECOSTRESS_SW_IN
  Tower_data0$ET_scaled = slope1*Tower_data0$SW_IN_MERRA2 #scale with ET/SW_IN that day
  # Tower_data0$ET_scaled = slope0*Tower_data0$SW_IN_MERRA2 #scale with ET/SW_IN during the 10-day period
  
  Tower_data0$ET_Tower[is.na(Tower_data0$ET_MERRA2_modfgfMfSM)] = NA
  range0 = range(c(Tower_data0$ET_MERRA2_modfgfMfSM, Tower_data0$ET_scaled, Tower_data0$ET_Tower),na.rm=T)
  plot(Tower_timestamps, Tower_data0$ET_MERRA2_modfgfMfSM,pch=20, xlim=c(5,20), ylim=range0, xlab="", ylab="")
  points(Tower_timestamps, Tower_data0$ET_scaled, pch=20, col="red", ylim=range0)
  points(Tower_timestamps, Tower_data0$ET_Tower, pch=3, col="blue", ylim=range0)
  
  # plot(Tower_timestamps, Tower_data0$ET_scaled - Tower_data0$ET_MERRA2_modfgfMfSM, pch=20, col="lightblue")
  
  dailyET = sum(Tower_data0$ET_MERRA2_modfgfMfSM,na.rm=T)/(2.5*1e6)*30*60
  dailyETdif = sum(Tower_data0$ET_scaled-Tower_data0$ET_MERRA2_modfgfMfSM,na.rm=T)/(2.5*1e6)*30*60
  dailyETdif_prec = dailyETdif / dailyET #daytime ET diff in percentage
  
  yearstr = as.character(ECOSTRESS_data$local_year[i])
  monthstr = str_pad(ECOSTRESS_data$local_month[i], pad='0',2)
  datestr = str_pad(ECOSTRESS_data$local_date[i], pad='0',2)
  hourstr = str_pad(ECOSTRESS_data$local_hour[i], pad='0',2)
  minutestr = str_pad(ECOSTRESS_data$local_minute[i], pad='0',2)
  
  mtext(paste0(yearstr,'/',monthstr,'/',datestr,' ',hourstr,':',minutestr),3,line=0.5,cex=0.8)
  
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/6,par('usr')[3]+(par('usr')[4]-par('usr')[3])/5, bquote("Daily ET: "*.(format(dailyET,digits = 2,nsmall=2))*'mm'),pos=4)
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/6,par('usr')[3]+(par('usr')[4]-par('usr')[3])/12, bquote("Differ by: "*.(format(dailyETdif_prec*100,digits = 1,nsmall=2))*'%'),pos=4)
  
  # text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/6,par('usr')[3]+(par('usr')[4]-par('usr')[3])/15*4, paste0('Overpass time: ',yearstr,'/',monthstr,'/',datestr,' ',hourstr,':',minutestr),pos=4)
  # text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/6,par('usr')[3]+(par('usr')[4]-par('usr')[3])/15*3, bquote("ET/SW_IN Slope: "*.(format(slope1,digits = 2,nsmall=2))),pos=4)
  # text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/6,par('usr')[3]+(par('usr')[4]-par('usr')[3])/15*2, bquote("Daily ET: "*.(format(dailyET,digits = 2,nsmall=2))*'mm'),pos=4)
  # text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/6,par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, bquote("Daily ET difference: "*.(format(dailyETdif,digits = 2,nsmall=2))*'mm ('*.(format(dailyETdif_prec*100,digits = 1,nsmall=2))*'%)'),pos=4)

  text(par('usr')[2]-(par('usr')[2]-par('usr')[1])/5,par('usr')[4]-(par('usr')[4]-par('usr')[3])/8,plotid,pos=4,cex=1.2)
  
}

jpeg(paste0("Figures_revised/radiation_ET_scaling.jpg"), width = 3000, height = 2000,quality = 100,res=300)
par(mfrow=c(4,5),mar=c(2, 2, 2, 1) + 0.1,oma = c(3, 3, 1, 0.2))
diurnal_scaling_plot("us-seg-region2new", 1, '(a)')
diurnal_scaling_plot("us-seg-region2new", 2, '(b)')
diurnal_scaling_plot("us-seg-region2new", 4, '(c)')
plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", frame=F, axes=F)
# legend('left',legend = c("Constructed ET","Scaled ET"), pch=20, col=c('black','red'),bty='n',ncol=1,cex=1.3)
legend('left',legend = c("Constructed ET","Scaled ET", 'Tower ET'), pch=c(20, 20, 3), col=c('black','red', 'blue'),bty='n',ncol=1,cex=1.3)
plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", frame=F, axes=F)

diurnal_scaling_plot("us-seg-region5new", 1, '(d)')
mtext(expression(paste("ET (W/m"^"2",")")),2,line = 2.8, cex=1.1, at=-30)
diurnal_scaling_plot("us-seg-region5new", 2, '(e)')
diurnal_scaling_plot("us-seg-region5new", 3, '(f)')
diurnal_scaling_plot("us-seg-region5new", 4, '(g)')
plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", frame=F, axes=F)

diurnal_scaling_plot("us-seg-region4new", 1, '(h)')
diurnal_scaling_plot("us-seg-region4new", 2, '(i)')
diurnal_scaling_plot("us-seg-region4new", 3, '(j)')
diurnal_scaling_plot("us-seg-region4new", 4, '(k)')
diurnal_scaling_plot("us-seg-region4new", 5, '(l)')

diurnal_scaling_plot("us-arm-region2new", 1, '(m)')
diurnal_scaling_plot("us-arm-region2new", 2, '(n)')
diurnal_scaling_plot("us-arm-region2new", 3, '(o)')
mtext('Time of Day',1,line = 2.8, cex=1.1)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

##################################################################################
#scatterplot
library(stringr)
diurnal_scaling_scatterplot = function(RegionName, obsid, plotid){
  Tower_data = read.csv(paste0(RegionName,"/Tower/Tower_input.csv"))
  ECOSTRESS_data = read.csv(paste0(RegionName,"/Tower/ECOSTRESS_ET.csv"))
  ECOSTRESS_data = ECOSTRESS_data[order(ECOSTRESS_data$local_month + ECOSTRESS_data$local_date/31),]
  
  i = obsid
  ECOSTRESS_date = ECOSTRESS_data$local_date[i]
  ECOSTRESS_hour = ECOSTRESS_data$local_hour[i]
  ECOSTRESS_minute = ECOSTRESS_data$local_minute[i]
  
  Tower_data0 = Tower_data[Tower_data$local_date == ECOSTRESS_date,]
  # ECOSTRESS_SW_IN = approx(x = Tower_data0$local_hour + Tower_data0$local_minute/60, y = Tower_data0$SW_IN_MERRA2, xout = ECOSTRESS_hour+ECOSTRESS_minute/60)
  Tower_timestamps = Tower_data0$local_hour + Tower_data0$local_minute/60
  timeid = which.min(abs(Tower_timestamps - (ECOSTRESS_hour+ECOSTRESS_minute/60)))
  ECOSTRESS_SW_IN = Tower_data0$SW_IN_MERRA2[timeid] #use the nearest timestamp
  ECOSTRESS_ET = Tower_data0$ET_MERRA2_modfgfMfSM[timeid] #use the constructed ET (modified fSM)
  
  slope1 = ECOSTRESS_ET/ECOSTRESS_SW_IN
  Tower_data0$ET_scaled = slope1*Tower_data0$SW_IN_MERRA2 #scale with ET/SW_IN that day
  # Tower_data0$ET_scaled = slope0*Tower_data0$SW_IN_MERRA2 #scale with ET/SW_IN during the 10-day period
  
  Tower_data0$ET_Tower[is.na(Tower_data0$ET_MERRA2_modfgfMfSM)] = NA
  range0 = range(c(Tower_data0$ET_MERRA2_modfgfMfSM, Tower_data0$ET_scaled, Tower_data0$ET_Tower),na.rm=T)
  plot(Tower_data0$ET_Tower, Tower_data0$ET_MERRA2_modfgfMfSM,pch=20, xlim=range0, ylim=range0, xlab="", ylab="")
  points(Tower_data0$ET_Tower, Tower_data0$ET_scaled, pch=20, col="red", xlim=range0, ylim=range0)
  
  yearstr = as.character(ECOSTRESS_data$local_year[i])
  monthstr = str_pad(ECOSTRESS_data$local_month[i], pad='0',2)
  datestr = str_pad(ECOSTRESS_data$local_date[i], pad='0',2)
  hourstr = str_pad(ECOSTRESS_data$local_hour[i], pad='0',2)
  minutestr = str_pad(ECOSTRESS_data$local_minute[i], pad='0',2)
  
  mtext(paste0(yearstr,'/',monthstr,'/',datestr,' ',hourstr,':',minutestr),3,line=0.5,cex=0.8)
  
  text(par('usr')[2]-(par('usr')[2]-par('usr')[1])/3,par('usr')[3]+(par('usr')[4]-par('usr')[3])/6, bquote("r="*.(format(cor(Tower_data0$ET_Tower, Tower_data0$ET_MERRA2_modfgfMfSM,use = "complete.obs"),digits = 2,nsmall=2))),pos=4, col='black', cex=1.2)
  text(par('usr')[2]-(par('usr')[2]-par('usr')[1])/3,par('usr')[3]+(par('usr')[4]-par('usr')[3])/15, bquote("r="*.(format(cor(Tower_data0$ET_Tower, Tower_data0$ET_scaled,use = "complete.obs"),digits = 1,nsmall=2))),pos=4, col='red', cex=1.2)
  
  text(par('usr')[2]-(par('usr')[2]-par('usr')[1])/5,par('usr')[4]-(par('usr')[4]-par('usr')[3])/8,plotid,pos=4,cex=1.2)
  
}

jpeg(paste0("Figures_revised/radiation_ET_scaling_scatterplot.jpg"), width = 3000, height = 2500,quality = 100,res=300)
par(mfrow=c(4,5),mar=c(2, 2, 2, 1) + 0.1,oma = c(3, 3, 1, 0.2))
diurnal_scaling_scatterplot("us-seg-region2new", 1, '(a)')
diurnal_scaling_scatterplot("us-seg-region2new", 2, '(b)')
diurnal_scaling_scatterplot("us-seg-region2new", 4, '(c)')
plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", frame=F, axes=F)
legend('left',legend = c("Constructed ET","Scaled ET"), pch=20, col=c('black','red'),bty='n',ncol=1,cex=1.3)
plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", frame=F, axes=F)

diurnal_scaling_scatterplot("us-seg-region5new", 1, '(d)')
mtext(expression(paste("Calculated ET (W/m"^"2",")")),2,line = 2.8, cex=1.1, at=-30)
diurnal_scaling_scatterplot("us-seg-region5new", 2, '(e)')
diurnal_scaling_scatterplot("us-seg-region5new", 3, '(f)')
diurnal_scaling_scatterplot("us-seg-region5new", 4, '(g)')
plot(NULL,xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", frame=F, axes=F)

diurnal_scaling_scatterplot("us-seg-region4new", 1, '(h)')
diurnal_scaling_scatterplot("us-seg-region4new", 2, '(i)')
diurnal_scaling_scatterplot("us-seg-region4new", 3, '(j)')
diurnal_scaling_scatterplot("us-seg-region4new", 4, '(k)')
diurnal_scaling_scatterplot("us-seg-region4new", 5, '(l)')

diurnal_scaling_scatterplot("us-arm-region2new", 1, '(m)')
diurnal_scaling_scatterplot("us-arm-region2new", 2, '(n)')
diurnal_scaling_scatterplot("us-arm-region2new", 3, '(o)')
mtext(expression(paste("Tower ET (W/m"^"2",")")),1,line = 2.8, cex=1.1)

par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()

##############################################################################
#spatial map of ET differences, using 20180804 as an example
jpeg(paste0("Figures_revised/radiation_ET_scaling_spatialdiff1.jpg"), width = 2300, height = 600,quality = 100,res=300)
par(mfrow=c(1,4),mar=c(0,0,0,0),oma = c(1, 1, 0.2, 5))

ET_inst = raster("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/2018_8_4_14_30.nc")
SWin_inst = raster("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/variables/SWGDN2018_8_4_14_30.nc")
hourlist = c(8, 11, 14, 17)
idlist = c('(a)','(b)','(c)','(d)')

for (i in 1:length(hourlist)){
  hour = hourlist[i]
  id = paste0(idlist[i], " ", str_pad(hour,2,pad=0),":00")
  SWin_t = raster(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/variables/SWGDN2018_8_4_",hour,"_0.nc"))
  ET_scaled = ET_inst / SWin_inst * SWin_t
  ET_constructed = raster(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/2018_8_4_",hour,"_0.nc"))
  ET_diff = ET_scaled - ET_constructed
  pal=colorRampPalette(c("red","white", "blue"))(100)
  range0 = c(-40,40)
  ET_diff[ET_diff<range0[1]]=range0[1]
  ET_diff[ET_diff>range0[2]]=range0[2]
  
  plot(ET_diff,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=pal)
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/4,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels=id,cex=1.5)
  
  if (i ==length(hourlist)){
    par(fig=c(0.94,1,0,1), new=TRUE)
    plot(ET_diff, zlim=range0,col=pal, legend.only=TRUE,
         legend.width=10, legend.shrink=0.75,
         axis.args=list(cex.axis=1.2),
         legend.args=list(text=expression(paste("    W/m"^"2")), side=3, font=1, line=0, cex=0.8))
  }
}
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()


##############################################################################
#spatial map of ET differences relative to ET, using 20180804 as an example
jpeg(paste0("Figures_revised/radiation_ET_scaling_spatialdiff2.jpg"), width = 2300, height = 600,quality = 100,res=300)
par(mfrow=c(1,4),mar=c(0,0,0,0),oma = c(1, 1, 0.2, 5))

ET_inst = raster("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/2018_8_4_14_30.nc")
SWin_inst = raster("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/variables/SWGDN2018_8_4_14_30.nc")
hourlist = c(8, 11, 14, 17)
idlist = c('(e)','(f)','(g)','(h)')

for (i in 1:length(hourlist)){
  hour = hourlist[i]
  id = paste0(idlist[i], " ", str_pad(hour,2,pad=0),":00")
  SWin_t = raster(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/variables/SWGDN2018_8_4_",hour,"_0.nc"))
  ET_scaled = ET_inst / SWin_inst * SWin_t
  ET_constructed = raster(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region2new/Model/diurnal/ET/2018_8_4_",hour,"_0.nc"))
  ET_diff = ET_scaled - ET_constructed
  ET_diff_ratio = ET_diff / ET_constructed
  print(summary(ET_diff_ratio[]))
  pal=colorRampPalette(c("red","white", "blue"))(100)
  range0 = c(-1,1)
  ET_diff_ratio[ET_diff_ratio<range0[1]]=range0[1]
  ET_diff_ratio[ET_diff_ratio>range0[2]]=range0[2]
  
  plot(ET_diff_ratio,axes=F,legend=FALSE,main="",colNA="grey",zlim=range0,col=pal)
  text(par('usr')[1]+(par('usr')[2]-par('usr')[1])/4,par('usr')[3]+(par('usr')[4]-par('usr')[3])/10,labels=id,cex=1.5)
  
  if (i ==length(hourlist)){
    par(fig=c(0.94,1,0,1), new=TRUE)
    plot(ET_diff_ratio*100, zlim=range0*100,col=pal, legend.only=TRUE,
         legend.width=10, legend.shrink=0.75,
         axis.args=list(cex.axis=1.2),
         legend.args=list(text=expression(paste("%")), side=3, font=1, line=0, cex=0.8))
  }
}
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1,oma=c(0,0,0,0))
dev.off()