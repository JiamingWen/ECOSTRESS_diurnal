#run DTC at flux tower site

##################################################################
#prepare data
#Ecostress data
Ecostress_tower=na.omit(read.csv(paste0("Data/", RegionName,"/Tower/Ecostress_tower.csv")))
Ecostress_tower$local_time=as.POSIXlt(paste0(Ecostress_tower$local_year,"-",Ecostress_tower$local_month,"-",Ecostress_tower$local_date," ",Ecostress_tower$local_hour,":",Ecostress_tower$local_minute))
Ecostress_tower$t0_Eco=as.numeric(difftime(Ecostress_tower$local_time,starttime,units="hour"))
t0_Eco=Ecostress_tower$t0_Eco[Ecostress_tower$t0_Eco>=0]
temps0_Eco=Ecostress_tower$Ecostress_tower[Ecostress_tower$t0_Eco>=0]-273.15

#GOES data
#original GOES LST
# GOES_LST_val=read.csv(paste0("Data/", RegionName,"/GOES_LST/DTC_Parameters/GOES_LST_val.csv"))
# GOES_LST_time=read.csv(paste0("Data/", RegionName,"/GOES_LST/DTC_Parameters/GOES_LST_time.csv"))
# tmp=(GOES_LST_val$lat-lat0)^2+(GOES_LST_val$lon-lon0)^2
# id=which(tmp==min(tmp))
# t0_GOES=as.numeric(GOES_LST_time[6,])
# temps0_GOES=as.numeric(GOES_LST_val[id,3:ncol(GOES_LST_val)])
#interpolated GOES LST
GOES_tower=na.omit(read.csv(paste0("Data/", RegionName,"/Tower/GOES_tower.csv")))
GOES_tower$local_time=as.POSIXlt(paste0(GOES_tower$local_year,"-",GOES_tower$local_month,"-",GOES_tower$local_date," ",GOES_tower$local_hour,":",GOES_tower$local_minute))
GOES_tower$t0_GOES=as.numeric(difftime(GOES_tower$local_time,starttime,units="hour"))
t0_GOES=GOES_tower$t0_GOES[GOES_tower$t0_GOES>=0]
temps0_GOES=GOES_tower$GOES_LST_tower[GOES_tower$t0_GOES>=0]-273.15

#Tower data
Tower_LST=read.csv(paste0("Data/", RegionName,"/Tower/Tower_LST.csv"))
temps0_tower=Tower_LST$Tower_LST-273.15
Tower_time=difftime(as.POSIXct(paste0(Tower_LST$local_year,"-",Tower_LST$local_month,"-",Tower_LST$local_date," ",Tower_LST$local_hour,":",Tower_LST$local_minute,":00")),as.POSIXct(paste0(startdatestr," 00:00:00")),units="days")

##################################################################
#fit DTC parameters
t0_output0=as.double(Tower_time)*24-t_sr #tower timestamps
t0_output=t0_output0[t0_output0>=0]


#fit GOES LST
# GOES_DTC_param=read.csv(paste0("Data/", RegionName,"/GOES_LST/DTC_Parameters/GOES_DTC_param.csv"))
# GOES_param=as.numeric(GOES_DTC_param[id,3:ncol(GOES_DTC_param)])
tmp=DTC_GOES(t0_GOES=t0_GOES,temps0_GOES=temps0_GOES,t0_output=t0_output)
GOES_param=c(tmp$t_max,tmp$t_ss,tmp$T_sr_all,tmp$T_max_all)
t_max_GOES=GOES_param[1]
t_ss_GOES=GOES_param[2]
Days=as.integer(as.Date(enddatestr)-as.Date(startdatestr))+1
T_sr_GOES=GOES_param[3:(3+Days)]
T_max_GOES=GOES_param[(4+Days):(length(GOES_param))]
temps.est_GOES=tmp$temps.est_GOES

#plot
plot((t0_GOES+t_sr)/24,temps0_GOES,type="p",pch=20,col="blue",xlab="",ylab="LST",axes = F,frame=T)
axis(2)
time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
axis(1,at=0:time_intv,labels = F,tck=-0.05)
xtick_loc=seq(0,time_intv,round(time_intv/5))
xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
axis(1,at=xtick_loc,labels = xtick_label)
lines((t0_output+t_sr)/24, temps.est_GOES,col="blue")


#fit for ECOSTRESS
result=DTC_Ecostress(t0_Eco,temps0_Eco,t0_GOES,temps0_GOES,t_max_GOES,t_ss_GOES,T_sr_GOES, T_max_GOES,t0_output,alpha=1,alpha_cali=T,leave_one_out=T)
temps.est_Eco=result$temps.est_Eco
leave_one_out_pred=result$leave_one_out_pred
result2=DTC_Ecostress(t0_Eco,temps0_Eco,t0_GOES,temps0_GOES,t_max_GOES,t_ss_GOES,T_sr_GOES, T_max_GOES,t0_Eco,alpha=1,alpha_cali=T,leave_one_out=T)
useall_pred=result2$temps.est_Eco #use all observations to fit

GOES_ts=rep(NA,length(Tower_time));GOES_ts[t0_output0>=0]=temps.est_GOES
Eco_ts=rep(NA,length(Tower_time));Eco_ts[t0_output0>=0]=temps.est_Eco
result_ts=data.frame(Tower_LST[,c("local_year","local_month","local_date","local_hour","local_minute")],Tower_ts=Tower_LST$Tower_LST-273.15,GOES_ts,Eco_ts)
write.csv(result_ts,file=paste0("Data/", RegionName,"/Tower/DTC_ts.csv"),row.names = F)
result_loo=data.frame(Ecostress_tower[,c("local_year","local_month","local_date","local_hour","local_minute")],Ecostress_obs=Ecostress_tower$Ecostress_tower-273.15,leave_one_out_pred,useall_pred)
write.csv(result_loo,file=paste0("Data/", RegionName,"/Tower/DTC_loo.csv"),row.names = F)
summary(lm(Eco_ts~Tower_ts,data=result_ts))
summary(lm(leave_one_out_pred~Ecostress_obs,data=result_loo))

#plot
range0=range(c(temps0_GOES,temps.est_GOES,temps0_Eco,temps.est_Eco,leave_one_out_pred,temps0_tower),na.rm=T)
plot(Tower_time,temps0_tower,type="l",pch=1,col="black",ylim=range0+c(0,2),xlab="",ylab="LST",axes = F,frame=T)
axis(2)
time_intv=as.integer(as.Date(enddatestr)-as.Date(startdatestr)+2)
axis(1,at=0:time_intv,labels = F,tck=-0.05)
xtick_loc=seq(0,time_intv,round(time_intv/5))
xtick_label=as.character(as.Date(startdatestr)+xtick_loc)
axis(1,at=xtick_loc,labels = xtick_label)

lines((t0_output+t_sr)/24, temps.est_GOES,col="blue")
points((t0_GOES+t_sr)/24, temps0_GOES,pch=20,col="blue")
lines((t0_output+t_sr)/24, temps.est_Eco,col="red")
points((t0_Eco+t_sr)/24, temps0_Eco,pch=20,col="red",cex=1.5)
points((t0_Eco+t_sr)/24, leave_one_out_pred,pch=8,col="red",cex=1.5)

plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="",axes=F)
legend("topleft",legend = c("Tower","GOES LST","ECOSTRESS LST","GOES fitted","ECOSTRESS fitted","ECOSTRESS leave-one-out prediction"),col=c("black","blue","red","blue","red","red"),lty=c(1,NA,NA,1,1,NA),pch=c(NA,20,20,NA,NA,8),bty = "n",cex=0.8,ncol = 1,seg.len=1)

