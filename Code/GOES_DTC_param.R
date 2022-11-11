#derive DTC parameters for GOES observations
GOES_LST_val=read.csv(paste0("Data/",RegionName,"/GOES_LST/DTC_Parameters/GOES_LST_val.csv"))
GOES_LST_time=read.csv(paste0("Data/",RegionName,"/GOES_LST/DTC_Parameters/GOES_LST_time.csv"))
latlon=GOES_LST_val[,1:2]

# #example for one pixel
# id=100
# t0_GOES=as.numeric(GOES_LST_time[6,])
# temps0_GOES=as.numeric(GOES_LST_val[id,3:ncol(GOES_LST_val)])
# t0=seq(min(t0_GOES),max(t0_GOES),length.out = 200)
# tmp=DTC_GOES(t0_GOES=t0_GOES,temps0_GOES=temps0_GOES,t0_output=t0)
# plot(t0_GOES,temps0_GOES,pch=20)
# lines(t0,tmp$temps.est_GOES)

#for all GOES observations
#export: t_max, t_ss, T_sr(all D+1 days), T_max(all D days) 
DTC_GOES_parallel=function(x){
  tmp=DTC_GOES(t0_GOES=as.numeric(GOES_LST_time[6,]),temps0_GOES=as.numeric(x))
  return(c(tmp$t_max,tmp$t_ss,tmp$T_sr_all,tmp$T_max_all))
}

GOES_DTC_param0=t(apply(GOES_LST_val[,-(1:2)],1,DTC_GOES_parallel))
GOES_DTC_param=data.frame(latlon,GOES_DTC_param0)
write.csv(GOES_DTC_param,file=paste0("Data/",RegionName,"/GOES_LST/DTC_Parameters/GOES_DTC_param.csv"),row.names = F)

###########################################################
##linear/spline interpolation of DTC parameters from irregular grids
library(akima)
GOES_DTC_param=na.omit(read.csv(paste0("Data/",RegionName,"/GOES_LST/DTC_Parameters/GOES_DTC_param.csv")))
Days=(ncol(GOES_DTC_param)-5)/2
param_names=c("t_max","t_ss",paste("T_sr_Day",1:(Days+1),sep = ""),paste("T_max_Day",1:Days,sep = ""))
for (i in 3:ncol(GOES_DTC_param)){
  result=interp(GOES_DTC_param$lon,GOES_DTC_param$lat,GOES_DTC_param[,i],xo=seq(min(GOES_DTC_param$lon), max(GOES_DTC_param$lon), by = 0.01),
                yo=seq(min(GOES_DTC_param$lat), max(GOES_DTC_param$lat), by=0.01),linear=T,extrap=F) #at 0.01 approx. 1km resolution
  res_x=abs(result$x[1]-result$x[2])
  res_y=abs(result$y[1]-result$y[2])
  result_ras=raster(xmn=min(result$x)-res_x/2,xmx=max(result$x)+res_x/2,ymn=min(result$y)-res_y/2,ymx=max(result$y)+res_y/2,res=c(res_x,res_y))
  result_ras[]=as.vector(result$z)
  result_ras=flip(result_ras,direction = 2)

  writeRaster(result_ras,filename = paste0("Data/",RegionName,"/GOES_LST/DTC_Parameters/GOES_DTC_param_",param_names[i-2],".nc"),overwrite=T)
}
