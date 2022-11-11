#calculate daily and 2week average for MERRA-2 Rn, Ta, vpd, rh
year=startdate%/%10000
doyrange=1:365

#daily
library(parallel)
cl <- makeCluster(30)
parallel::clusterExport(cl, c("ReadMERRA2_daily","ReadMERRA2_anytime","ReadMERRA2_slice","year","et_tem","UTC_OFFSET","time_decomp","doyrange","RegionName"), envir=environment())
parallel::clusterEvalQ(cl, c(library(raster),library(ncdf4),library(stringr),library(bigleaf)))
parSapply(cl, doyrange,function(x){ReadMERRA2_daily(year0=year,doy0=x,target_ras=et_tem,UTC_OFFSET=UTC_OFFSET)})
stopCluster(cl)

#2week
variable=c("Ta","vpd","rh","Rn")
library(parallel)
cl <- makeCluster(4)
parallel::clusterExport(cl, c("ReadMERRA2_2week","year","variable","doyrange","RegionName"), envir=environment())
parallel::clusterEvalQ(cl, c(library(raster),library(ncdf4),library(stringr)))
parSapply(cl, variable,function(x){ReadMERRA2_2week(varname=x,year0=year,doyrange=doyrange)})
stopCluster(cl)
