csvlist0=list.files(paste0("Data/",RegionName,"/GOES_LST/csvfile"),pattern = ".csv",full.names = F)
csvlist=c()
for (csvfile in csvlist0){
  date0=as.Date(substr(csvfile,5,14))
  startdate0=as.Date(startdatestr)
  enddate0=as.Date(enddatestr)
  ncname=paste0("Data/",RegionName,"/GOES_LST/ncfile/",substr(csvfile,1,nchar(csvfile)-4),".nc")
  if(!file.exists(ncname) & (date0-startdate0>-1) & (enddate0-date0>-1)){
    csvlist=c(csvlist,csvfile)
  }
}

# #non-parallel
# t=Sys.time()
# lapply(csvlist,csv_to_ncdf,ras=et_tem)
# Sys.time()-t

#parallel
# t=Sys.time()
library(parallel)
no_cores=30
cl<-makeCluster(no_cores)
clusterExport(cl, c("RegionName","et_tem","csvlist","csv_to_ncdf"))
clusterEvalQ(cl, c(library(raster),library(ncdf4), library(akima)))
parLapply(cl, 1:length(csvlist),function(x){csv_to_ncdf(csvlist[x],ras=et_tem, RegionName=RegionName)})
stopCluster(cl)
# Sys.time()-t