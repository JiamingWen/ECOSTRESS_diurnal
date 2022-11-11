#read Tair and Tdew from MOD07_L2 and compare with Famiglietti et al., 2018
library(rgdal)
library(gdalUtils)
library(raster)
library(MODIS)
library(ncdf4)
as.Date("2014-08-01")-as.Date("2014-01-01")+1

ncname="C:/Users/jw2495/Downloads/MOD07/2014/08/01/2014.08.01.NSM.MODISsin5km.h08v05.nc"
# ncname="C:/Users/jw2495/Downloads/MOD07/2014/08/01/2014.08.01.NSM.MODISsin5km.h07v06.nc"
ncin=nc_open(ncname)
print(ncin)
Tair=raster(ncname,varname="Ta_K")
crs(Tair)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
Tair_wgs84=projectRaster(Tair,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Tair_wgs84)
Td=raster(ncname,varname="Td_K")
crs(Td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
Td_wgs84=projectRaster(Td,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Td_wgs84)
Hour=raster(ncname,varname="hour")
crs(Hour)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
Hour_wgs84=projectRaster(Hour,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Hour_wgs84)
hist((Hour_wgs84[]-11)*60)
hist(Hour_wgs84[]-coordinates(Hour_wgs84)[,1]*4/60) #GMT

Tair_wgs84[Hour_wgs84[]-coordinates(Hour_wgs84)[,1]*4/60<18.5]=NA
Td_wgs84[Hour_wgs84[]-coordinates(Hour_wgs84)[,1]*4/60<18.5]=NA
Hour_wgs84[Hour_wgs84[]-coordinates(Hour_wgs84)[,1]*4/60<18.5]=NA

#MOD07_L2 data
hdflist=list.files("C:/Users/jw2495/Downloads/MOD07_L2.A2014213/",pattern = "hdf",full.names = T)
hdf_contain=list()
lat0=(ymin(Tair_wgs84)+ymax(Tair_wgs84))/2
lon0=(xmin(Tair_wgs84)+xmax(Tair_wgs84))/2
num=0
for (hdfname in hdflist){
  one <- getSds(hdfname)
  #lat lon
  lat=raster(readGDAL(one$SDS4gdal[29], as.is = TRUE))
  lon=raster(readGDAL(one$SDS4gdal[30], as.is = TRUE))
  if (max(lat[])>lat0 & min(lat[])<lat0 & max(lon[])>lon0 & min(lon[])<lon0){
    num=num+1
    hdf_contain[num]=hdfname
  }
}
hdf_contain

hdfname="C:/Users/jw2495/Downloads/MOD07_L2.A2014213/MOD07_L2.A2014213.1900.061.2017313194131.hdf"
# hdfname="C:/Users/jw2495/Downloads/MOD07_L2.A2014213/MOD07_L2.A2014213.1905.061.2017313194116.hdf"
hdf_info <- GDALinfo(hdfname, returnScaleOffset = FALSE)
meta <- attr(hdf_info, "mdata")
# Open the first hdf file
one <- getSds(hdfname)
#lat lon
lat=raster(readGDAL(one$SDS4gdal[29], as.is = TRUE));summary(lat)
lon=raster(readGDAL(one$SDS4gdal[30], as.is = TRUE));summary(lon)
qc=raster(readGDAL(one$SDS4gdal[11], as.is = TRUE)) #Processing_Flag somehow all NA
# Open Water_Vapor as a raster brick
Water_Vapor <- brick(readGDAL(one$SDS4gdal[23], as.is = TRUE))*0.001000000047497451 #cm
Retrieved_Temperature_Profile0 <- brick(readGDAL(one$SDS4gdal[15], as.is = TRUE))
Retrieved_Temperature_Profile = (Retrieved_Temperature_Profile0+15000)*0.009999999776482582
plot(Retrieved_Temperature_Profile[[20]])
Retrieved_Moisture_Profile0 <- brick(readGDAL(one$SDS4gdal[16], as.is = TRUE))
Retrieved_Moisture_Profile  = (Retrieved_Moisture_Profile0+15000)*0.009999999776482582 #degK
Surface_Pressure <- brick(readGDAL(one$SDS4gdal[9], as.is = TRUE))*0.1000000014901161

summary(Retrieved_Moisture_Profile[[20]][])
summary(Retrieved_Temperature_Profile[[20]][]) #same missing mask

mat_s=as.data.frame(stack(Retrieved_Temperature_Profile,Surface_Pressure))
mat_d=as.data.frame(stack(Retrieved_Moisture_Profile,Surface_Pressure))

calTair=function(vec){
  Temp_profile=as.numeric(vec[1:20])
  Pres_profile=c(5, 10, 20, 30, 50, 70, 100, 150, 200, 250, 
                 300, 400, 500, 620, 700, 780, 850, 920, 950, 1000)
  Pres_s=vec[21]
  tmp=na.omit(data.frame(Pres_profile,Temp_profile))
  if (nrow(tmp)>1 & !is.na(Pres_s)){
    Tlower=tmp[nrow(tmp),2]
    Plower=tmp[nrow(tmp),1]
    Tupper=tmp[nrow(tmp)-1,2]
    Pupper=tmp[nrow(tmp)-1,1]
    Zlower=287.053/9.8*Tlower*log(Pres_s/Plower)
    Zupper=287.053/9.8*Tupper*log(Plower/Pupper)
    T_s=Tlower+(Tlower-Tupper)*Zlower/Zupper
  }else{
    T_s=NA
  }
  return(T_s)
}

T_s=apply(mat_s, 1, calTair)
T_d=apply(mat_d, 1, calTair)

color <- rev(terrain.colors(100))[as.numeric(cut(T_s[],breaks = 100))]
plot(lon[],lat[],pch=20,col = color,xlab="lon",ylab="lat")

sum0=rep(0,ncell(Tair_wgs84))
count0=rep(0,ncell(Tair_wgs84))
for (i in 1:ncell(lon)){
  if (i%%10000==0){
    print(i)
  }
  if (!is.na(T_s[i])){
    id0=cellFromXY(Tair_wgs84,c(lon[i],lat[i]))
    sum0[id0]=sum0[id0]+T_s[i]
    count0[id0]=count0[id0]+1
  }
}
Tair_my=Tair_wgs84
Tair_my[]=sum0/count0
Tair_my[is.na(Tair_wgs84[])]=NA
plot(Tair_wgs84)
plot(Tair_my)
plot(Tair_wgs84-Tair_my)
hist(Tair_my[]-Tair_wgs84[])
summary(Tair_my[]-Tair_wgs84[])
tmp=data.frame(theirs=Tair_wgs84[],mine=Tair_my[])
summary(lm(theirs~mine,data=tmp))
summary(lm(mine~theirs,data=tmp))
plot(tmp,pch=20)
tmp=tmp[abs(tmp$mine-tmp$theirs)<3,]
hist(tmp$mine-tmp$theirs,breaks=50)

color <- rev(terrain.colors(100))[as.numeric(cut(Retrieved_Moisture_Profile[[20]][],breaks = 100))] #20,19,18
plot(lon[],lat[],pch=20,col = color,xlab="lon",ylab="lat",xlim=c(xmin(Tair_my),xmax(Tair_my)),ylim=c(ymin(Tair_my),ymax(Tair_my)))



###################################
#Gridding with HEG tool
Retrieved_Temperature_Profile0 <- brick(readGDAL(one$SDS4gdal[15], as.is = TRUE))
Retrieved_Temperature_Profile = (Retrieved_Temperature_Profile0+15000)*0.009999999776482582
plot(Retrieved_Temperature_Profile[[20]])
Retrieved_Moisture_Profile0 <- brick(readGDAL(one$SDS4gdal[16], as.is = TRUE))
Retrieved_Moisture_Profile  = (Retrieved_Moisture_Profile0+15000)*0.009999999776482582 #degK
Surface_Pressure <- brick(readGDAL(one$SDS4gdal[9], as.is = TRUE))*0.1000000014901161

Retrieved_Temperature_Profile_HEG=stack()
for (i in 1:20){
  tmp=raster("C:/Users/jw2495/Downloads/MOD07_L2.A2014213/MOD07_L2.A2014213.1900.061.2017313194131_Temp.tif",band=i)
  tmp[tmp[]==-32768]=NA
  tmp=(tmp+15000)*0.009999999776482582
  Retrieved_Temperature_Profile_HEG=stack(Retrieved_Temperature_Profile_HEG,tmp)
}
plot(Retrieved_Temperature_Profile_HEG[[20]])

Surface_Pressure_HEG <- raster("C:/Users/jw2495/Downloads/MOD07_L2.A2014213/MOD07_L2.A2014213.1900.061.2017313194131_Pres.tif")
Surface_Pressure_HEG[Surface_Pressure_HEG[]==-32768]=NA
Surface_Pressure_HEG <- Surface_Pressure_HEG*0.1000000014901161
mat_s_HEG=as.data.frame(stack(Retrieved_Temperature_Profile_HEG,Surface_Pressure_HEG))
T_s_HEG=apply(mat_s_HEG, 1, calTair)
Tair_HEG=Surface_Pressure_HEG
Tair_HEG[]=T_s_HEG
plot(Tair_HEG)
Tair_HEG_proj=projectRaster(Tair_HEG,Tair_wgs84)

#compare with data Josh provided
Tair_HEG_proj[is.na(Tair_wgs84[])]=NA
plot(Tair_HEG_proj)
plot(Tair_wgs84)
hist(Tair_HEG_proj[]-Tair_wgs84[],breaks=100)
summary(Tair_HEG_proj[]-Tair_wgs84[])
tmp=data.frame(theirs=Tair_wgs84[],mine=Tair_HEG_proj[])
summary(lm(theirs~mine,data=tmp))
summary(lm(mine~theirs,data=tmp))
plot(tmp,pch=20)

#compare with my calculation
hist(Tair_HEG_proj[]-Tair_my[],breaks=100)
summary(Tair_HEG_proj[]-Tair_my[])
tmp=data.frame(theirs=Tair_HEG_proj[],mine=Tair_my[])
summary(lm(theirs~mine,data=tmp))
summary(lm(mine~theirs,data=tmp))
plot(tmp,pch=20)

Tair1000=projectRaster(Retrieved_Temperature_Profile_HEG[[20]],Tair_wgs84)
plot(Tair1000)
