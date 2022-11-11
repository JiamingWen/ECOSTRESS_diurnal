#create a shapefile for the test region
library(sp)
library(sf)
library(rgdal)
library(rgeos)

CreateShapefile=function(lon,lat,offset0,RegionName){
  x_coor=c(lon-offset0,lon+offset0,lon+offset0,lon-offset0)
  y_coor=c(lat+offset0,lat+offset0,lat-offset0,lat-offset0)
  xym <- cbind(x_coor, y_coor)
  p = Polygon(xym)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(sps)
  data_dummy = data.frame(f=99.9)
  spdf = SpatialPolygonsDataFrame(sps,data_dummy)
  writeOGR(spdf, dsn = paste0('Data/',RegionName,"/Shapefile/"), layer = RegionName, driver = "ESRI Shapefile")
}

CreateShapefile(lon=lon0,lat=lat0,offset0=offset0,RegionName=RegionName)
