#create animation for constructed LST and ET
library(raster)
library(ncdf4)

varname="LST" #LST ET

#September 26-October 5, 2020
RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
hourlist = 7:18

#mask water
mask = raster("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region4new/Model/diurnal/ET/2020_9_26_12_0.nc")

for (datenum in 0:9){
  tmp = as.Date(startdatestr) + datenum
  tmp2 = strsplit(as.character(tmp),"-")[[1]]
  year = as.integer(tmp2[1])
  month = as.integer(tmp2[2])
  date = as.integer(tmp2[3])
  datestr = paste(year,month,date,sep = '_')
  
  for (hour in hourlist){
    jpeg(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/Figures_revised/animation/",varname,"/",datestr,'_',hour,'_0',".jpg"), width = 750, height = 650,quality = 100,res=300)
    op=par() #original par
    par(mar=c(1, 1, 2, 1) + 0.1,oma = c(0, 0, 0, 0))
    
    ras=raster(paste0("/local/workdir/jw2495/ECOSTRESS/DTC/us-seg-region4new/Model/diurnal/",varname,"/",datestr,"_",hour,"_0.nc"))
    if (varname=="LST"){
      ras=ras-273.15
      col=colorRampPalette(c("blue", "cyan","yellow", "red"))(255)
      range0=c(15,50)
      breaks=seq(ceiling(range0[1]),floor(range0[2]),8)
      legend_label='\u00B0C'
    }else{
      ras[is.na(ras)]=0
      ras[is.na(mask)]=NA #mask water pixels
      ETcolors = c("#f6e8c3", "#d8b365", "#99974a", "#53792d", "#6bdfd2", "#1839c5")
      col=colorRampPalette(ETcolors)(255)
      range0=c(0,400)
      breaks=seq(0,400,100)
      legend_label=expression(paste("W/m"^"2"))
    }
    ras[ras[]<range0[1]]=range0[1]
    ras[ras[]>range0[2]]=range0[2]
    
    plot(ras,col=col,axes=F,legend=F,main="",colNA="grey",zlim=range0)
    title(paste0(varname," ",year,"/",month,"/",date," ",hour,":00"),line=0.5,cex.main = 0.7)
    plot(ras,col=col, legend.only=TRUE,zlim=range0,
         legend.width=0.5, legend.shrink=0.5,
         axis.args=list(cex.axis=0.5,at=breaks,labels=breaks),
         legend.args=list(text=legend_label, side=3, font=1, line=0, cex=0.5))
    
    par(op) #restore par
    dev.off()
  }
}


##################################################################
#create gif
library(magick)

varname="LST" #LST ET

#September 26-October 5, 2020
RegionName="us-seg-region4new"
startdate=20200926;startdatestr="2020-09-26" #2020270
enddate=20201005;enddatestr="2020-10-05" #2020279
hourlist = 7:18

filelist = NULL
for (datenum in 0:9){
  tmp = as.Date(startdatestr) + datenum
  tmp2 = strsplit(as.character(tmp),"-")[[1]]
  year = as.integer(tmp2[1])
  month = as.integer(tmp2[2])
  date = as.integer(tmp2[3])
  datestr = paste(year,month,date,sep = '_')
  filestr = paste0("/local/workdir/jw2495/ECOSTRESS/DTC/Figures_revised/animation/",varname,"/",datestr,"_",7:18,"_0.jpg")
  filelist = c(filelist, filestr)
}

img_list <- lapply(filelist, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = paste0("/local/workdir/jw2495/ECOSTRESS/DTC/Figures_revised/animation/",varname,".gif"))
