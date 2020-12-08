dir="D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental/DATA/CHM/ANPP2/Thinning/Data/sel"
# res_agg=5
#

raster_focal<-function(dir){
  lista=list.files(path=dir, recursive=TRUE, full.names=TRUE, pattern='.tif')
  require(raster)

  for(j in 1:length(lista)){
    print(lista[j])
    r=raster(lista[j])
    r_focal=focal(r, w=matrix(1,3,3), fun=mean,na.rm=FALSE)
    r3 <- cover(r_focal, r)
    t=gsub("(.*).tif.*", "\\1",lista[j])
    writeRaster(r3, filename=paste0(t, "_focal_if.tif"), format="GTiff", overwrite=TRUE)

  }
}


raster_focal("D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental/DATA/CHM/ANPP4/sel")



