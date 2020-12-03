# dir="D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental/DATA/CHM/ANPP2/Thinning/Data/sel"
# res_agg=5
#
#raster_aggregation("D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental/DATA/CHM/ANPP2/Thinning/Data/sel/t",5)

raster_aggregation<-function(dir,res_agg){
  lista=list.files(path=dir, recursive=TRUE, full.names=TRUE, pattern='.tif')
  require(raster)

  for(j in 1:length(lista)){
    print(lista[j])
    r=raster(lista[j])
    r_agg=aggregate(r,fact=res_agg, Expand=F)
    t=gsub("(.*).tif.*", "\\1",lista[j])
    writeRaster(r_agg, filename=paste0(t, "_agg",res_agg,"m.tif"), format="GTiff", overwrite=TRUE)

  }
}





