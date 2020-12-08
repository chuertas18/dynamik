
### gaps_leitold ("D:/temp/chm/maxCHM_FO_PAR_2013_Plot_1.tif","D:/temp/chm/maxCHM_FO_PAR_2019_Plot_1.tif","D:/temp/gaps","gaps_leitold_1913")
#### gaps_leitold
# gaps_leitold = delta_area >=  4 m² delta_h >= 3 m (height losses between chm) .
# Note for the definition of the different types of clearings
# Leitold uses volume and makes calculation from vector
gaps_leitold <-function(chm0,chm1,outdir,outname){
  require(GISTools)
  require(rgdal)
  require(rgeos)
  require(raster)
  chm1=raster(chm1)
  chm1_g=chm1
  chm0=raster(chm0)
  chm0_g=chm0
  r=chm1-chm0 # delta
  r[r>=-3]<-0 # delta_h >= 3 m (height losses between chm)
  #plot(r)

  # extend r with a number of rows and culomns (at each side)
  # to isolate clumps adjacents to plot axes
  r2<-extend(r, c(1,1))
  #plot(r2)
  rc <- clump(r2, directions = 8)
  #plot(rc)

  # get frequency table
  f<-freq(rc)
  # save frequency table as data frame
  f<-as.data.frame(f)

  clumps_pixels=4 # 4m²

  # which rows of the data.frame are only represented by clumps under 9pixels?
  str(which(f$count <= clumps_pixels))
  # which values do these correspond to?
  str(f$value[which(f$count <= clumps_pixels)])
  # put these into a vector of clump ID's to be removed
  excludeID <- f$value[which(f$count <= clumps_pixels)]

  # make a new raster to be sieved
  formaskSieve <- rc
  # assign NA to all clumps whose IDs are found in excludeID
  formaskSieve[rc %in% excludeID] <- NA

  #plot(formaskSieve)
  formaskSieve[is.na(formaskSieve)] <- 0
  formaskSieve[formaskSieve>0]<-1

  return(formaskSieve)


  # raster_ip=r*formaskSieve
  # raster_ip[is.na(raster_ip)] <- 0
  #
  #
  # gaps_pol <- rasterToPolygons(formaskSieve, fun=function(x){x==1},dissolve=TRUE) # Poligonizar y disolver
  # gaps_pol <- disaggregate(gaps_pol) # Disaggregate polygons

  #writeRaster(formaskSieve, filename=paste0(outdir,"/",outname,"_surface"), format="GTiff", overwrite=TRUE)
  # writeRaster(raster_ip, filename=paste0(outdir,"/",outname,"_volumetric"), format="GTiff", overwrite=TRUE)
  # writeOGR(obj=gaps_pol, dsn=outdir, layer=outname, driver="ESRI Shapefile", overwrite=TRUE) # this is in geographical projection

}
