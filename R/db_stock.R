year=2015
year_p16=2015
unit="square" ###"habitat", "square"
sq=250
p16=TRUE
palms=TRUE
write=TRUE

# db_stock(year=2015,year_p16=2015,unit="square",sq=250,p16=TRUE,palms=TRUE,write=TRUE)

# Function to calculate the stock for a year, for different square units or habitats, including palms or not and the P16.
db_stock<-function(year,year_p16,unit,sq,p16,palms,write,grilla){
  if(exists("datos_postgres")==FALSE){
    datos_postgres=request_postgres(dbname="paracou",host="localhost",port=5432,user="postgres",password="postgres",sql_postgres="SELECT DISTINCT * FROM  paracou.bd_plots_corrected_habitats WHERE dbh>9.6 AND codealive_cor IS NOT NULL ORDER BY idtree, censusyear;")
  }
  
  datos=as.data.frame(datos_postgres)
  cols= c("idtree", "codealive_cor","censusyear", "family", "genus", "species","dbh","ba", "wd", "wd_level", "agb", "agv","xutmr", 
          "yutmr", "square_250", "square_125", "square_62","habitat", "plot", "trait")
  
  # Selection in the database by year
  # P16 is included
  if (p16==TRUE) {
    db0=datos[which(datos$censusyear==year & datos$codealive_cor==T & datos$plot!=16),cols]
    db1=datos[which(datos$censusyear==year_p16 & datos$codealive_cor==T & datos$plot==16),cols]
    if(nrow(db1)!=0){
      db=rbind(db0,db1)
    } else {
      print("Error when setting the P16 date (censusyear)")
    }
    
  } else if (p16==FALSE)  {
    db=datos[which(datos$censusyear==year & datos$codealive_cor==T & datos$plot!=16),cols]
  } else {
    stop("Error in setting whether or not to include P16")
  }
  
  
  
  # Include or not palms
  if (palms==TRUE) {
    db=db
  } else if (palms==FALSE) {
    db=db[which(db$family!="Arecaceae"),]
  } else {
    stop("Error in stating whether palms are included or not")
  }
  
  ## Reassigned the unit or analysis square
  if(unit=="square"){
    library(raster)
    grid<-shapefile(grilla)
    crs(grid) <- "+init=epsg:32622"
    grid=as.data.frame(grid)
    if (sq==62|sq==62.5) {
      db$square=db$square_62
      grid_area=aggregate(list(area=grid[,c("area")]),by = list(square=grid$square_62,trait=grid$trait,plot=grid$parcelle),FUN = sum)
    } else if (sq==125) {
      db$square=db$square_125
      grid_area=aggregate(list(area=grid[,c("area")]),by = list(square=grid$square_125,trait=grid$trait,plot=grid$parcelle),FUN = sum)
    } else if (sq==250) {
      db$square=db$square_250
      grid_area=aggregate(list(area=grid[,c("area")]),by = list(square=grid$square_250,trait=grid$trait,plot=grid$parcelle),FUN = sum)
    }else{
      stop("Error defining the square")
    }  
  } else if (unit=="habitat") { # Remember that the habitats are defined by the idtree and not by the shapefile.
    ### Ad ferry shapefile - Habitats
    # # Surface Ferry
    library(raster)
    ferry<-shapefile(grilla)
    crs(ferry) <- "+init=epsg:32622"
    ferry=as.data.frame(ferry)
    if (sq==62|sq==62.5) {
      db$square=paste0(db$square_62,"-",db$habitat)
      ferry_area=aggregate(list(area=ferry[,c("area")]),by = list(square=paste0(ferry$square_62,"-",habitat=ferry$habitats),trait=ferry$trait,
                                                                  plot=ferry$parcelle),FUN = sum)
    } else if (sq==125) {
      db$square=paste0(db$square_125,"-",db$habitat)
      ferry_area=aggregate(list(area=ferry[,c("area")]),by = list(square=paste0(ferry$square_125,"-",habitat=ferry$habitats),trait=ferry$trait,
                                                                  plot=ferry$parcelle),FUN = sum)
    } else if (sq==250) {
      db$square=paste0(db$square_250,"-",db$habitat)
      ferry_area=aggregate(list(area=ferry[,c("area")]),by = list(square=paste0(ferry$square_250,"-",habitat=ferry$habitats),trait=ferry$trait,
                                                                  plot=ferry$parcelle),FUN = sum)
      
    }else{
      stop("Error defining the square")
    }  
  } else {
    stop("Error defining the study (habitat or square) unit")
  }
  
  
  
  # Aggregation of stock variables
  db_sum=aggregate(db[,c("ba","agb","agv")], by=list(square=db$square), sum)
  db_len=aggregate(list(n=db[,c("idtree")]), by=list(square=db$square), length)
  db_mean=aggregate(db[,c("dbh","wd")], by=list(square=db$square), mean)
  
  if (unit=="habitat") {
    db<-as.data.frame(Reduce(function(...) merge(..., all = TRUE, by ="square"),list(db_sum,db_len,db_mean,ferry_area)))
  } else if (unit=="square") {
    db<-as.data.frame(Reduce(function(...) merge(..., all = TRUE, by ="square"),list(db_sum,db_len,db_mean,grid_area)))
  } else {
    stop("Error defining the study (habitat or square) unit")
  }
  
  # Eliminate those individuals that are outside of the grids
  db=db[which(!is.na(db$area)),]
  db$year=year
  db$unit=unit
  db$sq=sq
  db$p16=p16
  db$palms=palms
  db$norm=FALSE # If the ground variables were normalized by area
  
  dtotal=db
  return(dtotal)
  
  # if(write==TRUE){
  #   out_name=paste0("Y:/users/ClaudiaHuertas/MANUSCRIT/3_Article1_Stock_DBH_Height_WD/Output/Ground_stock_",year,"_",unit,"_",sq,"_P16",p16,"_palms",
  #                   palms,".csv")
  #   write.csv(dtotal, file=out_name)
  #   print(paste0("The table with the results is stored in the directory...", out_name))
  # } 
  
}
