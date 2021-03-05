
#
# dataframe=d0
#shp_grid<-grilla
#
#sq=125
#}columna= # numero columna de shapefile
#
# head(dataframe)
###################
dynamics_grid_ground_nonnrm<-function(dataframe,shp_grid,sq){
  library(raster)
  shp_grid<-shapefile(shp_grid)
  crs(shp_grid) <- "+init=epsg:32622"
  grid<-shp_grid[order(shp_grid$id),]



  # id_tree_list
  id_tree_list<-unique(dataframe[,c('idtree','xutm','yutm')])
  coordinates(id_tree_list) <- ~xutm+ yutm # Convert to SpatialPointsDataFrame
  crs(id_tree_list) <- "+init=epsg:32622"


  merge_grid <-function (grid,id_tree_list){
    require(maptools)
    grid<-grid[order(grid@data$id),]
    g.grid <- over(id_tree_list, grid)
    n.grid <- spCbind(id_tree_list, g.grid)
    n.grid<-as.data.frame(n.grid)
    n.grid <- na.omit(n.grid)
    # n.grid <- rename(as.data.table(n.grid), c("g.grid" = varname))
    return(n.grid)
  }


  if (sq==60|sq==62|sq==62.5) {
    columna=2
  } else if (sq==120|sq==125) {
    columna=3
  } else if (sq==240|sq==250) {
    columna=3
  }else{print("Error shapefile")}



  g_db<-merge_grid(grid,id_tree_list)
  names(g_db)[columna]<-"square"
  # names(g_db)<-c("idtree", "square", "xfield", "yfield")

  database<-merge(dataframe,g_db[,c("idtree","square","Parcelle","trait")],by = "idtree")



  # head(g_db)
  # head(database)
  # head(database)

  db_dead<-database[which(database$state=='dead'),]
  #db_surv<-database[which(database$state=='survivors'),]
  db_surv_rec<-(database[which(database$state!='dead'),])
  db_surv_dead<-(database[which(database$state!='recruits'),])
  db_rec<-(database[which(database$state=='recruits'),])

  # Croissance des recrutements
  vol10cmDBH=0.07516036
  # data_d$AGB.y-data_d$AGB.x # Calculate the AGB difference
  AGB_sq=database$square[which(database$state=='recruits')]
  AGB_rec=database$agb1[which(database$state=='recruits')]- vol10cmDBH*(database$wd[which(database$state=='recruits')])^0.976 # Adjust growth for recruits only recruited trees, only biomass above 10cm dbh
  AGB_rec=ifelse(AGB_rec<0,0,AGB_rec)
  AGV_rec=database$agv1[which(database$state=='recruits')]- vol10cmDBH*(1)^0.976 # Adjust growth for recruits only recruited trees, only biomass above 10cm dbh
  AGV_rec=ifelse(AGV_rec<0,0,AGV_rec)
  BA_rec=database$ba1[which(database$state=='recruits')] - ((pi*25)*0.0001) # Adjust growth for recruits
  BA_rec=ifelse(BA_rec<0,0,BA_rec)
  agg_rec0=data.frame(square=AGB_sq,AGB_rec=AGB_rec,AGV_rec=AGV_rec,BA_rec=BA_rec)
  agg_rec=aggregate(agg_rec0[,c("AGB_rec","AGV_rec","BA_rec")],by = list(square=agg_rec0$square),FUN = sum)



  # Croissance - Productivity
  agg_surv_dead = aggregate(db_surv_dead[,c("agb0","ba0","agv0","agb1","agv1","ba1")],by = list(square=db_surv_dead$square),FUN = sum)
  agg_surv_dead0=merge(agg_surv_dead,agg_rec,by="square",all.x=T)
  agg_surv_dead0[is.na(agg_surv_dead0)] = 0
  AGB_G=(agg_surv_dead0$agb1-agg_surv_dead0$agb0)+agg_surv_dead0$AGB_rec
  AGV_G=(agg_surv_dead0$agv1-agg_surv_dead0$agv0)+agg_surv_dead0$AGV_rec
  BA_G=(agg_surv_dead0$ba1-agg_surv_dead0$ba0)+agg_surv_dead0$BA_rec
  agg_G=data.frame(square=agg_surv_dead0$square,AGB_G=AGB_G,AGV_G=AGV_G,BA_G=BA_G)

  # agg_surv_dead0[which(agg_surv_dead$square==46),]
  # agg_G

  # Mortality
  agg_D = aggregate(db_dead[,c("agb1","agv1","ba1")],by = list(square=db_dead$square),FUN = sum)
  names(agg_D)=c("square","AGB_D","AGV_D","BA_D")


  # Dead number of stems
  stem_num=function (idtree) {length(na.omit(idtree))}
  agg_N_D=aggregate(db_dead[,c("idtree")],by = list(square=db_dead$square),FUN = stem_num)
  names(agg_N_D)=c("square","N_D")

  #db_rec<-(database[which(database$state=='recruits'),])
  # Recruits number of stems
  agg_N_R=aggregate(db_rec[,c("idtree")],by = list(square=db_rec$square),FUN = stem_num)
  names(agg_N_R)=c("square","N_R")



 # Stocks
  agg_stock0=aggregate(db_surv_dead[,c("agb0","agv0","ba0")],by = list(square=db_surv_dead$square),FUN = sum)
  agg_stock1=aggregate(db_surv_rec[,c("agb1","agv1","ba1")],by = list(square=db_surv_rec$square),FUN = sum)

  agg_stock_N0=aggregate(db_surv_dead[,c("idtree")],by = list(square=db_surv_dead$square),FUN = stem_num)
  names(agg_stock_N0)[2]<-"N0"
  agg_stock_N1=aggregate(db_surv_rec[,c("idtree")],by = list(square=db_surv_rec$square),FUN = stem_num)
  names(agg_stock_N1)[2]<-"N1"

  agg_WD0=aggregate(db_surv_dead[,c("wd")],by = list(square=db_surv_dead$square),FUN = mean)
  names(agg_WD0)[2]<-"WD0"
  agg_WD1=aggregate(db_surv_rec[,c("wd")],by = list(square=db_surv_rec$square),FUN = mean)
  names(agg_WD1)[2]<-"WD1"

  agg_union<-as.data.frame(Reduce(function(...) merge(..., all = TRUE, by = "square"),list(agg_G,agg_D,agg_stock0,agg_stock1,agg_stock_N0,agg_stock_N1,agg_N_D,agg_N_R,agg_WD0,agg_WD1,unique(database[,c("square","Parcelle","trait")]))))
  #head(agg_union)







  ## Normalization###
  cols_period =  c("AGB_G", "AGV_G", "BA_G", "AGB_D", "AGV_D", "BA_D", "N_D", "N_R")
  cols_square = c("AGB_G", "AGV_G", "BA_G", "AGB_D", "AGV_D", "BA_D", "agb0", "agv0", "ba0", "agb1", "agv1", "ba1","N0","N1","N_D", "N_R")




  ### Function to normalize periods
  norm_period_fun<-function(x,period) { (as.numeric(as.character(x)))/period}

  # Function that allows to normalize by surface, the variable that is indicated is the resolution
  # in square meters and a variable in hectares is obtained
  # res_square in metres
  norm_square_fun<-function(x,res_square){x_norm=(as.numeric(as.character(x)))/((res_square^2)/10000)
    return(x_norm)}

  data_norm<-agg_union
  data_norm[,cols_period] = apply(data_norm[,cols_period], 2, norm_period_fun,period=unique(dataframe$period))
  # data_norm[,cols_square] = apply(data_norm[,cols_square], 2, norm_square_fun, res_square=sq)

  return(data_norm)


  # head(dataframe)
  # agg_rec = aggregate(db_rec[,c("agb1","agv1","ba1")],by = list(square=db_rec$square),FUN = sum)
  # AGB_G1= agg_rec$agb1
  # AGV_G1= agg_rec$agv1
  # BA_G1= agg_rec$ba1

  # AGB_G=database$agb1
  # AGB_rec=database$agb1[which(database$state=='recruits')]  - vol10cmDBH*(database$wd[which(database$state=='recruits')])^0.976 # Adjust growth for recruits only recruited trees, only biomass above 10cm dbh
  # BA_rec=database$ba1[which(database$state=='recruits')] - ((pi*25)*0.0001) # Adjust growth for recruits
  #

}



