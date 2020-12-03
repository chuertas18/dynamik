

# dataframe=d
# shp_grid<-("D:/temp/grille/grille_125.shp")
# # square=125
# sq=125
#
# head(dataframe)
###################
dynamics_grid_ground_rotate<-function(dataframe,shp_grid,sq){
  shp_grid<-shapefile(shp_grid)
  grid<-shp_grid[order(shp_grid$id),]

  # id_tree_list
  id_tree_list<-unique(dataframe[,c('idtree','xfield','yfield')])
  coordinates(id_tree_list) <- ~xfield + yfield # Convert to SpatialPointsDataFrame

  merge_grid <-function (grid,id_tree_list,varname){
    require(maptools)
    grid<-grid[order(grid@data$id),]
    g.grid <- over(id_tree_list, grid)
    n.grid <- spCbind(id_tree_list, g.grid)
    n.grid<-as.data.frame(n.grid)
    # n.grid <- rename(as.data.table(n.grid), c("g.grid" = varname))
    return(n.grid)
  }



  g_db<-merge_grid(grid,id_tree_list,"square")
  names(g_db)[2]<-"square"
  # names(g_db)<-c("idtree", "square", "xfield", "yfield")

  database<-merge(dataframe,g_db[,c("idtree","square")],by = "idtree")

  # head(g_db)
  # head(database)
  # head(database)

  # data_grid<-as.data.frame(Reduce(function(...) merge(..., all = TRUE, by = "idtree"),list(g_250[,1:2], g_125[,1:2], g_62[,1:2],df)))
  # head(data_grid)

  # database=data_grid


  # data_grid<-as.data.frame(Reduce(function(...) merge(..., all = TRUE, by = "idtree"),list(g_250[,1:2], g_125[,1:2], g_62[,1:2],df)))
  # head(data_grid)


  # dtbase1<-database[which(database$state=='dead'),]
  # dtbase2<-database[which(database$state=='surv'),]
  # names(database)
  # colnames(database)[3] <- "square"



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
  AGB_G=(agg_surv_dead$agb1-agg_surv_dead$agb0)+agg_rec$AGB_rec
  AGV_G=(agg_surv_dead$agv1-agg_surv_dead$agv0)+agg_rec$AGV_rec
  BA_G=(agg_surv_dead$ba1-agg_surv_dead$ba0)+agg_rec$BA_rec

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
  agg_stock_N1=aggregate(db_surv_rec[,c("idtree")],by = list(square=db_surv_rec$square),FUN = stem_num)


  agg_union<-data.frame(square=agg_D$square,AGB_G=AGB_G,AGB_D=agg_D$AGB_D,AGV_G=AGV_G,AGV_D=agg_D$AGV_D,BA_G=BA_G,BA_D=agg_D$BA_D,
                        AGB0=agg_stock0$agb0,AGB1=agg_stock1$agb1,AGV0=agg_stock0$agv0,AGV1=agg_stock1$agv1,
                        BA0=agg_stock0$ba0,BA1=agg_stock1$ba1,N0=agg_stock_N0$idtree,N1=agg_stock_N1$idtree,N_D=agg_N_D$N_D,N_R=agg_N_R$N_R,plot=i,per=unique(dataframe$per))


  ## Normalization###
  cols_period = c("AGB_G","AGB_D","AGV_G","AGV_D","BA_G","BA_D","N_D","N_R")
  cols_square = c("AGB_G","AGB_D","AGV_G","AGV_D","BA_G","BA_D",
                  "AGB0","AGB1","AGV0","AGV1","BA0","BA1","N0","N1","N_D","N_R")


  ### Function to normalize periods
  norm_period_fun<-function(x,period) { (as.numeric(as.character(x)))/period}

  # Function that allows to normalize by surface, the variable that is indicated is the resolution
  # in square meters and a variable in hectares is obtained
  # res_square in metres
  norm_square_fun<-function(x,res_square){x_norm=(as.numeric(as.character(x)))/((res_square^2)/10000)
    return(x_norm)}

  data_norm<-agg_union
  data_norm[,cols_period] = apply(data_norm[,cols_period], 2, norm_period_fun,period=unique(dataframe$period))
  data_norm[,cols_square] = apply(data_norm[,cols_square], 2, norm_square_fun, res_square=sq)
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



