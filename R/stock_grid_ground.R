
#
# dataframe=d0
#shp_grid<-grilla
#
#sq=125
#}columna= # numero columna de shapefile
#
# head(dataframe)
# datos=datos_postgres[which(datos_postgres$trait!="T4"),]
# datos$square=datos$square_125
# dataframe=datos
# shp_grid="D:/CLAUDIA/Drive/TESIS/Final_manuscript/Github/Manuscrit/Chapter3_AGWNPP/Data/Grids/grille_125_s38.shp"
# sq=125
# year=2020
# p16=FALSE

dataframe=data_p16
shp_grid="D:/CLAUDIA/Drive/TESIS/Final_manuscript/Github/Manuscrit/Chapter3_AGWNPP/Data/Grids/grille_125_s38.shp"
sq=125
year=agno
p16=TRUE

###################
stocks_grid_ground<-function(dataframe,shp_grid,sq,year,p16){
  
  ### Tree field data
  library(data.table)
  library(raster)
  
  source("D:/CLAUDIA/github/dynamik/R/AGB_eq.R")
  source("D:/CLAUDIA/github/dynamik/R/height_eq.R")
  
  ## dataframe subset for each year
  datos <-as.data.table(dataframe) # The subset becomes more efficient if it is transformed into datatable
  
  # It is guaranteed that only those that are marked as alive
  df = na.omit(unique(datos[censusyear == year &
                              codealive_cor == T, .(idtree,dbh,censusyear,dbh_dead,family,genus,wd,xfield,yfield,xutm,yutm,habitat,square,plot,trait)])) ##### Revisar
  
  head(df)
  
  ## Classification of status between dead, survivors and recruits For this you must first replace NA of dbh by zero
  df$dbh[is.na(df$dbh)] <- 0
  
  # Classify the status
  df$state = factor(
    ifelse(
      df$dbh > 0,'survivors',
      ifelse(df$dbh == 0, 'non survivors')
    ),
    levels = c('survivors', 'non survivors')
  )
  
  # Ecuacion area basal
  basal_area = function(vec_dbh) {
    (((vec_dbh / 2) ^ 2 * pi) * 0.0001)
  }
  ##### calculate aGB and BA at the alive individual level
  ## 0.001 is applied to change from AGB (kg) to Tons # Without P16
  if (p16==FALSE) {
    df$agb <- agb_eq(df$wd,df$dbh,height_eq(df$dbh, "Low"),df$family,df$genus)
    df$agv <-agb_eq(1, df$dbh, height_eq(df$dbh, "Low"), df$family, df$genus)
  } else {
    print("Remember that you only have to enter the values for plot P16")
    df$agb <- agb_eq(df$wd,df$dbh,height_eq(df$dbh, "High"),df$family,df$genus)
    df$agv <-agb_eq(1, df$dbh, height_eq(df$dbh, "High"), df$family, df$genus)
  }
  
  
  df$ba <- basal_area(df$dbh)
  df$year <- year
  
  
  # # # Shapefile grid
  # shp_grid<-shapefile(shp_grid)
  # crs(shp_grid) <- "+init=epsg:32622"
  # grid<-shp_grid[order(shp_grid$id),]
  # # 
  # # 
  # # 
  # # # id_tree_list - Remember that the Paracou database has duplicates and individuals outside the boundaries 
  # id_tree_list<-unique(dataframe[,c('idtree','xutm','yutm')])
  # coordinates(id_tree_list) <- ~xutm+ yutm # Convert to SpatialPointsDataFrame
  # crs(id_tree_list) <- "+init=epsg:32622"
  # # 
  # # 
  # merge_grid <-function (grid,id_tree_list){
  #   require(maptools)
  #   grid<-grid[order(grid@data$id),]
  #   g.grid <- over(id_tree_list, grid)
  #   n.grid <- spCbind(id_tree_list, g.grid)
  #   n.grid<-as.data.frame(n.grid)
  #   n.grid <- na.omit(n.grid)
  #   # n.grid <- rename(as.data.table(n.grid), c("g.grid" = varname))
  #   return(n.grid)
  # }
  # # 
  # # 
  # # 
  # # 
  # # 
  # g_db<-merge_grid(grid,id_tree_list)
  # # 
  # g_db$square=as.numeric(g_db$square)
  # database<-merge(dataframe,g_db[,c("idtree","square","Parcelle")],by = c("idtree","square"))
  # # head(database)
  # 
  database<-df
  
  stem_num=function (idtree) {length(na.omit(idtree))}
  
  # Stocks
  agg_stock0=aggregate(database[,c("agb","agv","ba")],by = list(square=database$square),FUN = sum)

  agg_stock_N0=aggregate(database[,c("idtree")],by = list(square=database$square),FUN = stem_num)
  names(agg_stock_N0)[2]<-"N0"

  agg_WD0=aggregate(database[,c("wd")],by = list(square=database$square),FUN = mean)
  names(agg_WD0)[2]<-"WD0"

  
  # Ecuacion qmd - Diametre quadratique moyen en cm
  qmd_fun<-function(vec_dbh){ # Diametre quadratique moyen en cm
    sqrt(sum(na.omit(vec_dbh^2))/length(na.omit(vec_dbh)))
  }
  agg_qmd0=aggregate(database[,c("dbh")],by = list(square=database$square),FUN = qmd_fun)
  names(agg_qmd0)[2]<-"qmd0"
  
  agg_union<-as.data.frame(Reduce(function(...) merge(..., all = TRUE, by = "square"),list(agg_stock0,agg_stock_N0,agg_WD0,agg_qmd0,unique(database[,c("square","plot","trait")]))))
  #head(agg_union)
  agg_union$year=year
  agg_union$sq=sq
  
  
  ## Normalization###
  cols_square = c( "agb", "agv", "ba", "N0")
  
  
  ### Function to normalize periods
  # norm_period_fun<-function(x,period) { (as.numeric(as.character(x)))/period}
  
  # Function that allows to normalize by surface, the variable that is indicated is the resolution
  # in square meters and a variable in hectares is obtained
  # res_square in metres
  norm_square_fun<-function(x,res_square){x_norm=(as.numeric(as.character(x)))/((res_square^2)/10000)
  return(x_norm)}
  
  data_norm<-agg_union
  #data_norm[,cols_period] = apply(data_norm[,cols_period], 2, norm_period_fun,period=data_norm$period)
  data_norm[,cols_square] = apply(data_norm[,cols_square], 2, norm_square_fun, res_square=sq)
  
  return(data_norm)
  
  
  # head(data_norm)
  
}



