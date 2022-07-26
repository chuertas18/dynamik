rm(list=ls())
# Uelison project, stock
# Functions
source("D:/CLAUDIA/github/dynamik/R/AGB_eq.R")
source("D:/CLAUDIA/github/dynamik/R/tree_field_data_m3MM.R")


# Libraries
library(data.table)
library(raster)

# CIRAD database
#db_cirad<-fread("F:/Melaine/CLAUDIA/Drive/TESIS/BACKUP/BD_Paracou_1984_2020.csv")
db_cirad<-fread("G:/Mon Drive/TESIS/BACKUP/BD_Paracou_1984_2020.csv")
db_cirad$square=db_cirad$square_62
# db_cirad=db_cirad[which(db_cirad$trait=="T0"),]

d0<-tree_field_data_m3MM(database=db_cirad,
                         year0=2013,p16=FALSE,
                         model="G:/Mon Drive/TESIS/Allometry/Scripts/Data/BRM/model3_MM_nsing_CC_dbh20",
                         HC="C:/Users/clauh/Downloads/HC_BDParacou_CIRAD_codeGreg_2013.csv") 

head(d0)
table(d0$trait)
# d0=df

write.csv(d0,"C:/Users/clauh/Downloads/d0_2013.csv")

# Plot grids
shp_grid<-shapefile("G:/Mon Drive/TESIS/Final_manuscript/Data/Grids/grille_62_s38.shp")
crs(shp_grid) <- "+init=epsg:32622"
grid<-shp_grid[order(shp_grid$id),]


# Converting the database to a spatial object
id_tree_list<-unique(d0[,c('idtree','xutm','yutm')])
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




# Union grid and database
g_db<-merge_grid(grid,id_tree_list)
# head(g_db)
g_db$square=as.numeric(g_db$square)
database<-merge(d0,g_db[,c("idtree","square","parcelle","trait")],by = c("idtree","square"))


# Stocks
# AGB, AGV, BA
database$WD_p<-database$wd*database$agv
agg_stock=aggregate(database[,c("agb","agv","ba","WD_p")],by = list(square=database$square),FUN = sum)
# N stem
stem_num=function (idtree) {length(na.omit(idtree))}
agg_stock_N=aggregate(database[,c("idtree")],by = list(square=database$square),FUN = stem_num)
names(agg_stock_N)[2]<-"N"
# Wood density

# agg_WD=aggregate(database[,c("wd")],by = list(square=database$square),FUN = mean)
# names(agg_WD)[2]<-"WD"



# Ecuacion qmd - Diametre quadratique moyen en cm
qmd_fun<-function(vec_dbh){ # Diametre quadratique moyen en cm
  sqrt(sum(na.omit(vec_dbh^2))/length(na.omit(vec_dbh)))
}
agg_qmd=aggregate(database[,c("dbh")],by = list(square=database$square),FUN = qmd_fun)
names(agg_qmd)[2]<-"qmd"


agg_union<-as.data.frame(Reduce(function(...) merge(..., all = TRUE, by = "square"),list(agg_stock,agg_stock_N,agg_qmd,unique(database[,c("square","parcelle","trait.x","year")]))))
agg_union$sq=62.5
#head(agg_union)





## Normalization###
agg_union$WDmean<-agg_union$WD_p/agg_union$agv
cols_square = c("agb", "agv", "ba", "N")
norm_square_fun<-function(x,res_square){x_norm=(as.numeric(as.character(x)))/((res_square^2)/10000)
return(x_norm)}

data_norm<-agg_union
data_norm[,cols_square] = apply(data_norm[,cols_square], 2, norm_square_fun, res_square=data_norm$sq)


names(data_norm)<-c("square", "agb", "agv", "ba", "WDi_AGVi", "N", "qmd", "parcelle", 
                    "trait", "year", "sq", "WDmean")

head(data_norm)
write.csv(data_norm,"G:/Mon Drive/TESIS/Final_manuscript/Presentation/Data/Stock_N_WD_T0_Paracou_2013_s62.csv")
