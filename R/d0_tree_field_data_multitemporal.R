year_start=1991
year_end=2019
seq=2 # The analysis is biannual, annual or every five years.
only_control=TRUE
p16 = FALSE
analysis = 'habitat' # It can be by habitat or by square
sq=125 # Square
ferry_shp="Y:/users/ClaudiaHuertas/Mortality/Data/Ferry/TopoFerry_corr_s125.shp" # Must correspond to shapefile with the square the line above.

###################################################################################################################################
# tree_field_data d0
###################################################################################################################################
##########################################################################################################################
# Chargement de la base de données Postgres 
source("D:/CLAUDIA/github/dynamik/R/request_postgres.R") # Fonction de lecture Postgres
if(exists("datos_postgres")==FALSE){
  datos_postgres=request_postgres(dbname="paracou",host="localhost",port=5432,user="postgres",password="postgres",
  sql_postgres="SELECT DISTINCT * FROM  paracou.bd_plots_corrected_habitats WHERE dbh>9.6 ORDER BY idtree, censusyear;")
}

if(only_control==TRUE{
datos<-datos_postgres[which(datos_postgres$trait=="T0"),]
}else{
if(p16==FALSE){datos=datos_postgres[which(datos_postgres$trait!="P16"),]
}else if(p16==TRUE){
datos<-datos_postgres
seq=5
}
}

# Sequences for dates
v=seq(year_start, year_end, by=seq)
t=length(v)
newdf<-data.frame(y0=as.numeric(v[1:t-1]),y1=as.numeric(v[2:t]))
dy<-newdf
print(dy)

if (analysis == 'habitat'){
# # Surface Ferry
ferry<-shapefile(ferry_shp)
crs(ferry) <- "+init=epsg:32622"
plot(ferry)
head(ferry@data)
ferry=as.data.frame(ferry)


if(sq==250){
datos$id_square=datos$square_250
# union idtree and ferry
ferry$id_square<-as.integer(ferry$square_250) # Estaba como character
}if else(sq==125){
datos$id_square=datos$square_125
ferry$id_square<-as.integer(ferry$square_125) # Estaba como character
}if else(sq==62){
datos$id_square=datos$square_62
ferry$id_square<-as.integer(ferry$square_62) # Estaba como character
}else{Print("Error assigning the square")}

source("D:/CLAUDIA/github/dynamik/R/tree_field_data_bd_habitats.R")
habitat_area_s=aggregate(list(area=ferry$area[]),by = list(id_square=ferry$id_square[],habitat=ferry$habitats),FUN = sum) # Surface Ferry
datos_hab=merge(datos,habitat_area_s, by=c("id_square","habitat"),all.x=TRUE)# Normalizacion por superficie de habitat
datos_hab=datos_hab[!is.na(datos_hab$id_square) & !is.na(datos_hab$habitat),]
head(datos_hab)

data_t1=NULL
for(i in 1:nrow(dy)){
  year0=as.numeric(dy[i,1])
  year1=as.numeric(dy[i,2])
  d0 =tree_field_data_bd_habitats(datos_hab,year0,year1) # Habitas, agb y ba calculados
  d_sq$sq=sq
  #d_sq$year0=year0
  # d_sq$year1=year1
  
  
  
  # colnames(d_sq)[colnames(d_sq) == "treat"] <- "trait"
  
  
  
  # 
  data_t1<-rbind(data_t1,d0)}
}else if  (analysis == 'square'){
if(sq==250){
datos$id_square=datos$square_250
}if else(sq==125){
datos$id_square=datos$square_125
}if else(sq==62){
datos$id_square=datos$square_62
}else{Print("Error assigning the square")}

source("D:/CLAUDIA/github/dynamik/R/tree_field_data.R")
for(i in 1:nrow(dy)){
  year0=as.numeric(dy[i,1])
  year1=as.numeric(dy[i,2])
  colnames(datos_hab)[colnames(datos_hab) == "square_250"] <- "square"
  d0 = tree_field_data(datos_hab,year0,year1) # Base de datos por idtree
  #d_sq$year0=year0
  #d_sq$year1=year1
  #d_sq$sq=sq
  #d_sq=d_sq[which(d_sq$trait!="P16"),]
  if(p16==FALSE){d0=d0[which(d0$trait!="P16"),]}
  
  data_t2<-rbind(data_t2,d0) 
}
}else{stop("The type of analysis per habitat or per square was not assigned.")}





head(data_t1)



# BA y N Total
data_t2=NULL



year0 = 1991 # p13, 14 et 15 depuis 1991
year1 = 2020


period = 2
unite_kohyama = "trait"
p16 = FALSE

sq = 125 # In the case fit2 is applied