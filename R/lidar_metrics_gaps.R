# dir Directorio principal donde estan los datosd
# Make the Loop Database

#
# decimated<-"FALSE"
# dir<-"D:\temp\delta"
# gaps<-"/gaps_leitold_2009_2015_surface.tif"
# year<-"2009_2015"
# nom_out<-"gaps_leitold_2009_2015_surface"
# shp_grid<-"D:/GitHub/Paper_productivity/DATA/GRILLE/grille_125_gaps.shp"
#
# dir<-"Y:/users/ClaudiaHuertas/Mortality/Temp/delta"
# gaps_nom<-"Hmax_FO_PAR_2009_2019_dec_avsepoct"
# # couche<-"Delta_maxCHM_FO_PAR_2015_2019_Plot_1_Leitold.shp"
# year<-"2009_2019"
# nom_out<-"gaps_leitold_2009_2019_surface"
# shp_grid<-"Y:/users/ClaudiaHuertas/Mortality/Data/Grille/grille_125_s38.shp"

# year1<-2015

# chm1<-"DATA/CHM/ANPP4/CHM2015_SAR20_12ppm.tif"


lidar_metrics_gaps<-function(dir,gaps_nom,year,nom_out,shp_grid)
{
  nombre=nom_out
  y=year
  file=paste0(dir,"/",gaps_nom,"_surface.tif")
  file_shp=paste0(dir,"/",gaps_nom,".shp")
  print(paste0("Procesando... ",nom_out))

  #compare median and med metrics of canopy dynamics to predict basal area change
  setwd(dir)

  #######################################################################################
  #######################################################################################
  #######################################################################################

  #library(RasterMachine) # My library
  library(raster)
  
  ####################################################################################################
  ################################################################ FUNCTIONS
  ### KO BORRARLA
  cleanupCHM<-function(gaps,treshold){
    r=gaps
    require(raster)
    r[r<0] <- 0
    r[r>treshold] <- NA
    return(r)
  }


  ###########################################################################################
  gaps=raster(file)
  crs(gaps) <- "+init=epsg:32622"
  #gaps<-cleanupCHM(gaps,60) # My library RasterMachine
  range(gaps[],na.rm = T)
  
  gaps_shp=shapefile(file_shp)
  crs(gaps_shp) <- "+init=epsg:32622"
  #gaps<-cleanupCHM(gaps,60) # My library RasterMachine
  range(gaps[],na.rm = T)


  # grid (60, 120, 240)
  #shp_grid=readOGR(dsn="DATA/GRILLE", layer="Plots60x60")
  shp_grid<-shapefile(shp_grid)
  crs(shp_grid) <- "+init=epsg:32622"
  df_shp_grid<-as.data.frame(shp_grid)
  #names(df_shp_grid)<-c("Parcelle", "trait", "square_60", "square_120", "square_240", "s120","id")
  #names(df_shp_grid)<-c("id", "square", "Parcelle", "trait")


  # plot(gaps)
  # plot(shp_grid,add=T)
  #
  #### This function allows the calculation of net and brute change
  #### indicators and merges the information with the ground data according to the scale
  # square is the variable by which they will be joined
  # function_delta, one of the following functions: delta_s120, delta_240, delta_60
  # delta_grid, The grid product of the subtraction
  # ground_data, data of the plots


  ### DELTA AGB
  gaps_grid<-raster::extract(gaps, shp_grid,fun=sum,sp=TRUE)
  gaps_grid<-as.data.frame(gaps_grid)
  names(gaps_grid)[names(gaps_grid) == paste0(gaps_nom,"_surface")]<-"lei"

  #gaps_shp<-over(gaps_shp, shp_grid, returnList = TRUE)
  gaps_shp=intersect(shp_grid,gaps_shp)
  head(gaps_shp)
  ### Calculate the id_gaps in QGIS
  gaps_shp_agg=aggregate(gaps_shp[,c("id_gaps")],by = list(square=gaps_shp$square),FUN = length)

  data_s=merge(gaps_grid[,c("square","trait","square_250","parcelle","lei")],as.data.frame(gaps_shp_agg),by="square")
  head(data_s)
  names(data_s)[names(data_s) == "id_gaps"]<-"N_gaps"
 #summary(toto@data)  
  # cols_period<-c("acc_med", "acc_med2", "acc_mean", "aff_med", "aff_mean", "acc_mean2", "aff_med2",
  #                "aff_mean2", "aff_sum", "acc_sum","absmed","absmean")
  #
  # ### Function to normalize periods
  # norm_period_fun<-function(x,period) { (as.numeric(as.character(x)))/period}

  # data_s<-lidar_union(gaps_grid,chm_s,"square")
  # data_s[,cols_period] = apply(data_s[,cols_period], 2, norm_period_fun,period=period)
  data_s$year=y
  # data_s$carre=60
  #data_s<-data_s[,c("square_60", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
  # data_s<-merge(data_s,df_shp_grid[,c("square","Parcelle","trait")],by="square")
  # # names(data_s)<-c("square", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
  # names(data_s)[1]<-"square"
  # head(data_s)


  #table1<-rbind(data_60,data_120,data_240)
  # table1<-data_s
  # table2<-cbind(nombre,file0,file1)
  # table3<-cbind(table1,table2)
  return(data_s)
}

# head(table3)

