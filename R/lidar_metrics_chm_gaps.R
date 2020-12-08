# dir Directorio principal donde estan los datosd
# Make the Loop Database

#
# decimated<-"FALSE"
dir<-"D:/temp/gaps2"
chm<-"/gaps_leitold_2009_2015_surface.tif"
year<-"2009_2015"
nom_out<-"gaps_leitold_2009_2015_surface"
shp_grid<-"D:/GitHub/Paper_productivity/DATA/GRILLE/grille_250.shp"

# year1<-2015

# chm1<-"DATA/CHM/ANPP4/CHM2015_SAR20_12ppm.tif"


lidar_metrics_gaps<-function(dir,chm,year,nom_out,shp_grid)
{
  nombre=nom_out
  y=year
  file=chm
  print(paste0("Procesando... ",nom_out))



  #compare median and med metrics of canopy dynamics to predict basal area change
  setwd(dir)



  #######################################################################################
  #######################################################################################
  #######################################################################################

  #library(RasterMachine) # My library
  library(raster)
  # library(rgdal)
  # library(maptools)
  # library(carData)
  # library(lme4)
  # library(car)
  # library(dplyr)
  # library(ggplot2)

  ### KO BORRARLA
  cleanupCHM<-function(chm,treshold){
    r=chm
    require(raster)
    r[r<0] <- 0
    r[r>treshold] <- NA
    return(r)
  }

  ####################################################################################################
  ################################################################ FUNCTIONS

  # med=function(v){ #  mediane d'accroissement sans normalisation
  #   v=na.omit(v)
  #   med=median(v)
  #   return(med)
  # }
  #
  #
  # mean=function(v){ #  mediane d'accroissement sans normalisation
  #   v=na.omit(v)
  #   moy=mean(v)
  #   return(moy)
  # }

  max=function(v){ #  mediane d'accroissement sans normalisation
    v=na.omit(v)
    max=max(v)
    return(max)
  }

  min=function(v){ #  mediane d'accroissement sans normalisation
    v=na.omit(v)
    min=min(v)
    return(min)
  }

  sd=function(v){ #  mediane d'accroissement sans normalisation
    v=na.omit(v)
    sd=sd(v)
    return(sd)
  }

  sum=function(v){
    v=na.omit(v)
    som=sum(v)
    return(som)
  }


  not_na=function(v){ #  mean d'accroissement sans normalisation
    v=na.omit(v)
    surf=length(v)
    return(surf)
  }

  sur=function(v){ #  mean d'accroissement sans normalisation
    v=v
    surf=length(v)
    return(surf)
  }

  test=chm_grid

  chm_s <- function (test){
    sq=shp_grid@data$square
    sq_ID=unique(sq)

    res=c()
    for (i in 1:length(sq_ID))
    {
      sel=which(sq==sq_ID[i])
      v=c()
      for (j in sel) {v=c(v,test[[j]])}
      temp=data.frame(square=sq_ID[i],med=med(v),mean=mean(v),max=max(v),min=min(v),sd=sd(v),sum=sum(v),not_na=not_na(v),sur=sur(v))
      res=rbind(res,temp)
    }
    return(res)
  }


  lidar_union<-function(chm_grid,function_delta,square){
    data0<-function_delta(chm_grid)
    return(data0)
  }



  ###########################################################################################
  chm=raster(paste0(dir,"/",file))
  crs(chm) <- "+init=epsg:2972"
  chm<-cleanupCHM(chm,60) # My library RasterMachine
  range(chm[],na.rm = T)




  # grid (60, 120, 240)
  #shp_grid=readOGR(dsn="DATA/GRILLE", layer="Plots60x60")
  shp_grid<-shapefile(shp_grid)
  crs(shp_grid) <- "+init=epsg:2972"
  df_shp_grid<-as.data.frame(shp_grid)
  #names(df_shp_grid)<-c("Parcelle", "trait", "square_60", "square_120", "square_240", "s120","id")
  #names(df_shp_grid)<-c("id", "square", "Parcelle", "trait")

  #### This function allows the calculation of net and brute change
  #### indicators and merges the information with the ground data according to the scale
  # square is the variable by which they will be joined
  # function_delta, one of the following functions: delta_s120, delta_240, delta_60
  # delta_grid, The grid product of the subtraction
  # ground_data, data of the plots


  ### DELTA AGB
  chm_grid<-raster::extract(chm, shp_grid)

  # cols_period<-c("acc_med", "acc_med2", "acc_mean", "aff_med", "aff_mean", "acc_mean2", "aff_med2",
  #                "aff_mean2", "aff_sum", "acc_sum","absmed","absmean")
  #
  # ### Function to normalize periods
  # norm_period_fun<-function(x,period) { (as.numeric(as.character(x)))/period}

  data_s<-lidar_union(chm_grid,chm_s,"square")
  # data_s[,cols_period] = apply(data_s[,cols_period], 2, norm_period_fun,period=period)
  data_s$year=y
  # data_s$carre=60
  #data_s<-data_s[,c("square_60", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
  data_s<-merge(data_s,df_shp_grid[,c("square","Parcelle","trait")],by="square")
  # names(data_s)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
  names(data_s)[1]<-"square_id"
  head(data_s)


  #table1<-rbind(data_60,data_120,data_240)
  table1<-data_s
  table2<-cbind(nombre,dec,file0,file1)
  table3<-cbind(table1,table2)
  return(table3)
}

# head(table3)

