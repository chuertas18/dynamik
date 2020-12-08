# dir Directorio principal donde estan los datosd
# Make the Loop Database
# dir<-"D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental"
# nom_out<-"SAR20_12ppm"
# decimated<-"FALSE"
# year0<-2013
# year1<-2015
# chm0<-"DATA/CHM/ANPP4/CHM2013_SAR20_full.tif"
# chm1<-"DATA/CHM/ANPP4/CHM2015_SAR20_12ppm.tif"
# shp_grid<-"D:/GitHub/Paper_productivity/DATA/GRILLE/grille_125_s38.shp"
# mask<-"D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental/DATA/CHM/leitold/gaps_leitold_2013_2015_surface.tif"

lidar_metrics_mask<-function(dir,chm0,chm1,year0,year1,decimated,nom_out,shp_grid,mask)
{
  nombre=nom_out
  dec=decimated
  y0=year0
  y1=year1
  period=year1-year0
  file0=chm0
  file1=chm1
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
  acc_med=function(v){
    v=na.omit(v)
    med=median(v[v>0])
    surf=length(v)
    acc=sum(v>0)
    return(med * acc/surf)
  }
  acc_med2=function(v){ #  mediane d'accroissement sans normalisation
    v=na.omit(v)
    med=median(v[v>0])
    return(med)
  }
  acc_mean=function(v){
    v=na.omit(v)
    moy=mean(v[v>0])
    surf=length(v)
    acc=sum(v>0)
    return(moy * acc/surf)
  }

  acc_mean2=function(v){ #  mediane d'accroissement sans normalisation
    v=na.omit(v)
    moy=mean(v[v>0])
    return(moy)
  }

  aff_mean=function(v){
    v=na.omit(v)
    moy=mean(v[v<0])
    surf=length(v)
    aff=sum(v<0)
    return(-moy * aff/surf)
  }

  aff_med=function(v){
    v=na.omit(v)
    med=median(v[v<0])
    surf=length(v)
    aff=sum(v<0)
    return(-med * aff/surf)
  }

  aff_med2=function(v){
    v=na.omit(v)
    med=median(v[v<0])
    return(-med)
  }
  aff_mean2=function(v){
    v=na.omit(v)
    moy=mean(v[v<0])
    return(-moy)
  }
  aff_sum=function(v){
    v=na.omit(v)
    som=sum(v[v<0])
    return(-som)
  }
  acc_sum=function(v){
    v=na.omit(v)
    som=sum(v[v>0])
    return(som)
  }

  absmed=function(v, na.rm=T){
    v=na.omit(v)
    median(abs(v))
  }

  absmean=function(v, na.rm=T){
    v=na.omit(v)
    mean(abs(v))
  }


  sur_aff=function(v){ #  mediane d'accroissement sans normalisation
    v=na.omit(v)
    aff=length(v[v<0])
    surf=length(v)
    d=aff/surf
    return(d)
  }

  sur_acc=function(v){ #  mean d'accroissement sans normalisation
    v=na.omit(v)
    acc=length(v[v>0])
    surf=length(v)
    d=acc/surf
    return(d)
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

  acc_75pct=function(v){
    v=na.omit(v)
    pct75=as.numeric(quantile(v[v>=0], probs=c(0.75)))
    return(pct75)
  }

  acc_ab30=function(v){
    v=na.omit(v)
    med30=median(v[v>=0.3])
    return(med30)
  }

  acc_abmin50=function(v){
    v=na.omit(v)
    med50=median(v[v>=-0.5])
    return(med50)
  }

  acc_abmin100=function(v){
    v=na.omit(v)
    med100=median(v[v>=-1])
    return(med100)
  }

  acc_abmin150=function(v){
    v=na.omit(v)
    med150=median(v[v>=-1.5])
    return(med150)
  }

  acc_abmin300=function(v){
    v=na.omit(v)
    med300=median(v[v>=-3])
    return(med300)
  }

  # Function to add to 120 square LIDAR indicators (delta  raster)
  # delta_s120 <- function (test){
  #   sq120=paste(shp_grid@data$square_240, shp_grid@data$square_120, sep="_")
  #   sq120_ID=unique(sq120)
  #
  #   res120=c()
  #   for (i in 1:length(sq120_ID))
  #   {
  #     sel=which(sq120==sq120_ID[i])
  #     v=c()
  #     for (j in sel) {v=c(v,test[[j]])}
  #     temp=data.frame(square_120=sq120_ID[i],acc_med=acc_med(v), acc_med2=acc_med2(v), acc_mean=acc_mean(v), aff_med=aff_med(v), aff_mean=aff_mean(v),acc_mean2=acc_mean2(v),aff_med2=aff_med2(v),aff_mean2=aff_mean2(v),aff_sum=aff_sum(v),acc_sum=acc_sum(v),absmed=absmed(v),absmean=absmean(v),absmed2=absmed2(v),absmean2=absmean2(v))
  #     res120=rbind(res120,temp)
  #   }
  #   return(res120)
  # }
  #
  #
  # # Function to add to 240 square LIDAR indicators (delta  raster)
  # delta_s240 <- function (test){
  #   sq240=shp_grid@data$square_240
  #   sq240_ID=unique(sq240)
  #
  #   res240=c()
  #   for (i in 1:length(sq240_ID))
  #   {
  #     sel=which(sq240==sq240_ID[i])
  #     v=c()
  #     for (j in sel) {v=c(v,test[[j]])}
  #     temp=data.frame(square_240=sq240_ID[i],acc_med=acc_med(v),acc_med2=acc_med2(v), acc_mean=acc_mean(v), aff_med=aff_med(v), aff_mean=aff_mean(v),acc_mean2=acc_mean2(v),aff_med2=aff_med2(v),aff_mean2=aff_mean2(v),aff_sum=aff_sum(v),acc_sum=acc_sum(v),absmed=absmed(v),absmean=absmean(v),absmed2=absmed2(v),absmean2=absmean2(v))
  #     res240=rbind(res240,temp)
  #   }
  #   return(res240)
  # }



  # Function to add to 60 square LIDAR indicators (delta  raster)
  # delta_s60 <- function (test){
  #   sq60=shp_grid@data$square
  #   sq60_ID=unique(sq60)
  #
  #   res60=c()
  #   for (i in 1:length(sq60_ID))
  #   {
  #     sel=which(sq60==sq60_ID[i])
  #     v=c()
  #     for (j in sel) {v=c(v,test[[j]])}
  #     temp=data.frame(square_60=sq60_ID[i],acc_med=acc_med(v),acc_med2=acc_med2(v), acc_mean=acc_mean(v), aff_med=aff_med(v), aff_mean=aff_mean(v),acc_mean2=acc_mean2(v),aff_med2=aff_med2(v),aff_mean2=aff_mean2(v),aff_sum=aff_sum(v),acc_sum=acc_sum(v),absmed=absmed(v),absmean=absmean(v),absmed2=absmed2(v),absmean2=absmean2(v))
  #     res60=rbind(res60,temp)
  #   }
  #   return(res60)
  # }


  delta_s <- function (test){
    sq=shp_grid@data$square
    sq_ID=unique(sq)

    res=c()
    for (i in 1:length(sq_ID))
    {
      sel=which(sq==sq_ID[i])
      v=c()
      for (j in sel) {v=c(v,test[[j]])}
      temp=data.frame(square=sq_ID[i],acc_med=acc_med(v),acc_med2=acc_med2(v), acc_mean=acc_mean(v), aff_med=aff_med(v), aff_mean=aff_mean(v),acc_mean2=acc_mean2(v),aff_med2=aff_med2(v),aff_mean2=aff_mean2(v),aff_sum=aff_sum(v),acc_sum=acc_sum(v),absmed=absmed(v),absmean=absmean(v),sur_aff=sur_aff(v),sur_acc=sur_acc(v),not_na=not_na(v),sur=sur(v),
                      acc_75pct=acc_75pct(v),acc_ab30=acc_ab30(v),acc_abmin50=acc_abmin50(v),acc_abmin100=acc_abmin100(v),acc_abmin150=acc_abmin150(v),acc_abmin300=acc_abmin300(v))
      res=rbind(res,temp)
    }
    return(res)
  }


  lidar_union<-function(delta_grid,function_delta,square){
    data0<-function_delta(delta_grid)
    # data0$cdc_med<-data0$acc_med+data0$aff_med
    # data0$cdc_mean<-data0$acc_mean+data0$aff_mean
    # data0$cdc_med2<-data0$acc_med2+data0$aff_med2
    # data0$cdc_mean2<-data0$acc_mean2+data0$aff_mean2
    return(data0)
  }



  ###########################################################################################
  chm0=raster(paste0(dir,"/",file0))
  crs(chm0) <- "+init=epsg:2972"
  chm0<-cleanupCHM(chm0,60) # My library RasterMachine
  range(chm0[],na.rm = T)

  chm1=raster(paste0(dir,"/",file1))
  crs(chm1) <- "+init=epsg:2972"
  chm1<-cleanupCHM(chm1,60)
  range(chm1[],na.rm = T)
  #delta=chm0
  #delta=chm1-chm0

  mask0=raster(mask)
  # mask1=mask(delta1513, G1513,maskvalue=1)

  delta0=chm1-chm0
  # delta=delta0*mask0 #(Delta_NG)
  delta1=crop(delta0,mask0)
  mask1=crop(mask0,delta1)
  delta2=mask(delta1,mask1,maskvalue=1)#(Delta_NG)
  delta=focal(delta2, w=matrix(1,3,3), fun=mean,na.rm=TRUE)
  #plot(delta)

  #delta <- cover(delta1, delta0)
  # plot(delta)
  hist(delta[], breaks=100)
  # median(delta[which(delta[]>=0)], na.rm=T)
  # mean(delta[which(delta[]>=0)], na.rm=T)

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
  delta_grid<-raster::extract(delta, shp_grid)

  cols_period<-c("acc_med", "acc_med2", "acc_mean", "aff_med", "aff_mean", "acc_mean2", "aff_med2",
                 "aff_mean2", "aff_sum", "acc_sum","absmed","absmean","acc_75pct","acc_ab30","acc_abmin50","acc_abmin100","acc_abmin150","acc_abmin300")

  ### Function to normalize periods
  norm_period_fun<-function(x,period) { (as.numeric(as.character(x)))/period}

  data_s<-lidar_union(delta_grid,delta_s,"square")
  data_s[,cols_period] = apply(data_s[,cols_period], 2, norm_period_fun,period=period)
  data_s$year0=y0
  data_s$year1=y1
  # data_s$carre=60
  #data_s<-data_s[,c("square_60", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
  data_s<-merge(data_s,df_shp_grid[,c("square","Parcelle","trait")],by="square")
  # names(data_s)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
  names(data_s)[1]<-"square_id"
  head(data_s)

  # data_60<-lidar_union(delta_grid,delta_s,"square")
  # # data_60[,cols_period] = apply(data_60[,cols_period], 2, norm_period_fun,period=period)
  # data_60$year0=y0
  # data_60$year1=y1
  # data_60$square=60
  # #data_60<-data_60[,c("square_60", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
  # data_60<-merge(data_60,df_shp_grid[,c("square_60","trait")],by="square_60")
  # # names(data_60)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
  # names(data_60)[1]<-"square_id"
  # head(data_60)



  # data_120<-lidar_union(delta_grid,delta_s120,"square_120")
  # # data_120[,cols_period] = apply(data_120[,cols_period], 2, norm_period_fun,period=period)
  # data_120$year0=y0
  # data_120$year1=y1
  # data_120$square=120
  # #data_120<-data_120[,c("square_120", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
  # data_120<-merge(data_120,unique(df_shp_grid[,c("s120","trait")]),by.x="square_120",by.y="s120")
  # #names(data_120)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
  # names(data_120)[1]<-"square_id"
  # head(data_120)
  #
  # data_240<-lidar_union(delta_grid,delta_s240,"square_240")
  # # data_240[,cols_period] = apply(data_240[,cols_period], 2, norm_period_fun,period=period)
  # data_240$year0=y0
  # data_240$year1=y1
  # data_240$square=240
  # #data_240<-data_240[,c("square_240", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
  # data_240<-merge(data_240,unique(df_shp_grid[,c("square_240","trait")]),by="square_240")
  # #names(data_240)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
  # names(data_240)[1]<-"square_id"
  # head(data_240)
  #
  # variable="AGB"
  # data=data_60
  # square="60"



  #table1<-rbind(data_60,data_120,data_240)
  table1<-data_s
  table2<-cbind(nombre,dec,file0,file1)
  table3<-cbind(table1,table2)
  return(table3)
}

# head(table3)

