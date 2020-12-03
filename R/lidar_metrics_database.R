# dir Directorio principal donde estan los datosd
# Make the Loop Database
dir<-"D:/CLAUDIA/PAPERS/Scripts_AGB_incremental"
nom_out<-"Full_09_15"
decimated<-"FALSE"
year0<-2009
year1<-2015
chm0<-"CHM_Paracou_2009_SEP_OCT.tif"
chm1<-"CHM_Paracou_2015_MNT15.tif"




lidar_metrics<-functions(dir,chm0,chm1,year0,year1,decimated,nom_out){
  data_loop<-data.frame(nombre=nom_out,dec=decimated,y0=year0,y1=year1,chm0=chm0,
                        chm1=chm1)
  # data_loop<-read.csv("data_loop_lidar2.csv")
  data_loop$per<-data_loop$y1-data_loop$y0
  data_loop
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
    acc=sum(v>=0)
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
    acc=sum(v>=0)
    return(moy * acc/surf)
  }

  acc_mean2=function(v){ #  mediane d'accroissement sans normalisation
    v=na.omit(v)
    moy=mean(v[v>0])
    return(moy)
  }

  aff_mean=function(v){
    v=na.omit(v)
    med=mean(v[v<0])
    surf=length(v)
    aff=sum(v<=0)
    return(-med * aff/surf)
  }

  aff_med=function(v){
    v=na.omit(v)
    med=median(v[v<0])
    surf=length(v)
    aff=sum(v<=0)
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



  # Function to add to 120 square LIDAR indicators (delta  raster)
  delta_s120 <- function (test){
    sq120=paste(shp60@data$square_240, shp60@data$square_120, sep="_")
    sq120_ID=unique(sq120)

    res120=c()
    for (i in 1:length(sq120_ID))
    {
      sel=which(sq120==sq120_ID[i])
      v=c()
      for (j in sel) {v=c(v,test[[j]])}
      temp=data.frame(square_120=sq120_ID[i],acc_med=acc_med(v), acc_med2=acc_med2(v), acc_mean=acc_mean(v), aff_med=aff_med(v), aff_mean=aff_mean(v),acc_mean2=acc_mean2(v),aff_med2=aff_med2(v),aff_mean2=aff_mean2(v),aff_sum=aff_sum(v),acc_sum=acc_sum(v))
      res120=rbind(res120,temp)
    }
    return(res120)
  }


  # Function to add to 240 square LIDAR indicators (delta  raster)
  delta_s240 <- function (test){
    sq240=shp60@data$square_240
    sq240_ID=unique(sq240)

    res240=c()
    for (i in 1:length(sq240_ID))
    {
      sel=which(sq240==sq240_ID[i])
      v=c()
      for (j in sel) {v=c(v,test[[j]])}
      temp=data.frame(square_240=sq240_ID[i],acc_med=acc_med(v),acc_med2=acc_med2(v), acc_mean=acc_mean(v), aff_med=aff_med(v), aff_mean=aff_mean(v),acc_mean2=acc_mean2(v),aff_med2=aff_med2(v),aff_mean2=aff_mean2(v),aff_sum=aff_sum(v),acc_sum=acc_sum(v))
      res240=rbind(res240,temp)
    }
    return(res240)
  }



  # Function to add to 60 square LIDAR indicators (delta  raster)
  delta_s60 <- function (test){
    sq60=shp60@data$square_60
    sq60_ID=unique(sq60)

    res60=c()
    for (i in 1:length(sq60_ID))
    {
      sel=which(sq60==sq60_ID[i])
      v=c()
      for (j in sel) {v=c(v,test[[j]])}
      temp=data.frame(square_60=sq60_ID[i],acc_med=acc_med(v),acc_med2=acc_med2(v), acc_mean=acc_mean(v), aff_med=aff_med(v), aff_mean=aff_mean(v),acc_mean2=acc_mean2(v),aff_med2=aff_med2(v),aff_mean2=aff_mean2(v),aff_sum=aff_sum(v),acc_sum=acc_sum(v))
      res60=rbind(res60,temp)
    }
    return(res60)
  }


  lidar_union<-function(delta_grid,function_delta,square){
    data0<-function_delta(delta_grid)
    data0$cdc_med<-data0$acc_med+data0$aff_med
    data0$cdc_mean<-data0$acc_mean+data0$aff_mean
    data0$cdc_med2<-data0$acc_med2+data0$aff_med2
    data0$cdc_mean2<-data0$acc_mean2+data0$aff_mean2
    return(data0)
  }



  ###########################################################################################
  tabla<-NULL
  tabla2<-NULL


  for (i in 1:nrow(data_loop)){
    nombre=as.character(data_loop[i,1])
    dec=as.character(data_loop[i,2])
    y0=as.character(data_loop[i,3])
    y1=as.character(data_loop[i,4])
    period_nom=as.numeric(data_loop[i,7])
    file0=paste0("DATA/CHM/",as.character(data_loop[i,5]))
    file1=paste0("DATA/CHM/",as.character(data_loop[i,6]))
    print(paste0("Procesando",as.character(data_loop[i,1])))


    chm0=raster(file0)
    crs(chm0) <- "+init=epsg:32622"
    chm0<-cleanupCHM(chm0,60) # My library RasterMachine
    range(chm0[],na.rm = T)

    chm1=raster(file1)
    crs(chm1) <- "+init=epsg:32622"
    chm1<-cleanupCHM(chm1,60)
    range(chm1[],na.rm = T)
    delta=chm1-chm0
    # plot(delta)
    hist(delta[], breaks=100)
    # median(delta[which(delta[]>=0)], na.rm=T)
    # mean(delta[which(delta[]>=0)], na.rm=T)

    # grid (60, 120, 240)
    #shp60=readOGR(dsn="DATA/GRILLE", layer="Plots60x60")
    shp60=readOGR(dsn="DATA/GRILLE", layer="Plots60x60_s38")
    df_shp60<-as.data.frame(shp60)
    names(df_shp60)<-c("Parcelle", "trait", "square_60", "square_120", "square_240", "s120","id")

    #### This function allows the calculation of net and brute change
    #### indicators and merges the information with the ground data according to the scale
    # square is the variable by which they will be joined
    # function_delta, one of the following functions: delta_s120, delta_240, delta_60
    # delta_grid, The grid product of the subtraction
    # ground_data, data of the plots


    ### DELTA AGB
    delta_grid<-raster::extract(delta, shp60)

    cols_period<-c("acc_med", "acc_mean","acc_med2","acc_mean2","aff_med", "aff_mean","aff_med2","aff_mean2", "cdc_med", "cdc_mean",  "cdc_med2", "cdc_mean2")

    ### Function to normalize periods
    norm_period_fun<-function(x,period) { (as.numeric(as.character(x)))/period}

    data_60<-lidar_union(delta_grid,delta_s60,"square_60")
    data_60[,cols_period] = apply(data_60[,cols_period], 2, norm_period_fun,period=period_nom)
    data_60$year0=y0
    data_60$year1=y1
    data_60$square=60
    #data_60<-data_60[,c("square_60", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
    data_60<-merge(data_60,df_shp60[,c("square_60","trait")],by="square_60")
    # names(data_60)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
    names(data_60)[1]<-"square_id"
    head(data_60)



    data_120<-lidar_union(delta_grid,delta_s120,"square_120")
    data_120[,cols_period] = apply(data_120[,cols_period], 2, norm_period_fun,period=period_nom)
    data_120$year0=y0
    data_120$year1=y1
    data_120$square=120
    #data_120<-data_120[,c("square_120", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
    data_120<-merge(data_120,unique(df_shp60[,c("s120","trait")]),by.x="square_120",by.y="s120")
    #names(data_120)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
    names(data_120)[1]<-"square_id"
    head(data_120)

    data_240<-lidar_union(delta_grid,delta_s240,"square_240")
    data_240[,cols_period] = apply(data_240[,cols_period], 2, norm_period_fun,period=period_nom)
    data_240$year0=y0
    data_240$year1=y1
    data_240$square=240
    #data_240<-data_240[,c("square_240", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square")]
    data_240<-merge(data_240,unique(df_shp60[,c("square_240","trait")]),by="square_240")
    #names(data_240)<-c("square_id", "acc_med", "acc_med2", "acc_mean", "year0", "year1","square","trait")
    names(data_240)[1]<-"square_id"
    head(data_240)
    #
    # variable="AGB"
    # data=data_60
    # square="60"



    table1<-as.data.frame(rbind(data_60,data_120,data_240))
    table2<-cbind(nombre,dec,file0,file1)
    table2<-as.data.frame(cbind(table1,table2))
    tabla<-rbind(tabla,table2)

  }
}
  # head(tabla)
  # summary(tabla)
  # table(tabla$year0)
  # write.csv(tabla,paste0("D:/Mes Donnees/Images/nuevo/tabla_gain_lidar_s38_080720.csv"), row.names = FALSE)
  #






















