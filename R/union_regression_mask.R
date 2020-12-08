# rm(list=setdiff(ls(), c("datos_postgres","tree_field_data","dynamics_grid_ground","lidar_metrics")))
# ##########################################################################################################################
# #######
# ##########################################################################################################################
# library(dynamik)
# library(raster) # grid
#
#
#
# ##########################################################################################################################
# work_dir<-"D:/temp"
# setwd(work_dir)
# wd_data<-"D:/temp/Data/Wood_Density_Paracou_all_idtree8419_200520.csv" # Table with wood density values per idtree # Table with wood density values per idtree
#
# ##########################################################################################################################
# # FUNCTIONS
# ##########################################################################################################################
#
# ##########################################################################################################################
# if(exists("datos_postgres")==FALSE){
#   datos_postgres=request_postgres(dbname="paracou",host="localhost",port=5432,user="postgres",password="postgres",sql_postgres="SELECT DISTINCT * FROM  paracou.bd_plots_corrected WHERE dbh>9.6 ORDER BY idtree, censusyear;")
# }
#
# head(datos_postgres,10)
# wd_data<-read.table(file = wd_data, sep=",",dec=".",header=TRUE) # Wood density table
#
# # Selection of only the plot data
# datos<-datos_postgres
# # Join to the wood density database
# datos=merge(datos,wd_data[,c("idtree","wd")], by="idtree", all.x=T)
# head(datos)
#
# d0=tree_field_data(datos,2011,2013)
# d_sq=dynamics_grid_ground(dataframe=d0,shp_grid="D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental/DATA/GRILLE/Plots120x120.shp",sq=120)
# head(d_sq)


# # Calculates turnover indicators for a certain grid
# if(chm_rotate==TRUE){
#     d1=dynamics_grid_ground(d0,"D:/temp/grille/grille_250.shp",250)
#     #d1=dynamics_grid_ground(d0,"D:/temp/grille/grille_125.shp",125)
# }

##########################################################################
#### SQUARE 60
##########################################################################
# grilla="D:/CLAUDIA/PAPERS/5_Productivity/Scripts_AGB_incremental/DATA/GRILLE/Plots60x60_s38.shp"
# resol=60
# db_loop="D:/temp/data_lidar_60.csv"
# datos
# year0=2015
# year1=2019
# period_nom=year1-year0
# # chm_rotate=FALSE
datos=datos
db_loop="D:/temp/data_lidar_64.csv"
grilla="D:/GitHub/Paper_productivity/DATA/GRILLE/grille_125_s38.shp"
resol=125

union_regression_mask <- function(datos, db_loop, grilla, resol) {
  # Preparation of the database for the two periods
  data_loop = read.table(
    file = db_loop,
    sep = ",",
    dec = ".",
    header = TRUE
  ) # Wood density table
  #head(data_loop)
  data_loop$shp_grid <- grilla
  table = NULL
  res_total = NULL
  for (i in 1:nrow(data_loop)) {
    dir = as.character(data_loop[i, 1])
    chm0 = as.character(data_loop[i, 2])
    chm1 = as.character(data_loop[i, 3])
    y0 = as.numeric(data_loop[i, 4])
    y1 = as.numeric(data_loop[i, 5])
    decimated = as.character(data_loop[i, 6])
    nom_out = as.character(data_loop[i, 7])
    mask = as.character(data_loop[i, 8])
    shp_grid = as.character(data_loop[i, 9])


    d0 = tree_field_data(datos, y0, y1)
    # head(d0)


    #d_sq=dynamics_grid_ground(d0,grilla,resol,2)
    d_sq = dynamics_grid_ground(d0, grilla, resol)
    names(d_sq)[1] <- "square_id"
    d_sq$carre = resol
    d_sq$year0 = y0
    d_sq$year1 = y1
    #head(d_sq)




    # d=lidar_metrics(dir=dir,chm0=chm0,chm1=chm1,year0=y0,year1=y1,
    #                 decimated=decimated,nom_out=nom_out,shp_grid= shp_grid)

    d = lidar_metrics_mask(
      dir = dir,
      chm0 = chm0,
      chm1 = chm1,
      year0 = y0,
      year1 = y1,
      decimated = decimated,
      nom_out = nom_out,
      shp_grid = shp_grid,
      mask = mask
    )
    #table<-rbind(table,d)

    union <-merge(d_sq, d[, -which(names(d) %in% c("Parcelle", "trait", "year0", "year1"))], by =
              "square_id")
    #head(union)
    # dput(names(union))

    col_y = c(
      "AGB_G",
      "AGV_G",
      "BA_G",
      "AGB_D",
      "AGV_D",
      "BA_D",
      "agb0",
      "agv0",
      "ba0",
      "agb1",
      "agv1",
      "ba1",
      "N0",
      "N1",
      "N_D",
      "N_R",
      "WD0",
      "WD1"
    )

    col_x = c(
      "acc_med",
      "acc_med2",
      "acc_mean",
      "aff_med",
      "aff_mean",
      "acc_mean2",
      "aff_med2",
      "aff_mean2",
      "aff_sum",
      "acc_sum",
      "absmed",
      "absmean",
      "sur_aff",
      "sur_acc",
      "not_na",
      "sur",
      "acc_75pct",
      "acc_ab30",
      "acc_abmin50",
      "acc_abmin100",
      "acc_abmin150",
      "acc_abmin300"
    )


    db_res1 = NULL
    nom_out = paste0(nom_out, "_", resol, "_", y0, "_", y1)
    for (w in as.vector(unique(union$nombre))) {
      data1 = union
      data1 = data1[which(data1$nombre == w), ]
      data1[is.na(data1)] <- 0
      for (colx_s in col_x) {
        for (coly_s in col_y) {
          x_s <- data1[, colx_s]
          y_s <- data1[, coly_s]
          trait <- data1$trait.x
          db_res = regression_res(
            x = x_s,
            y = y_s,
            trait = as.factor(trait),
            nom_out = paste0(nom_out, "_", w),
            scatterplot = FALSE,
            dir_out = "D:/temp/plots/reg_251120/"
          )
          db_res$x <- colx_s
          db_res$y <- coly_s
          db_res$las <- w
          db_res$res <- resol
          db_res$y0 <- y0
          db_res$y1 <- y1
          db_res1 <- rbind(db_res1, db_res)

        }
      }
    }

    write.csv(union, file = paste0("D:/temp/Datatable_", nom_out, "_", i, ".csv"))
    write.csv(db_res1, file = paste0("D:/temp/db_res_", nom_out, "_", i, ".csv"))

    # table=rbind(table,union)
    # res_total=rbind(res_total,db_res1)
  }

  # write.csv(table, file=paste0("D:/temp/Datatable_total.csv"))
  # write.csv(res_total, file=paste0("D:/temp/db_res_total.csv"))

}
