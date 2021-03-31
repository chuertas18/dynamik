#' @title Tree field data
#' @description Connection to the postgres database, where inventory data is stored.
#' @param database Dataframe containing the database with the following columns:idtree,dbh,censusyear,dbh_dead,family,genus,wd,xfield,yfield, the two years must be included their difference is made by the attribute censusyear
#' @param
#' @return database with the calculated values of AGB, basal area and biovolume for two periods
#' @export tree_field_data
#' @examples
#' 
#' OJO remplazar con el tree_field_data

database=datos[which(datos$plot==16),]
year0_p16=2015
year1_p16=2019

tree_field_data_p16 <- function(database,year0_p16,year1_p16) {
  library(data.table)
  ## Database subset for each year
  period = year1_p16 - year0_p16
  datos <-as.data.table(database[which(database$plot==16),]) # The subset becomes more efficient if it is transformed into datatable
  extract_year0 = na.omit(unique(datos[censusyear == year0_p16 & codealive_cor == T, .(idtree,
                                                                                   dbh,
                                                                                   censusyear,
                                                                                   dbh_dead,
                                                                                   family,
                                                                                   genus,
                                                                                   xfield,
                                                                                   yfield,
                                                                                   xutm,
                                                                                   yutm,
                                                                                   habitat,
                                                                                   square_250,
                                                                                   square_125,
                                                                                   square_62,
                                                                                   plot,
                                                                                   trait,
                                                                                   ba,
                                                                                   agb,
                                                                                   wd,
                                                                                   area,
                                                                                   ba_dead,
                                                                                   agb_dead
  )])) 
  extract_year1 = na.omit(unique(datos[censusyear == year1 & codealive_cor == T, .(idtree,
                                                                                   dbh,
                                                                                   censusyear,
                                                                                   dbh_dead,
                                                                                   family,
                                                                                   genus,
                                                                                   xfield,
                                                                                   yfield,
                                                                                   xutm,
                                                                                   yutm,
                                                                                   habitat,
                                                                                   square_250,
                                                                                   square_125,
                                                                                   square_62,
                                                                                   plot,
                                                                                   trait,
                                                                                   ba,
                                                                                   agb,
                                                                                   wd,
                                                                                   area,
                                                                                   ba_dead,
                                                                                   agb_dead
  )])) 

  # extract_year0 = na.omit(unique(datos[censusyear == year0_p16 &
  #                                        codealive_cor == T, .(idtree,
  #                                                              dbh,
  #                                                              censusyear,
  #                                                              dbh_dead,
  #                                                              family,
  #                                                              genus,
  #                                                              wd,
  #                                                              xfield,
  #                                                              yfield,
  #                                                              xutm,
  #                                                              yutm,
  #                                                              habitat,
  #                                                              plot,
  #                                                              trait)])) ##### Revisar
  # extract_year1 = na.omit(unique(datos[censusyear == year1_p16 &
  #                                        codealive_cor == T, .(idtree,
  #                                                              dbh,
  #                                                              censusyear,
  #                                                              dbh_dead,
  #                                                              family,
  #                                                              genus,
  #                                                              wd,
  #                                                              xfield,
  #                                                              yfield,
  #                                                              xutm,
  #                                                              yutm,
  #                                                              habitat,
  #                                                              plot,
  #                                                              trait)])) ##### Revisar
  df = unique(merge(extract_year0, extract_year1, by = "idtree", all = T))
  # head(df)

  ## Classification of status between dead, survivors and recruits For this you must first replace NA of dbh by zero
  df$dbh.x[is.na(df$dbh.x)] <- 0
  df$dbh.y[is.na(df$dbh.y)] <- 0
  # Classify the status
  df$state = factor(
    ifelse(
      df$dbh.x > 0 & df$dbh.y > 0,
      'survivors',
      ifelse(df$dbh.x > 0 &
               df$dbh.y == 0, 'dead', 'recruits')
    ),
    levels = c('survivors', 'dead', 'recruits')
  )


  df$dbh_dead <-
    ifelse(is.na(df$dbh_dead.x) , df$dbh_dead.y, df$dbh_dead.x)
  df$wd <- ifelse(is.na(df$wd.x) , df$wd.y, df$wd.x)
  df$family <- ifelse(is.na(df$family.x) , df$family.y, df$family.x)
  df$genus <- ifelse(is.na(df$genus.x) , df$genus.y, df$genus.x)
  df$xfield <- ifelse(is.na(df$xfield.x) , df$xfield.y, df$xfield.x)
  df$yfield <- ifelse(is.na(df$yfield.x) , df$yfield.y, df$yfield.x)
  df$xutm <- ifelse(is.na(df$xutm.x) , df$xutm.y, df$xutm.x)
  df$yutm <- ifelse(is.na(df$yutm.x) , df$yutm.y, df$yutm.x)
  # df$habitat <- ifelse(is.na(df$habitat.x) , df$habitat.y, df$habitat.x)
  # df$square_125<- ifelse(is.na(df$square_125.x) , df$square_125.y, df$square_125.x)
  # df$parcela<- ifelse(is.na(df$plot.x) , df$plot.y, df$plot.x)
  # df$treat<- ifelse(is.na(df$trait.x) , df$trait.y, df$trait.x)



  df$dbh.y[which(df$state == "dead")] <-
    df$dbh_dead[which(df$state == "dead")]

  # select_cols<-c("idtree", "dbh.x", "censusyear.x", "dbh_dead.x",  "dbh.y", "censusyear.y", "state", "dbh_dead",
  #                "family", "genus","wd", "xfield", "yfield","xutm","yutm","habitat","parcela","treat")
  select_cols <-c("idtree",
                  "dbh.x",
                  "censusyear.x",
                  "dbh.y",
                  "censusyear.y",
                  "state",
                  "family",
                  "genus",
                  "wd",
                  "xfield",
                  "yfield",
                  "xutm",
                  "yutm"
    )

  df <- df[, select_cols, with = FALSE]
  names(df) <- c("idtree",
                "dbh0",
                "censusyear0",
                "dbh1",
                "censusyear1",
                "state",
                "family",
                "genus",
                "wd",
                "xfield",
                "yfield",
                "xutm",
                "yutm"
    )
  # names(df)<-c("idtree", "dbh0", "censusyear0", "dbh_dead0", "dbh1", "censusyear1",
  #              "state", "dbh_dead", "family", "genus","wd", "xfield", "yfield")

  #head(df)

  # Ecuacion area basal
  basal_area = function(vec_dbh) {
    (((vec_dbh / 2) ^ 2 * pi) * 0.0001)
  }

  ##### calculate aGB and BA at the alive individual level
  ## 0.001 is applied to change from AGB (kg) to Tons
  # df$agb0 <-
  #   agb_eq(df$wd,
  #          df$dbh0,
  #          height_eq(df$dbh0, "High"),
  #          df$family,
  #          df$genus)
  df$agv0 <-agb_eq(1, df$dbh0, height_eq(df$dbh0, "High"), df$family, df$genus)
  
  ########### P16
  # df$agb0[which(df$tr)] <-
  #   agb_eq(df$wd,
  #          df$dbh0,
  #          height_eq(df$dbh0, "High"),
  #          df$family,
  #          df$genus)
  df$agv0 <-agb_eq(1, df$dbh0, height_eq(df$dbh0, "High"), df$family, df$genus)
  
  
  
  df$ba0 <- basal_area(df$dbh0)



  df$agb1 <-
    agb_eq(df$wd,
           df$dbh1,
           height_eq(df$dbh1, "High"),
           df$family,
           df$genus)
  df$agv1 <-
    agb_eq(1, df$dbh1, height_eq(df$dbh1, "High"), df$family, df$genus)
  df$ba1 <- basal_area(df$dbh1)
  df$period <- period
  df$per <- paste0(year0_p16, "-", year1_p16)

  ##### calcular aGB y BA a nivel indivduo MUERTOS
  ## 0.001 is applied to change from AGB (kg) to Tons
  # df$agb_dead<-agb_eq(df$wd,df$dbh_dead,height_eq(df$dbh_dead,"High"),df$family,df$genus)
  # df$agv_dead<-agb_eq(1,df$dbh_dead,height_eq(df$dbh_dead,"High"),df$family,df$genus)
  # df$ba_dead<-basal_area(df$dbh_dead)
  #
  # df$agb_dead[which(df$state!="dead")]<-NA
  # df$agv_dead[which(df$state!="dead")]<-NA
  # df$ba_dead[which(df$state!="dead")]<-NA
  return(df)
}
