#' @title Tree field data
#' @description Connection to the postgres database, where inventory data is stored.
#' @param database Dataframe containing the database with the following columns:idtree,dbh,censusyear,dbh_dead,family,genus,wd,xfield,yfield, the two years must be included their difference is made by the attribute censusyear
#' @param
#' @return database with the calculated values of AGB, basal area and biovolume for two periods
#' @export tree_field_data
#' @examples

# database=datos[which(datos$trait!="T4"),]
# year0=1999
# year1=2019
# p16=FALSE



tree_field_data <- function(database,year0,year1,p16) {
  library(data.table)
  source("D:/CLAUDIA/github/dynamik/R/AGB_eq.R")
  source("D:/CLAUDIA/github/dynamik/R/height_eq.R")
  ## Database subset for each year
  period = year1 - year0
  datos <-as.data.table(database) # The subset becomes more efficient if it is transformed into datatable
  extract_year0 = na.omit(unique(datos[censusyear == year0 &
                                         codealive_cor == T, .(idtree,
                                                               dbh,
                                                               censusyear,
                                                               dbh_dead,
                                                               family,
                                                               genus,
                                                               wd,
                                                               xfield,
                                                               yfield,
                                                               xutm,
                                                               yutm,
                                                               habitat,
                                                               square,
                                                               plot,
                                                               trait)])) ##### Revisar
  extract_year1 = na.omit(unique(datos[censusyear == year1 &
                                         codealive_cor == T, .(idtree,
                                                               dbh,
                                                               censusyear,
                                                               dbh_dead,
                                                               family,
                                                               genus,
                                                               wd,
                                                               xfield,
                                                               yfield,
                                                               xutm,
                                                               yutm,
                                                               habitat,
                                                               square,
                                                               plot,
                                                               trait)])) ##### Revisar
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


  df$dbh_dead <-ifelse(is.na(df$dbh_dead.x) , df$dbh_dead.y, df$dbh_dead.x)
  df$wd <- ifelse(is.na(df$wd.x) , df$wd.y, df$wd.x)
  df$family <- ifelse(is.na(df$family.x) , df$family.y, df$family.x)
  df$genus <- ifelse(is.na(df$genus.x) , df$genus.y, df$genus.x)
  df$xfield <- ifelse(is.na(df$xfield.x) , df$xfield.y, df$xfield.x)
  df$yfield <- ifelse(is.na(df$yfield.x) , df$yfield.y, df$yfield.x)
  df$xutm <- ifelse(is.na(df$xutm.x) , df$xutm.y, df$xutm.x)
  df$yutm <- ifelse(is.na(df$yutm.x) , df$yutm.y, df$yutm.x)
  df$habitat <- ifelse(is.na(df$habitat.x) , df$habitat.y, df$habitat.x)
  df$square<- ifelse(is.na(df$square.x) , df$square.y, df$square.x)
  df$parcela<- ifelse(is.na(df$plot.x) , df$plot.y, df$plot.x)
  df$treat<- ifelse(is.na(df$trait.x) , df$trait.y, df$trait.x)



  df$dbh.y[which(df$state == "dead")] <-df$dbh_dead[which(df$state == "dead")]

  
  # select_cols<-c("idtree", "dbh.x", "censusyear.x", "dbh_dead.x",  "dbh.y", "censusyear.y", "state", "dbh_dead",
  #                "family", "genus","wd", "xfield", "yfield")
  select_cols <-
    c(
      "idtree",
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
      "yutm",
      "habitat",
      "square",
      "parcela",
      "treat"
    )

  df <- df[, select_cols, with = FALSE]
  names(df) <-
    c(
      "idtree",
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
      "yutm",
      "habitat",
      "square",
      "parcela",
      "treat"
    )
  # names(df)<-c("idtree", "dbh0", "censusyear0", "dbh_dead0", "dbh1", "censusyear1",
  #              "state", "dbh_dead", "family", "genus","wd", "xfield", "yfield")

  #head(df)

  # Ecuacion area basal
  basal_area = function(vec_dbh) {
    (((vec_dbh / 2) ^ 2 * pi) * 0.0001)
  }
  ##### calculate aGB and BA at the alive individual level
  ## 0.001 is applied to change from AGB (kg) to Tons # Without P16
  if (p16==FALSE) {
    df$agb0 <- agb_eq(df$wd,df$dbh0,height_eq(df$dbh0, "Low"),df$family,df$genus)
    df$agv0 <-agb_eq(1, df$dbh0, height_eq(df$dbh0, "Low"), df$family, df$genus)
    df$agb1 <-agb_eq(df$wd,df$dbh1, height_eq(df$dbh1, "Low"), df$family,df$genus)
    df$agv1 <-agb_eq(1, df$dbh1, height_eq(df$dbh1, "Low"), df$family, df$genus)
  } else {
    print("Remember that you only have to enter the values for plot P16")
    df$agb0 <- agb_eq(df$wd,df$dbh0,height_eq(df$dbh0, "High"),df$family,df$genus)
    df$agv0 <-agb_eq(1, df$dbh0, height_eq(df$dbh0, "High"), df$family, df$genus)
    df$agb1 <-agb_eq(df$wd,df$dbh1, height_eq(df$dbh1, "High"), df$family,df$genus)
    df$agv1 <-agb_eq(1, df$dbh1, height_eq(df$dbh1, "High"), df$family, df$genus)
  }
  
  
  
  
  df$ba0 <- basal_area(df$dbh0)
  df$ba1 <- basal_area(df$dbh1)
  df$period <- period
  df$per <- paste0(year0, "-", year1)

  ##### calcular aGB y BA a nivel indivduo MUERTOS
  ## 0.001 is applied to change from AGB (kg) to Tons
  # df$agb_dead<-agb_eq(df$wd,df$dbh_dead,height_eq(df$dbh_dead,"Low"),df$family,df$genus)
  # df$agv_dead<-agb_eq(1,df$dbh_dead,height_eq(df$dbh_dead,"Low"),df$family,df$genus)
  # df$ba_dead<-basal_area(df$dbh_dead)
  #
  # df$agb_dead[which(df$state!="dead")]<-NA
  # df$agv_dead[which(df$state!="dead")]<-NA
  # df$ba_dead[which(df$state!="dead")]<-NA
  return(df)
}
