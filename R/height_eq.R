#' @title Height equation for the Paracou network plots
#' @description (Vincent et al., 2014)
#' Vincent, G., Sabatier, D., Rutishauser, E., 2014. Revisiting a universal airborne light detection and ranging approach for tropical forest carbon mapping: scaling-up from tree to stand to landscape. Oecologia 175, 439–443. https://doi.org/10.1007/s00442-014-2913-y
#' @param vec_dbh Tree diameter (dbh) vector in centimeters
#' @param type_height "Low" and "High" (There are two models, the high one is used for the P16 plot)
#' @return Height value in meters
#' @export height_eq
#' @examples height_eq(10,"low")
#'
#'
#'
height_eq<-function(vec_dbh,type_height)
{
  if(type_height=="Low"||type_height=="low"){
    resultat<-44*(1-exp(-0.07*(vec_dbh^0.71))) #  Greg's formula Low forest (Vincent et al., 2014)
    return(resultat)
  }else if  (type_height=="High"||type_height=="high"){
    resultat<-313*(1-exp(-0.02*(vec_dbh^0.42))) #  Greg's formula High forest (Vincent et al., 2014)
    return(resultat)
  }else {
    stop("Problems in defining the height equation if it is High or Low")
  }
}



