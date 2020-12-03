#' @title Pantropical biomass equation Chave et al., 2005
#' @description Pantropical equation of the paper
#' Chave, J., Réjou-Méchain, M., Búrquez, A., Chidumayo, E., Colgan, M.S., Delitti, W.B.C., Duque, A., Eid, T., Fearnside, P.M., Goodman, R.C., Henry, M., Martínez-Yrízar, A., Mugasha, W.A., Muller-Landau, H.C., Mencuccini, M., Nelson, B.W., Ngomanda, A., Nogueira, E.M., Ortiz-Malavassi, E., Pélissier, R., Ploton, P., Ryan, C.M., Saldarriaga, J.G., Vieilledent, G., 2014. Improved allometric models to estimate the aboveground biomass of tropical trees. Glob. Change Biol. 20, 3177–3190. https://doi.org/10.1111/gcb.12629
#' @param vec_wd Data vector with WD value (per individual) in g.cm-3
#' @param vec_dbh Data vector with the diameter (dbh) value in centimeters
#' @param vec_height Data vector with the height value in meters
#' @param vec_family Vector with the family name
#' @param vec_genus Vector with the Genus Name
#' @return AGB Aboveground biomass value
#' @export AGB_chave14
#' @examples
#'agb_eq(0.5,10,12,"Lauraceae","As")
#' wd<-c(0.5,0.5,0.5,0.5)
#' dbh<-c(10,10,10,10)
#' h<-c(12,12,12,12)
#' family<-c("Lauraceae","Arecaceae","Arecaceae","Fabaceae")
#' genus<-c("Ocotea","Geonoma","Euterpe","Acacia")
#' agb_eq(0.5,10,12,"Lauraceae","As")
#' agb_eq(0.5,10,12,"Arecaceae","Geonoma")
#' agb_eq(0.5,10,12,"Arecaceae","Euterpe")

agb_eq<-function(vec_wd,vec_dbh,vec_height,vec_family,vec_genus){
  require(dplyr)
  datos=data.frame(wd=vec_wd,dbh=vec_dbh,height=vec_height,family=vec_family,genus=vec_genus)
  db<- datos %>% mutate(agb = case_when(family == "Arecaceae"  & genus == "Euterpe" ~ ((exp(-3.863+2.987*log(dbh))* 0.001)),
                                        family == "Arecaceae"  & genus != "Euterpe" ~ ((exp(-3.3488 + 2.7483*log(dbh)))* 0.001),
                                        #family != "Arecaceae" ~ AGB_chave14(wd,dbh,height_eq(dbh,"Low"))* 0.001)
                                        # Equation of Chave et al., 2005, Units Mg (vec_wd=g cm???3, dbh=cm,vec_height=m)
                                        family != "Arecaceae" ~ (0.0673*(vec_wd*(vec_dbh^2)*vec_height)^0.976)* 0.001))
  return(db$agb)
}





