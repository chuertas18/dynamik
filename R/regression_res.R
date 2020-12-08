# x=union$aff_med
# y=union$AGB_G
# trait=as.factor(union$trait)


#### resultados de la regression y grafica
regression_res<-function(x,y,trait,nom_out,scatterplot,dir_out){
  require(boot)
  v<-data.frame(cbind(y=y,x=x,trait=trait))
  # model<-lm(formula=Y~., data=v)
  v[is.na(v)] = 0
  model<-lm(y~x, data=v)

################
  # Bootstrap 95% CI for R-Squared
  # https://www.statmethods.net/advstats/bootstrapping.html
  # function to obtain R-Squared from the data
  ci_slope <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample
    fit <- lm(formula, data=d)
    return(summary(fit)$coefficients[2])
  }
  # bootstrapping with 1000 replications
  res_slope <- boot(data=v, statistic=ci_slope,
                  R=1000, formula=y~x)
  # view
  # res_slope
  # plot(results)

  # get 95% confidence interval
  ci_slope0=boot.ci(res_slope, type="bca")
  ci_slope=paste0(round(ci_slope0$bca[[4]],2),"-",round(ci_slope0$bca[[5]],2))

  ci_intercept <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample
    fit <- lm(formula, data=d)
    return(summary(fit)$coefficients[1])
  }
  # bootstrapping with 1000 replications
  res_intercept <- boot(data=v, statistic=ci_intercept,
                    R=1000, formula=y~x)
  ci_intercept0=boot.ci(res_intercept, type="bca")
  ci_intercept=paste0(round(ci_intercept0$bca[[4]],2),"-",round(ci_intercept0$bca[[5]],2))


  res <- data.frame(cbind(nom_out,intercept=round(summary(model)$coefficients[1],2),slope=round(summary(model)$coefficients[2],2),r2=round(summary(model)$adj.r.squared,2),
                          rmse=round((summary(model))$sigma,3),rrmse=round((((summary(model))$sigma)/mean(y))*100,3),n=length(x),ci_slope=ci_slope,ci_intercept=ci_intercept))

  #return(summary(model))

  if(scatterplot==TRUE){
    library(car)
    png(paste0(dir_out,"scatterplot",nom_out,".png"), width = 850, height = 600)
    scatterplot(y~x|trait, data=v, smooth=F, ellipse=list(levels=c(.95)),
                cex=1.5,
                xlab="Median canopy gain",
                ylab="AGWNPP",
                main=paste0(nom_out))
    dev.off()
  }
  return(res)

}


