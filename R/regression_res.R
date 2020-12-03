#### resultados de la regression y grafica
regression_res<-function(x,y,trait,nom_out,scatterplot,dir_out){
  v<-data.frame(cbind(y=y,x=x,trait=trait))
  # model<-lm(formula=Y~., data=v)
  v[is.na(v)] = 0
  model<-lm(y~x, data=v)


  res <- data.frame(cbind(nom_out,intercept=round(summary(model)$coefficients[1],2),slope=round(summary(model)$coefficients[2],2),r2=round(summary(model)$adj.r.squared,2),
                          rmse=round((summary(model))$sigma,3),rrmse=round((((summary(model))$sigma)/mean(y))*100,3),n=length(x)))

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
