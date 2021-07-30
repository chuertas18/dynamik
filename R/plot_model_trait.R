plot_model_trait<-function(db,y,x,var,y_expression,x_expression,x_lab,y_lab,p_16,nexpl,titulo,ylim_min,ylim_max,xlim_min,xlim_max){
  
  
  if(p_16==TRUE){
    if(nexpl==TRUE){
      paleta<-c("#8856a7","#2ca25f")
    }
    paleta<-c("#8728e7","#2b83ba","#abdda4","#fdae61","#d7191c")
    shape_point<-c(14,15, 16, 17,18)
    shape_line<-c("twodash","twodash", "longdash","dotted","dotdash")
  }else {
    paleta<-c("#2b83ba","#abdda4","#fdae61","#d7191c")
    shape_point<-c(15, 16, 17,18)
    shape_line<-c("twodash", "longdash","dotted","dotdash")
  }
  
  lm_eqn <- function(db){
    m <- lm(y ~ x, db);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }
  
  
  p1<-ggplot(db, aes(y=y, x=x,fill=trait)) +
    geom_point(aes(color=trait,shape=trait),size=2.5) +
    geom_smooth(method=lm, aes(fill=trait))+
    labs(title=titulo,x=x_expression, y = y_expression)+
    geom_smooth(method = "lm", aes(colour = trait),se=FALSE)+
    scale_color_manual(values = paleta)+
    scale_fill_manual(values = paleta)+
    # scale_shape_manual(values= shape_point)+
    scale_linetype_manual(values= shape_line)+ 
    xlim(as.numeric(xlim_min),as.numeric(xlim_max))+
    ylim(as.numeric(ylim_min),as.numeric(ylim_max))+
    theme_bw() + 
    theme(axis.line = element_line( size = 1, linetype = "solid"), #Ejes
          title =element_text(size=15, face='bold'),
          text = element_text(size=rel(4.5)),
          axis.title.x = element_text(size=15), # Tamano xlab
          axis.title.y = element_text(size=15), # Tamano ylab
          axis.text=element_text(size=15),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=15))
    # geom_text(x = xlim_min, y = ylim_min,label = lm_eqn(db), parse = TRUE, size=4, hjust = -0.5)
  
  print(p1)
}
