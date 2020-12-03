



# out_dir="D:/temp/vop/"
# working_dir=paste("Y:/shared/SIGParacou2.0/Parcelles/ParcellesEclateesNoBuffer/")


create_VOP_plot<-function(working_dir,out_dir){
require(sp)
require(maptools)
require(rgdal)
require(raster)

  fl=dir(working_dir, pattern=".shp") ### Important to note that there are no other shapefiles of other topics present
  #fl= gsub(".shp","", fl)

  setwd(working_dir)
  for (i in 1:length(fl))
  {
    ##shp=readOGR(dsn =working_dir, layer=fl[i])
    shp=shapefile(fl[i])
    plot(shp)
    coord=shp@polygons[[1]]@Polygons[[1]]@coords
    vec=coord[2,]-coord[1,]
    rho=atan(vec[1]/vec[2])

    #new rotation center (8 points per plot! for Paracou large plots)
    ppp=8 #points per plot, the plot, it has eight vertices
    center=c(mean(shp@polygons[[1]]@Polygons[[1]]@coords[1:ppp,1]), mean(shp@polygons[[1]]@Polygons[[1]]@coords[1:ppp,2]))

    coin_sud=coord[coord[,2]==min(coord[,2]),] #il faudrait trier sur deuxi?me coord pour ?tre g?n?ral!

    #creation matrice VOP
    # #Tr=R*T by definition
    # #so R*inv(R)*Tr=R*T so T=inv(R)*Tr
    # # but inv(R)=t(R) (R orthogonal!) so
    # t(R) %*% Tr= T

    id=diag(c(1,1,1,1))
    Trans=matrix(data=c(center[1], center[2], 0), nrow=1, dimnames = list(c(),c("x","y","z")))
    Tmat=id
    Tmat[1:3,4]=-as.numeric(Trans)

    #rotation by pi/2 -angle anti clockwise
    mat_rot=matrix(data=c(cos(rho),-sin(rho), 0, sin(rho),cos(rho),0,0,0,1),byrow=TRUE,nrow=3,ncol=3)
    R=id
    R[1:3,1]=mat_rot[,1]
    R[1:3,2]=mat_rot[,2]
    R[1:3,3]=mat_rot[,3]
    Tr= R %*% Tmat

    #visual check
    df=data.frame(coord[,1], coord[,2])

    df=data.frame(coord[,1], coord[,2],  rep(0, times=(ppp+1)), rep(1,times=(ppp+1)))
    dat=as.matrix(df)
    datbis=dat %*% t(Tr)
    plot(datbis[, 1:2])
    plot(dat %*% t(Tmat), col="red", main=fl[i])
    points(dat %*% t(Tr), col="blue")
    write.table(x=Tr,paste(out_dir,"/VOP_",fl[[i]],".txt", sep=''), row.names=F, col.names=F)
  }




  # rep(1:4, each = 2, len = 4)    # first 4 only.
  # rep(1:4, each = 2, len = 10)   # 8 integers plus two recycled 1's.
  # rep(1:4, each = 2, times = 3)  # length 24, 3 complete replications
  #
  #
  #
  # rep(1:4, 2)
  # rep(1:4, each = 2)       # not the same.
  # rep(1:4, c(2,2,2,2))

}


