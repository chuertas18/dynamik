rm(list=ls())

library(sf)
library(data.table)
library(rgdal)
library(raster)
# library(doParallel)
# 
# registerDoParallel()
# getDoParWorkers()

### Paracou database
#ATTENTION, the database is quite heavy.
ParacouDB<-fread("G:/Mon Drive/TESIS/BACKUP/BD_Paracou_1984_2020.csv")
Par2013<-ParacouDB[censusyear==2013,]
rm(ParacouDB)

# Canopy model, 25 ppm from 2013.
#chm=raster("G:/Mon Drive/TESIS/BACKUP/decimation/CHM2013_25ppm_fo_comp_MNT13.tif")
chm=raster("C:/Users/clauh/Downloads/CHM2013_25ppm_fo_comp_MNT13.tif")


crs(chm) <- "+init=epsg:32622"
plot(chm)

# split db per plot

# plot_list=list()
# for (p in unique(Par2013$plot))
# {
#   plot_list[[p]]<-SpatialPointsDataFrame(coords = cbind(Par2013[plot==p,xutmr],Par2013[plot==p,yutmr]),Par2013[plot==p,])
# }
# 
# res = foreach(p=1:16) %dopar% {
#   raster::extract(chm,plot_list[[p]],
#                   buffer = 30,
#                   fun=median,
#                   sp=T)}
# 
# tot<-do.call(bind, res)

db1<-fread("C:/Users/clauh/Downloads/d1.csv")

# toto<-Par2013[!(unique(Par2013$idtree)),]
# Par2013[.(!(idtree==toto)),]

df2<-Par2013[!(Par2013$idtree==unique(db1_2$idtree)),]

db=NULL
#for (i in unique(Par2013$idtree))
for (i in unique(df2$idtree))
{
  id_sel<-SpatialPointsDataFrame(coords = cbind(Par2013[idtree==i,xutmr],Par2013[idtree==i,yutmr]),Par2013[idtree==i,])
  # print("Processing...",i)
  res<-raster::extract(chm,id_sel,buffer = 30,fun=median,sp=T)
  res<-as.data.frame(res)
  db<-rbind(db,res)
}

head(db)

write.csv(db,"C:/Users/clauh/Downloads/d3.csv")


db1<-fread("C:/Users/clauh/Downloads/d1.csv")
db2<-fread("C:/Users/clauh/Downloads/d2.csv")
db3<-fread("C:/Users/clauh/Downloads/d3.csv")
db1_3<-rbind(db1,db2,db3)
head(db1_3)

colnames(db1_3)[which(names(db1_3) == "CHM2013_25ppm_fo_comp_MNT13")] <- "HC"
names(db1_3)

write.csv(db1_3[,-1],"C:/Users/clauh/Downloads/HC_BDParacou_CIRAD_codeGreg_2013.csv")
