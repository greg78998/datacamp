####Importation
library(data.table)
Y=fread("engieY.csv")
dim(Y)
X=fread("engieX.csv")
dim(X)
don <- merge(X,Y)
dim(don)
rm(X)
rm(Y)
names(don)
library(tidyverse)
don=rename(don,Y=TARGET)
names(don)
don$ID = NULL
table(don$MAC_CODE)
apply(is.na(don),2,sum)
#faire 4 modèles
don=filter(don,MAC_CODE=="WT1")%>%select(-MAC_CODE)
saveRDS(as.data.frame(don),"don.RDS")
don=readRDS("don.RDS")
dim(don)
names(don)
########
apply(is.na(don),2,sum)
library(ggplot2)
ggplot(don,aes(x=Date_time,y=Grid_voltage_std))+geom_line()
#####
##### on dégage les variables
#####
don <- select(don,-starts_with("Grid_voltage"))
dim(don)
indgrid <- grep("Grid_voltage",names(don))
indgrid
dim(don)
don <- don[,-c(indgrid)]
dim(don)
##### 
don[1:15,"Date_time"]
indh <- don$Date_time%%6
donh <- don[indh==1,]
dim(donh)
donh=na.omit(donh)
dim(donh)
saveRDS(donh[,-1],"donh.RDS")
######
summary(donh)
#########################################
## attention aux variables corrigées notées _c
##j'enlève à la main Nacelle_angle... et Absolute_wind_direction
#########################################
donh <- donh[,-c(46:49,54)]
#########################################
donh$AbsWindSin <- sin(donh$Absolute_wind_direction_c/360*2*pi)
donh$AbsWindCos <- cos(donh$Absolute_wind_direction_c/360*2*pi)
#########################################
donh <- select(donh,-Absolute_wind_direction_c)
saveRDS(donh,"donh.RDS")
donh=readRDS("donh.RDS")
dim(donh)
XX=donh[,-grep("Y",names(donh))]
library(FactoMineR)
resACP=PCA(XX)
##########################################
## y a t il des groupes d'individus ?
###########################################
gp=1:10
iner=1:10
for(ii in gp){
  tmp=kmeans(XX,centers=ii)
  iner[ii]=tmp$tot.withinss/tmp$totss
}
plot(gp,iner,type="h")

gp4=kmeans(XX,centers=4,nstart=10)
boxplot(donh$Y~gp4$cluster)

### et sans max et min ?
XXr <- select(XX,-ends_with("_min"),-ends_with("_max"))
gp=1:10
iner=1:10
for(ii in gp){
  tmp=kmeans(XXr,centers=ii)
  iner[ii]=tmp$tot.withinss/tmp$totss
}
plot(gp,iner,type="h")

gp3=kmeans(XX,centers=3,nstart=10)
boxplot(donh$Y~gp3$cluster)

donf <- data.frame(XXr,Y=donh$Y)
dim(donf)
saveRDS(donf,"donT.RDS")
