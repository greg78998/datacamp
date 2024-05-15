don <- readRDS("donT.RDS")
dim(don)
names(don)
set.seed(1234)
indtest <- sample(1:nrow(don),5000,replace=F)
boxplot(don$Y[indtest],don$Y[-indtest])
#############################################
saveRDS(don[indtest,],"dontest.RDS")
saveRDS(don[-indtest,],"donapp.RDS")
#############################################
donA=readRDS("donapp.RDS")
gp=1:10
iner=1:10
for(ii in gp){
  tmp=kmeans(donA[,-ncol(donA)],centers=ii)
  iner[ii]=tmp$tot.withinss/tmp$totss
}
plot(gp,iner,type="h")
gp3 <- kmeans(donA[,-ncol(donA)],3,nstart=100)
saveRDS(donA[gp3$cluster==1,],"donapp1.RDS")
saveRDS(donA[gp3$cluster==2,],"donapp2.RDS")
saveRDS(donA[gp3$cluster==3,],"donapp3.RDS")

