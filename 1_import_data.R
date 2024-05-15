matricule <- "else"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF/pg_pipeline/", sep = "")
} else {
  path_USER <- "C:/Users/cepe-s4-03/Desktop/datacamp" 
}


#apprentissage3


# 0 | chargement des libraries  -----

path_pg <- path_USER

source(paste0(path_pg,"//_before_chemins.R"))
source(paste0(path_pg,"//_before_libraries.R"))


# les données sont ici 

DB_Y <- fread(input = 'engieY.csv')
DB_X <- fread(input = 'engieX.csv')

data_ori <- DB_Y %>% left_join(DB_X, by = "ID") 



str(data_ori)
apply(data_ori, 2, function(x) sum(is.na(x)))

# pour traiter ce sujet, on va considérer que l'on va faire 4 modèles sur les turbines

data_ori <- data_ori %>% 
  filter(MAC_CODE== "WT1") %>% 
  rename(Y = TARGET)

ggplot(data_ori, aes(x = Date_time, y = Grid_voltage)) + 
         geom_line()

### on peut constater des valeurs atypiques 
### on peut constater des valeurs manquantes (manquants des données)
### changement de capteur? <- changement de variabilité total sur la fin de la période


### on décide de supprimer la variable puisque le comportement ne semble pas cohérent dans le temps
## dans un premier temps on choisit de ne pas utiliser la variable
data_ori <- data_ori %>% 
  select(-starts_with("Grid_voltage")) %>% 

# pour s'assurer de la variable est régulière 
  
indh <- data_ori$Date_time %% 6 

data_ori <- data_ori[indh==1]
na.omit(data_ori)

saveRDS(data_ori, "data_ori.RDS")
data_ori <- readRDS("data_ori.RDS")


summary(data_ori)
data_ori <- na.omit(data_ori)




data_ori <- data_ori %>% 
  select(-c(
  Nacelle_angle, Nacelle_angle_min,Nacelle_angle_max,Nacelle_angle_std,
  Absolute_wind_direction))


##### Attention aux angles et aux variables corrigées
varcorr <- names(data_ori %>% select(ends_with("_c")))


data_ori$abs_wind_sin <- sin(data_ori$Absolute_wind_direction_c/360*2*pi)
data_ori$abs_wind_cos <- cos(data_ori$Absolute_wind_direction_c/360*2*pi)

data_ori <- data_ori %>% select(-Absolute_wind_direction_c)

saveRDS(data_ori, "donh.RDS")

### Attention aussi si 

### --> attention  359 et 1£ c'est quasiment 
## pour traiter on peut utiliser les sin et cos n


data_ori <- readRDS("donh.RDS")

### On produit une donnée
library(FactoMineR)

XX <- data_ori %>% select(-c(Y,ID, MAC_CODE, Date_time), 
                          ends_with(c("_max","_min")))
resACP = PCA(XX)








nbC <- 10
part <- data.frame(k= seq(1,nbC))

for (ii in seq(1:nbC)){
  print(ii)
  km <- kmeans(XX, centers = ii, nstart = 100)
  part[ii,"btw"]=round(km$betweenss,0.1)
  part[ii,"tot.withing"]=km$tot.withinss
  part[ii,"tot"]=km$totss
  part[ii,"eq. R2"]=sum(km$withinss)/km$totss*100
}

gp4 <- kmeans(XX,centers= 3, nstart = 10)

plot(y = part$btw, x = part$k)

#### A ne pas faire --> on ne doit pas regarder Y 
boxplot(data_ori$Y~gp4$cluster)


### 3 groupes 
######" Les eoliennes en fonctionnement 
###### les eoliennes ont des soucis 
###### les éoliennes groupe avec une grande distribution

prop_training <-  0.75

bloc_test <- sample(1:100, size = dim(data_ori)[1], replace = TRUE)>prop_training*100

training_set <- data_ori[bloc_test == FALSE, ]
test_set <- data_ori[bloc_test == TRUE, ]


training_set_XX <- training_set %>% select(-c(ID,Y,MAC_CODE,Date_time)) 
nbC <- 10
part <- data.frame(k= seq(1,nbC))

for (ii in seq(1:nbC)){
  print(ii)
  km <- kmeans(training_set_XX, centers = ii, nstart = 100)
  part[ii,"btw"]=round(km$betweenss,0.1)
  part[ii,"tot.withing"]=km$tot.withinss
  part[ii,"tot"]=km$totss
  part[ii,"eq. R2"]=sum(km$withinss)/km$totss*100
}

plot(y = part$`eq. R2`, x = part$k)




