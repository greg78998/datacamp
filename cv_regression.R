# Creation des blocs 

matricule <- "else"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF/pg_pipeline/", sep = "")
} else {
  path_USER <- "C:/Users/cepe-s4-03/Desktop/datacamp" 
}
setwd(path_USER)
DB_bis <- readRDS("donapp3.RDS")
extrait <- sample(1:dim(DB_bis)[1],dim(DB_bis)[1])<=dim(DB_bis)[1]
DB <- DB_bis[extrait,]
nb <- 7

ee <- readRDS("gptest.RDS")
DB_test <- readRDS("dontest.RDS")
DB_test_vf <- DB_test[ee==3,]


need <- TRUE 

# pour assurer la reproductivité
if (need){
  set.seed(1234)
  
  # pour distinguer le training du test
  # blocs_training_test <- sample(rep(1:10, length = nrow(DB)))<=(perc_training*10)
  
  
  # pour distinguer le train de l'eval
  blocs <- sample(rep(1:nb,length=nrow(DB)))
}


PREV <- DB %>% 
  select(Y)

XX <- model.matrix(Y~., data = DB)
YY <- DB$Y

for(ii in 1:nb){
  
  print(paste0("Tour de piste : ",ii))
  
  DB_train <- DB[blocs!=ii,]
  DB_eval <- DB[blocs==ii,]
  
  XX_train <- XX[blocs!=ii,]
  XX_eval <- XX[blocs==ii,]
  YY_train <- YY[blocs!=ii]
  YY_eval <- YY[blocs==ii]
  
  ###methode1
  lm_mdl <- lm(Y~.,data=DB_train)
  PREV[blocs==ii,"lm"] <- predict(lm_mdl,DB_eval)
  
  # algo backward AIC
  #algo2 <- step(lm_mdl,trace=0)
  # PREV[blocs==ii,"aic"] <- predict(algo2,donT)
  ##### algo backward bis (BIC)
  # algo3 <- step(logit,k=log(nrow(donA)),trace=0)
  # PREV[blocs==ii,"bic"] <- predict(algo3,donT)
  
  #####methode3
  arbre_mdl <- rpart(Y~.,data=DB_train)
  PREV[blocs==ii,"arbre"] <- predict(arbre_mdl,DB_eval)
  #rf_mdl <- randomForest(Y~.,data=DB_train)
  #PREV[blocs==ii,"foret"] <- predict(rf_mdl,DB_eval)
  ranger_mdl <- ranger(Y~.,data=DB_train, probability = FALSE)
  PREV[blocs==ii,"foretRanger"] <- predict(ranger_mdl,DB_eval)$prediction
  
  #####ridge
  ridge <- cv.glmnet(XX_train,YY_train,alpha=0,family="gaussian")
  PREV[blocs==ii,"ridmin"] <- predict(ridge,XX_eval,s="lambda.min")
  # PREV[blocs==ii,"rid1se"] <- predict(ridge,XXT,s="lambda.1se")
  
  #####lasso
  lasso <- cv.glmnet(XX_train,YY_train,alpha=1,family="gaussian")
  PREV[blocs==ii,"lasmin"] <- predict(lasso,XX_eval,s="lambda.min")
  # PREV[blocs==ii,"las1se"] <- predict(lasso,XXT,s="lambda.1se")
  
  #####elas
  elas_net <- cv.glmnet(XX_train,YY_train,alpha=.5,family="gaussian")
  PREV[blocs==ii,"elamin"] <- predict(elas_net,XX_eval,s="lambda.min")
  #PREV[blocs==ii,"ela1se"] <- predict(elas,XXT,s="lambda.1se")
  
  print("Gradient boosting") 
  xgb_train = xgb.DMatrix(data = XX_train, label = YY_train)
  xgb_test = xgb.DMatrix(data = XX_eval, label = YY_eval)
  
  eta_param = 0.7
  cv <- xgb.cv(data=xgb_train,nrounds=250,max_depth=2,nfold=5,verbose=0,eta=eta_param)
  
  iteropt <- which.min(cv$evaluation_log$test_rmse_mean)
  print(iteropt)
  model_xgboost = xgboost(data = xgb_train, max.depth = 2, 
                          nrounds = iteropt, eta=eta_param,
                          verbose = 0)
  PREV[blocs==ii,"xgb"] <- predict(model_xgboost,xgb_test)
  
}

erreur <- function(X,Y){mean((X-Y)^2)}

apply(PREV,2,erreur,Y=PREV$Y)







EST <- data.frame(Y=DB_test_vf$Y)

xgb_train = xgb.DMatrix(data = XX, label = YY)
XX_test <- model.matrix(Y~., data =DB_test_vf)
YY_test <- DB_test_vf$Y
xgb_test = xgb.DMatrix(data = XX_test, label = YY_test)


model_ranger <- ranger(Y~.,data=DB, probability = FALSE)
EST[,"ranger"] <- predict(model_ranger,DB_test_vf)$prediction

model_xgboost = xgboost(data = xgb_train, max.depth = 2, 
                        nrounds = 230, eta=eta_param,
                        verbose = 0)

EST[,"xgb"] <- predict(model_xgboost,xgb_test)

apply(EST,2,erreur,Y=EST$Y)



EST$aa <- 
