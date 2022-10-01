
#############################################################################
#
#              Big Data & Machine Learning for Applied Economics                   
#                       Problem Set 2: Predicting Poverty
#                                   Grupo: 
#                      Paula Alarcón    Código: 201812516
#                      Martín Lara      Código: 201711300
#                      Nicolás González Código: 201813698
#
#############################################################################

# Limpio mi lugar de trabajo
rm(list=ls())

# Asigno un directorio donde me guarde los resultados: 
setwd(choose.dir())

#setwd("C:/Users/mlara/Desktop/BD&ML/Problem sets/Problem Set 2/Data") #cargo directorio de trabajo donde están los datos

##Cargamos paquetes necesarios
require(pacman)
p_load(tidyverse , rio , caret ,  modelsummary , gamlr, ROCR, pROC, doParallel, parallel)

##Cargamos los datos desde el directorio de trabajo
train_personas <- readRDS("train_personas.Rds")
test_personas <- readRDS("test_personas.Rds")
train_hogares <- readRDS("train_hogares.Rds")
test_hogares <- readRDS("test_hogares.Rds")

sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)

train_hogares<-left_join(train_hogares,sum_ingresos)
colnames(train_hogares)

head(train_hogares[c("id","Ingtotug","Ingtot_hogar")])

table(train_hogares$Pobre)

train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand_2)

# Estadisticas Descriptivas:
train_hogares<- train_hogares %>% mutate(Pobre_string=ifelse(Pobre==1,'Pobre','No pobre'))
PieChart(Pobre_string, hole=0, values="%", data=train_hogares, fill=1:2, weights=train_hogares$fex_c, radius=1, main="")

# Merge de datasets train y test ----------------------------------------------------------

train_hogares <- train_hogares %>% subset(select = c("Ingpcug","Pobre",colnames(test_hogares)))
train_personas <- train_personas %>% subset(select = c("Ingtot",colnames(test_personas)))

detach("package:dplyr", unload = TRUE)
library(dplyr)

var_hogar = train_personas %>% group_by(id) %>%
  mutate(conteo=1,
         nini=ifelse(P6040>=18 & is.na(Oc),1,0),
         et=ifelse(P6040>=12 & P6040<=64,1,0),
         cotiza=ifelse(P6090==1, 1, 0))%>% 
  summarize(n_ocupa=sum(Oc,na.rm=T),
            t_person_et=sum(et,na.rm=T),
            t_personas=sum(conteo),
            p_ocupa=n_ocupa/t_person_et,
            n_ninis=sum(nini,na.rm=T),
            n_cotiza=sum(cotiza, na.rm=T))

train_hogares <- left_join(train_hogares, var_hogar, by = "id")

var_hogar_test <-  test_personas %>% group_by(id) %>%
  mutate(conteo=1,
         nini=ifelse(P6040>=18 & is.na(Oc),1,0),
         et=ifelse(P6040>=12 & P6040<=64,1,0),
         cotiza=ifelse(P6090==1, 1, 0)) %>% 
  summarize(n_ocupa=sum(Oc,na.rm=T),
            t_person_et=sum(et,na.rm=T),
            t_personas=sum(conteo),
            p_ocupa=n_ocupa/t_person_et,
            n_ninis=sum(nini,na.rm=T),
            n_cotiza=sum(cotiza, na.rm=T))

test_hogares <- left_join(test_hogares, var_hogar_test, by = "id")

colMeans(is.na(train_hogares))
colMeans(is.na(test_hogares))

train_hogares <- train_hogares[ ,-c(9, 10, 11)]

train_hogares <- na.omit(train_hogares1)

test_hogares[is.na(test_hogares)]=0

# Dividir bases de train y test dentro del train --------------------------

set.seed(1234)

partition_class <- createDataPartition(y = train_hogares$Pobre , p = 1/3)[[1]]
pre_train_class <- train_hogares[partition_class,]
pre_test_class <- train_hogares[-partition_class,]
  
partition_reg <- createDataPartition(y = train_hogares$Ingpcug , p = 1/3)[[1]]
pre_train_reg <- train_hogares[partition_reg,]
pre_test_reg <- train_hogares[-partition_reg,]


################################################################################
################################################################################
################################################################################

  
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

regressControl  <- trainControl(method="repeatedcv",
                                number = 4,
                                repeats = 5) 


# Modelos de Regresión ----------------------------------------------------


regress <- train(Ingpcug ~ n_cotiza + n_ninis + n_ocupa + t_person_et,
                 data = pre_train_reg,
                 method  = "lm",
                 trControl = regressControl, 
                 tuneGrid  = expand.grid(intercept = FALSE))
regress
pre_test_reg$pred_regress <- predict(regress, pre_test_reg, type = "raw") %>% as.numeric()


ridge<-train(Ingpcug ~ n_cotiza + n_ninis + n_ocupa,
             data = pre_train_reg,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = 1))
ridge
pre_test_reg$pred_ridge <- predict(ridge, pre_test_reg, type = "raw")


lasso<-train(Ingpcug ~ n_cotiza + n_ninis + n_ocupa + t_person_et,
             data = pre_train_reg,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 1, lambda = 1)) 
lasso
pre_test_reg$pred_lasso <- predict(lasso, pre_test_reg, type = "raw")

################################################################################

rmse_regress <- RMSE(pre_test_reg$pred_regress, pre_test_reg$Ingpcug)
rmse_ridge <- RMSE(pre_test_reg$pred_ridge, pre_test_reg$Ingpcug)
rmse_lasso <- RMSE(pre_test_reg$pred_lasso, pre_test_reg$Ingpcug)

# Modelos de Clasificación ------------------------------------------------
pre_train_class <- pre_train_class %>% 
  mutate(pobre_factor=factor(Pobre,levels=c(1,0),labels=c("pobre","no_pobre")) ,
         y_numeric=ifelse(Pobre==1, 1, 0) %>% as.numeric() , const=1) 

model_class <- as.formula("pobre_factor ~ n_cotiza + n_ninis + n_ocupa + t_person_et")

logit = train(model_class,
              data=pre_train_class,
              method="glm",
              trControl =  control,
              family = "binomial",preProcess = c("center", "scale"))
logit

pre_test_class$pred_logit <- predict(logit, pre_test_class, type = "raw")


logit2 = train(pobre_factor ~ n_cotiza + n_ninis + n_ocupa + P5010 + P5090,
              data=pre_train_class,
              method="glm",
              trControl =  control,
              family = "binomial",
              preProcess = c("center", "scale"))
logit2

pre_test_class$pred_logit2 <- predict(logit2, pre_test_class, type = "raw")

################################################################################

pre_test_class <- pre_test_class %>% mutate(pred_logit=ifelse(pred_logit=="pobre", 1, 0))
pre_test_class <- pre_test_class %>% mutate(pred_logit2=ifelse(pred_logit2=="pobre", 1, 0))

auc_logit <- auc(pre_test_class$pred_logit, pre_test_class$Pobre)
auc_logit
auc_logit2 <- auc(pre_test_class$pred_logit2, pre_test_class$Pobre)
auc_logit2

Model_Metrics <- data.frame

Modelo <- c("Regresión", "Ridge (RMSE)", "Lasso (RMSE)", "Logit1 (AUC)", "Logit2 (AUC)")

Stat <- c(rmse_regress, rmse_ridge, rmse_lasso, auc_logit, auc_logit2)

Métricas <- data.frame(Modelo, Stat)

#set.seed(1234)
#no_cores <- detectCores() - 1
#
#cl <- makePSOCKcluster(no_cores)
#registerDoParallel(cl)
#
#random_forest = train(pobre_factor ~ n_cotiza + n_ninis + n_ocupa,
 #                     data=pre_train_class,
  #                    method="rf",
   #                   trControl =  control,
    #                  family = "binomial",
     #                 preProcess = c("center", "scale"))
#stopCluster(cl)
#registerDoSEQ()

#random_forest  
  




