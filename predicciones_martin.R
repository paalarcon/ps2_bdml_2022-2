
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

train_hogares1 <- na.omit(train_hogares)
colMeans(is.na(train_hogares))



test_hogares <- ifelse(is.na(test_hogares$p_ocupa),0)

# Dividir bases de train y test dentro del train --------------------------

set.seed(1234)

partition <- createDataPartition(y = train_hogares$Ingtotugarr , p = 1/3)[[1]]
pre_train <- 
pre_test <- 
  
  
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

# Modelos de Regresión ----------------------------------------------------

regressControl  <- trainControl(method="repeatedcv",
                                number = 4,
                                repeats = 5) 

regress <- train(Ingpcug ~ n_cotiza + n_ninis + n_ocupa + t_person_et,
                 data = train_hogares,
                 method  = "lm",
                 trControl = regressControl, 
                 tuneGrid  = expand.grid(intercept = FALSE))
regress

ridge<-train(Ingpcug ~ n_cotiza + n_ninis + n_ocupa + t_person_et,
             data = train_hogares,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = 1))
ridge

lasso<-train(Ingpcug ~ n_cotiza + n_ninis + n_ocupa + t_person_et + as.factor(P5000) + as.factor(P5010) + as.factor(P5090),
             data = train_hogares,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 1, lambda = 1)) 
lasso

# Modelos de Clasificación ------------------------------------------------
train_hogares <- train_hogares %>% 
  mutate(pobre_factor=factor(Pobre,levels=c(1,0),labels=c("pobre","no_pobre")) ,
         y_numeric=ifelse(Pobre==1, 1, 0) %>% as.numeric() , const=1) 

model_class <- as.formula("pobre_factor ~ n_cotiza + n_ninis + n_ocupa + t_person_et")

logit = train(model_class,
              data=train_hogares,
              method="glm",
              trControl =  control,
              family = "binomial",preProcess = c("center", "scale"))
logit



probit = train(Pobre ~ n_cotiza + n_ninis + n_ocupa  + t_person_et + P5090 + P5010,
               data=train_hogares,
               method="polr",
               trControl =  control,
               family = "binomial",
               preProcess = c("center", "scale"))
probit


set.seed(1234)

no_cores <- detectCores() - 1

cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

random_forest = train(pobre_factor ~ n_cotiza + n_ninis + n_ocupa,
                      data=train_hogares,
                      method="rf",
                      trControl =  control,
                      family = "binomial",
                      preProcess = c("center", "scale"))
stopCluster(cl)
registerDoSEQ()

random_forest  
  




