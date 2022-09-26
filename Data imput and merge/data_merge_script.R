<<<<<<< HEAD:Data imput and merge/data_merge_script.R
setwd("C:/Users/mlara/Desktop/BD&ML/Problem sets/Problem Set 2/Data") #cargo directorio de trabajo donde están los datos

##Cargamos paquetes necesarios
require(pacman)
p_load(tidyverse, ggplot2, MLmetrics, caret, here)

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
=======
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
p_load(tidyverse, ggplot2, MLmetrics, caret, here)

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

set.seed(1234)

id=sample(1:nrow(train_hogares),size=nrow(train_hogares)*0.7)
pre_train=train_hogares[id,] #Selecciona el 70% de las filas
pre_test=train_hogares[-id,] #Selecciona el 30% restante de filas
>>>>>>> 74e5039cb091cfd321fef98254949e7a4e5f1d13:data_merge_script.R
