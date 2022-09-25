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
