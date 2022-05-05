#Proyecto analisis de datos
library(tidyverse)
library(caret)
library(h2o)
setwd("C:/Users/marti/Desktop/Proyecto de Análisis de Datos I/P1")
datos<-read.csv("base_regresion.csv", header=T)
#datos<-as.tibble(datos)

require(psych) 
multi.hist(x = datos[,-c(1,3,4)], dcol = c("blue", "red"), dlty = c("dotted", "solid"), mar= c(5,4,4,2) + .1)

#Aplico logaritmo natural

datos<-datos %>% 
  mutate("Ln(precio)" =log(datos$precio))

names(datos)





#Analisis de las variables
glimpse(datos)
#Todas estan bien codificadas
#id no incluirla
#Precio es la variable respuesta
#Valoraciones: cantidad de personas que valoran
#Puntaje: hay muchos 0 que se corresponden con la cantidad de personas que las valoraron
#Podria crear una variable valoraciones por puntaje y asi saco informacion redundante del modelo y conservo lo que ya hay, muy relacionadas
#O bien tomar esos 0 como NA

#convierto variables a lo que son en realidad:
datos$tipo<-as.factor(datos$tipo)

# Número de observaciones del set de datos 
nrow(datos)

# Detección si hay alguna fila incompleta 
any(!complete.cases(datos))
#No hay datos ausentes

# Creación de un cluster local con todos los cores disponibles. 
h2o.init()

datos_path <- "C:/Users/marti/Desktop//base_regresion.csv"
datos_h2o <- h2o.importFile(path = datos_path)

datos_h2o <-datos_h2o[,-1]

h2o.describe(datos_h2o)

datos_h2o["tipo"] <- as.factor(datos_h2o["tipo"])

# Nombre de las columnas 
h2o.colnames(datos_h2o)

# Índices 
indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric") 
indices

#Analisis de correlacion
indices <- h2o.columns_by_type(object = datos_h2o, coltype = "numeric") 
h2o.cor(x = datos_h2o[,indices], y = NULL, na.rm = TRUE)
corr <- as.data.frame(h2o.cor(x = datos_h2o[,indices], y = NULL, na.rm = TRUE))
write.table(corr, file = "corr.txt", row.names = FALSE)

#No hay gran correlacion entre valoraciones y puntaje, asique voy a ver de poner 0 como NA

#Modelado

y <- "precio"

#SEPARACION DATOS
splits <- h2o.splitFrame(datos_h2o, ratios = 0.8, seed = 1996)
train <- splits[[1]]
test <- splits[[2]]

#AUTOML CANCU
aml_sup <- h2o.automl(
  y = "precio",
  training_frame = train,
  leaderboard_frame = test, #para hacer la tabla de leaderboard usa el test!!
  seed = 1996,
  exclude_algos = c("XGBoost", "StackedEnsemble","DeepLearning")
)

?h2o.automl
#LEADERBOARD
print(aml_sup@leaderboard)
lb <- as.data.frame(aml_sup@leaderboard)
write.csv(lb, file = "leaderboard.csv", row.names = FALSE)

#COMO ACCEDER A LOS MODELOS QUE NO SON EL LEADER
model_ids <- as.data.frame(aml_sup@leaderboard$model_id)[,1] 
model_ids
GBM <- h2o.getModel(model_ids[14])
GBM@parameters

#Performance del leader
perf <- h2o.performance(GBM, test)
perf

#Leaderboard con todos los modelos
lb <- h2o.get_leaderboard(object = aml_sup, extra_columns = 'ALL')
lb

#ESCRIBE EL MODELO FINAL
predictores <-c("DIA","MES","TEMP1","HUM1","TIPOCALL","PERDIA","PNM","DD","FF","INTTRAF","PRESVEG")  
var_respuesta <- "PM25" 

modelo_gbmfinal <- h2o.gbm( 
  # Variable respuesta y predictores. 
  y = var_respuesta, 
  x = predictores,
  # Datos de entrenamiento. 
  training_frame = train, 
  # Datos de validación para estimar el error. 
  validation_frame = test, 
  nfolds=5,
  keep_cross_validation_models=FALSE,
  keep_cross_validation_predictions=TRUE,
  score_tree_interval= 5,
  fold_assignment= "Modulo",
  ntrees = 55,
  max_depth=10,
  stopping_metric="deviance",
  stopping_tolerance=0.05,
  seed=2000,
  distribution="gaussian",
  sample_rate= 0.8,
  col_sample_rate=0.8,
  col_sample_rate_per_tree=0.8) 

modelo_gbmfinal
h2o.r2(modelo_gbmfinal, train = TRUE, valid = T, xval = T)
