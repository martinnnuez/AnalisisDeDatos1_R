#Proyeecto analisis de dato
setwd("C:/Users/marti/Desktop/Proyecto de Análisis de Datos I/P1")

matr<-matrix(c(94.09,90.45,99.38,73.56,74.39,98.81,103.55,115.23,129.06,117.61,197.18,207.31,217.5,226.05,222.74,102.93,117.51,119.92,112.01,101.1,83.14,89.59,87.76,96.43,82.94),
                ncol=5,byrow=F)
nam<-c("Premuda", "Ayuno", "Salvado 1","Salvado 2","Mezcla de malta")

datos<- data.frame(matr)

colnames(datos)<-nam

write.csv(datos, file = "EJ1.csv", row.names = FALSE)
