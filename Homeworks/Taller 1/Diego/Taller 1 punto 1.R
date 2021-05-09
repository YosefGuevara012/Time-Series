# Series de tiempo multivariada

rm(list=ls()) 
install.packages("MTS")
library(MTS)
install.packages("mvtnorm")
library(mtvnorm)
setwd("C:/Users/DDUQUE/OneDrive - Fundacion Corona/DIEGO DUQUE - PERSONAL/2.Universidad/2. ESTADÍSTICA/Series de tiempo multivariada")

#Cargar bases de datos

library(readr)
datos <- read_delim("GC=F (1).csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

#Crear series de tiempo
zt=(cbind(datos$Gold,datos$Oil,datos$Silver))
tdx=datos[,3]+datos[,4]+datos[,5]/12
require(MTS)
colnames(zt)
colnames(zt) <- c("Gold price","Oil price","Silver price")
MTSplot(zt,tdx)  #grafico 

plot(zt[,1],zt[,2],xlab="Golden price",ylab="Oil Price") ## Gráfico de puntos

ccm(zt)

#Matriz de varianzas y covarianzas

sig=diag(3)
x=rmvnorm(300,rep(0,3),sig)
MTSplot(x) 
ccm(x)
