################################################
# Con este código se pueden pronósticar series de tiempo univariadas
#
# Hecho por Mauricio Montiel el 10/04/2020
###############################################

# Preliminares
remove(list = ls())
setwd("~/GitHub/Forecasting/ARIMA")

#librerías
library(readxl)
library(tidyverse)
library(tseries)
library(urca)
library(forecast)

# Importar datos
sales <- read_excel("data.xlsx")
sales <- ts(sales, start = c(2014,01), frequency = 12)

# Grafico de datos en niveles
ts.plot(sales)

# Prueba de estacionariedad
ur.kpss(sales) %>% summary() #los datos en niveles no son estacionarios
ur.kpss(diff(sales)) %>% summary() # los datos diferenciados si son estacionarios

# grafica de los datos diferenciados
ts.plot(diff(sales))

##
# Modelo ARIMA
##

# AR representa una regresion de la variable cotra valores pasados de ella misma
# MA representa la media movil ponderada de los errores pasados del pronóstico.

# Combinando los dos modelos
# p = parte autorregresiva
# d = grado de difenrenciacion
# q = parte media movil

# Selección automatica del modelo ARIMA
modelo <- auto.arima(sales, seasonal=T, stepwise=T, approximation=T)
summary(modelo)
checkresiduals(modelo) # Los residuales no estan correlacionados


# Pronóstico
autoplot(forecast(modelo, h=9))

