
rm(list = ls()) # limpia el workspace de R-Project
library(readxl)
library(dplyr)
library(plyr)
library(DescTools)
library(data.table)
library(EnvStats)
library(ggplot2)  
library(qcc)
library(plotly)
library(ggplot2)
library(ivreg)
library(lmtest)


#######################################################################
##################### Regresión lineal múltiple #######################  
#######################################################################

# determinamos el directorio de trabajo (no es obligatorio esto, per
# facilita levantar la información posteriormente)
setwd("~/Dropbox/Docencia/Docencia Curico/Econometría/Datos Varios")

Datos = read_excel("LaborSupplyData.xls") # Leemos los datos desde un archivo excel 
View(Datos) # miramos los datos 

# Sacamos algunos estadísticos descriptivos de las variables
summary(Datos)
summaryStats(Datos$hours, stats.in.rows = TRUE)

f <- list(family = "Courier New, monospace", size = 18, color = I("green"))
x <- list(title = "Frecuencia", titlefont = f)
y <- list(title = "y Axis", titlefont = f)

HistDensHoras0 = plot_ly(
  x = Datos$hours,
  type = "histogram", stroke = I("Black"), nbinsx = 20, name="Histograma horas") %>% 
  layout(yaxis=list(type='linear')) %>%
  layout(xaxis = x, yaxis = y,title = "Horas anuales trabajadas")
HistDensHoras0

# nos quedamos con los casos de las muejeres que han trabajado y por lo tanto 
# ofrecieron horas de trabajo

DatosReg = subset(Datos,hours>0)
summary(DatosReg)
summaryStats(hours ~ city, data = DatosReg, stats.in.rows = TRUE)
HistDensHoras1 = plot_ly(
  x =~hours, data =  subset(DatosReg, city== 1),
  type = "histogram", stroke = I("Black"), nbinsx = 20, name="Histograma horas") %>% 
  layout(yaxis=list(type='linear')) %>%
  layout(xaxis = x, yaxis = y,title = "Horas anuales trabajadas")
HistDensHoras1


# Modelo: se puede definir previamente o directamente definirlo en el comando de 
# estimación "lm"
modelo1 = hours ~ lwage + nwifeinc + kidslt6 + kidsge6 + age + educ 
class(modelo1)
fit.mod1 = lm(modelo1, data = DatosReg)
summary(fit.mod1)
# Comandos útiles para extraer información
coefficients(fit.mod1) # coeficientes
confint(fit.mod1, level=0.95) # intervalos de confianza para el modelo
fitted(fit.mod1) # valores ajustados (predicción)
residuals(fit.mod1) # residuos
vcov(fit.mod1) # matriz de varianza covarianza 

# Define una matriz para el gráfico de los resultados de estimación
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit.mod1)

# Prueba de heterocedasticidad 
bptest(fit.mod1)

# Corrección de la heterocedasticidad 
DatosReg$lresi2 = log(fit.mod1$residuals^2)
modelo.var = lresi2 ~ lwage + nwifeinc + age + educ + kidslt6 + kidsge6
varfunc.mco = lm(modelo.var, data = DatosReg)
summary(varfunc.mco)
DatosReg$varfunc = exp(varfunc.mco$fitted.values)
fit.mod2 = lm(modelo1, weights = 1/sqrt(varfunc), data = DatosReg)
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit.mod2)

bptest(fit.mod2)

# Llevamos a cabo una estimación donde existe una relación no lineal entre 
# la variable dependiente y una de las covariables
Datos1 = read_excel("CostDataUSAirlines.xlsx", 
                    col_types = c("skip", "skip", "numeric",
                                  "numeric", "numeric", "numeric"))
View(Datos1) # miramos los datos 
Datos1$Q2 = Datos1$Q^2

modelo1.Air = log(C) ~ Q  + PF + LF

Air.mco = lm(modelo1.Air, data = Datos1)
summary(Air.mco)
layout(matrix(c(1,2,3,4),2,2)) 
plot(Air.mco)

modelo2.Air = log(C) ~ Q + Q2 + PF + LF
Air2.mco = lm(modelo2.Air, data = Datos1)
summary(Air2.mco)
layout(matrix(c(1,2,3,4),2,2)) 
plot(Air2.mco)

Datos1$Q3 = Datos1$Q^3
modelo3.Air = log(C) ~ Q + Q2 + Q3 + PF + LF
Air3.mco = lm(modelo3.Air, data = Datos1)
summary(Air3.mco)
layout(matrix(c(1,2,3,4),2,2)) 
plot(Air3.mco)
bptest(Air3.mco)

#######################################################################
###################### Problemas de especificación ####################  
#######################################################################

# Efecto de la educación sobre el salario 
DatosReg$logwage = log(DatosReg$wage)
mod1.mco = lm(logwage ~ educ, data=DatosReg)
summary(mod1.mco)
layout(matrix(c(1,2,3,4),2,2)) 
plot(mod1.mco)

# Problema de endogeneidad 
Educpadre.mco = lm(educ ~ motheduc + fatheduc  , data=DatosReg)
summary(Educpadre.mco)
DatosReg$educhat = Educpadre.mco$fitted.values

Hausman.mco = lm(logwage ~ educ + educhat , data=DatosReg) # Prueba de Hausman
summary(Hausman.mco)

mod2.mco = lm(logwage ~ educhat, data=DatosReg)
summary(mod2.mco)

mod1.iv = ivreg(logwage ~ educ  | motheduc + fatheduc,
              data = DatosReg)
summary(mod1.iv)

modelo3 = hours ~ lwage + nwifeinc + age + educ + kidslt6 + kidsge6 |
  motheduc + fatheduc + exper + expersq + kidslt6 + kidsge6 + poly(educ, 4) +
  poly(age, 4) + poly(educ, 3)*age + poly(age, 3)*educ

fit.mod3 = ivreg(modelo3, data = DatosReg)
summary(fit.mod3)


