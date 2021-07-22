
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
library(AER)
library(plm)
library(boot)



#######################################################################
####################### GLM : regresión logística #####################  
#######################################################################


setwd("/Users/mouse/Dropbox/Seminario Stata/Datos") # este comando determina que el 
# directorio de trabajo es el que se indica

##### Levantamos datos desde excel
# El comando read_excel permite la lectura de bases de datos desde excel. 
DalosLevantados = read_excel("EPH-2019-04/usu_Individual_T419.xls") # levantamos los datos
# desde un archivo de excel

nombrevar = names(DalosLevantados)
varlist = c(nombrevar[1:5],"REGION","AGLOMERADO","PONDERA","CH03","CH04","CH06", 
            "CH07","CH12","CH13","CH14","ESTADO","CAT_OCUP","CAT_INAC","P47T",
            "ITF","IPCF","PP04D_COD") # Creamos un objeto con los nombres de las variables
# que nos interesa conservar para el análisis
# el comando "subset" nos permite seleccionar un subconjunto del data.frame según el 
# criterio "select". En este caso, el criterio es que nos seleccione las variables 
# que aparecen en el objeto varlist que creamos previamente. 
DalosLevantados_subset = subset(DalosLevantados, select=varlist) 

names(DalosLevantados_subset)[9] = "Parentesco" # Le cambiamos el nombre a la variables 9

# cambiamos la codificación de los valores de la variables Parentesco, desde categrías
# numéricas a categorías del tipo caracter. 
DalosLevantados_subset$Parentesco = factor(DalosLevantados_subset$Parentesco,labels = 
                                             c("Jefe/a","Pareja","Hijo/a","Yerno/Nuera","Nieto/a",
                                               "Madre/Padre","Suegro/a","Hermano/a","Otro Fam",
                                               "No Fami"))

names(DalosLevantados_subset)[10] = "Sexo"
DalosLevantados_subset$Sexo = factor(DalosLevantados_subset$Sexo,labels = c("Hombre","Mujer"))
names(DalosLevantados_subset)[11] = "Edad"
names(DalosLevantados_subset)[12] = "Estcivil"
DalosLevantados_subset$Estcivil = factor(DalosLevantados_subset$Estcivil,labels = 
                                           c("Unido/a","Casado/a","separado/a","viudo/a",
                                             "soltero/a","NsNr"))
names(DalosLevantados_subset)[13] = "EducaMax"
DalosLevantados_subset$EducaMax = factor(DalosLevantados_subset$EducaMax,labels = 
                                           c("NsNr2","Jardin","Primario","EGB","Secundario",
                                             "Polimodal","Terciario","Universitario",
                                             "Postgrado","Educ. Espe.","NsNr"))
names(DalosLevantados_subset)[14] = "EducaMaxFin"
DalosLevantados_subset$EducaMaxFin = factor(DalosLevantados_subset$EducaMaxFin,labels = 
                                              c("NsNr2","Si","No","NsNr"))
DalosLevantados_subset$ESTADO = factor(DalosLevantados_subset$ESTADO,labels = 
                                         c("NoRespuesta","Ocupado","Desocupado","Inactivo",
                                           "Menor"))
names(DalosLevantados_subset)[15] = "EducaMaxUltAno"
names(DalosLevantados_subset)[16] = "SitLaboral"
names(DalosLevantados_subset)[19] = "IngTotalInd"
names(DalosLevantados_subset)[20] = "IngTotalFam"
names(DalosLevantados_subset)[21] = "IngTotalFamPerc"
names(DalosLevantados_subset)[22] = "RamaActi"
DalosLevantados_subset$RamaActi =  as.numeric(DalosLevantados_subset$RamaActi)

Ind_2019_04_subset = DalosLevantados_subset

Datos2 = subset(Ind_2019_04_subset, 
                select = c("CODUSU","COMPONENTE","REGION","AGLOMERADO",
                           "Parentesco","Sexo","Edad", "Estcivil","SitLaboral",
                           "EducaMax","EducaMaxUltAno","EducaMaxFin"))
Datos2 = subset(na.omit(Datos2), SitLaboral =="Ocupado" | SitLaboral == "Desocupado") 
Datos2$Ocupado =  Datos2$SitLaboral=="Ocupado" 


Datos2$aeduc = rep(0,dim(Datos2)[1])
Datos2$aeduc[(Datos2$EducaMax == "Primario" | Datos2$EducaMax == "EGB") & 
               Datos2$EducaMaxFin == "Si"] = 7 
condicion = (Datos2$EducaMax == "Primario" | Datos2$EducaMax == "EGB") & 
  Datos2$EducaMaxFin == "No"
Datos2$aeduc[condicion] = as.numeric(Datos2$EducaMaxUltAno[condicion])
Datos2$aeduc[(Datos2$EducaMax == "Secundario" | Datos2$EducaMax == "Polimodal") & 
               Datos2$EducaMaxFin == "Si"] = 7+5
condicion = (Datos2$EducaMax == "Secundario" | Datos2$EducaMax == "Polimodal") & 
  Datos2$EducaMaxFin == "No"
Datos2$aeduc[condicion] = 7+as.numeric(Datos2$EducaMaxUltAno[condicion])
Datos2$aeduc[(Datos2$EducaMax == "Terciario") & 
               Datos2$EducaMaxFin == "Si"] = 7+5+2
condicion = (Datos2$EducaMax == "Terciario") & 
  Datos2$EducaMaxFin == "No"
Datos2$aeduc[condicion] = 7+5+as.numeric(Datos2$EducaMaxUltAno[condicion])
Datos2$aeduc[(Datos2$EducaMax == "Universitario") & 
               Datos2$EducaMaxFin == "Si"] = 7+5+5
condicion = (Datos2$EducaMax == "Universitario") & 
  Datos2$EducaMaxFin == "No"
Datos2$aeduc[condicion] = 7+5+as.numeric(Datos2$EducaMaxUltAno[condicion])
Datos2$aeduc[(Datos2$EducaMax == "Postgrado") & 
               Datos2$EducaMaxFin == "Si"] = 7+5+5+3
condicion = (Datos2$EducaMax == "Postgrado") & 
  Datos2$EducaMaxFin == "No"
Datos2$aeduc[condicion] = 7+5+5+as.numeric(Datos2$EducaMaxUltAno[condicion])

modelo.log = Ocupado ~ REGION + AGLOMERADO + Parentesco + Sexo + Edad + 
  Estcivil + poly(aeduc,2)
fit.logit <- glm(modelo.log, data=Datos2, family = binomial(link = "logit"))

fit.probit <- glm(modelo.log, data=Datos2, family= binomial(link = "probit"))
summary(fit.logit)
summary(fit.probit)

prediccion = inv.logit(predict(fit.logit, type="response"))

prediccion = cnorm(predict(fit.probit, type="response"))

datonuevo = data.frame("REGION" = c(43, 43), "AGLOMERADO"= c(13, 13), "Parentesco"= 
                       c("Jefe/a","Jefe/a"), "Sexo" = c("Mujer", "Hombre"), "Edad" = 
                         c(40,40), "Estcivil" = c("Casado/a", "Casado/a"), 
                       "aeduc" = c(17, 17))

pred.logit  = inv.logit(predict(fit.logit, type="response", newdata = datonuevo))
pred.logit
diff(pred.logit)

pred.probit  = pnorm(predict(fit.probit, type="response", newdata = datonuevo))
pred.probit
diff(pred.probit)



#######################################################################
############################ Datos de panel  ##########################  
#######################################################################

data("Fatalities")
View(Fatalities)
str(Fatalities)
head(Fatalities)

# Definimos la variable tasa de muertes
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# Trabajamos con dos años (primero y último)
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

# estimamos el modelo que trata de medir la relación entre impuestos y tasa de muertes 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimación de la diferencia de la tasa de muertes entres los años 1982 y 1988
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)
summary(fatal_diff_mod)

# incorporación del efecto fijo por estado
# Forma uno
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
summary(fatal_fe_lm_mod)

# Forma dos
Fatalities_demeaned <- with(Fatalities,
                            data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                       beertax = beertax - ave(beertax, state)))
summary(lm(fatal_rate ~ beertax - 1, data = Fatalities_demeaned))

# Forma tres 
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")
summary(fatal_fe_mod)

# Efecto Aleatorio
fatal_re_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "random")
summary(fatal_re_mod)

# incorporación del efecto fijo por estado y temporal
# Forma uno
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
summary(fatal_tefe_lm_mod)

# Forma dos
fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")
summary(fatal_tefe_mod)
