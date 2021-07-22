
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

#######################################################################
######################  Algunos comandos básicos ######################  
#######################################################################

# Creación de variables
a = 4 # escalar (número simple)
b = 5 # escalar (número simple)
c = a+b*2+a*b + a^2 - 10 # operaciones matemáticas básicas

# Creación de vectores y matrices
c = c(1,2,3,4,5) # el comando "c" crea un objeto que contiene los elementos que se 
                 # encuentran entre paréntesis. 5 elementos numéricos
d = c(6,7,8,9,10) # el comando "c" crea un objeto que contiene los elementos que se 
                  # encuentran entre paréntesis. 5 elementos numéricos
cc = c("a", "b", "c") # en este caso el objeto contiene letras, pero no hacen 
                      # referencias a los objetos a y b creadas previamentes

A = matrix(c(c,d),nrow = 5, byrow = FALSE) # crea una matriz de 5 files y 2 columnas
                                          # a partir de los objetos a y b
B = matrix(c(c,d),nrow = 5, byrow = TRUE) # crea una matriz de 5 files y 2 columnas, 
                                          # a partir de los objetos a y b, pero por fila
A*B # Producto elemento por elemento
A%*%t(B) # Producto matricial
C = array(1:75, dim = c(5,5,3)) # Array, matriz multidimensional

# Operaciones lógilas
# el comnado "if, else" permite realizar operaciones según se cumpla una o varias 
# condiciones
if(a > b){ # si se cumple la condición "a>b" entonces se ejecuta lo que está entre llaves,
           # sino, pasa a la siguiente condición. 
  print("a es mayor a b")
}else if(a == b){
  print("a es igual a b")
}else{
  print("a es menor a b")
}

# Bucles:
e = c()
for(i in 1:5){ #  permite repetir operaciones un número determinado de veces
  e = c(e, i^2)
} 
print(e)

e1 = c()
i = 0
while(i < 5) { #  permite repetir operaciones hasta que se deje de cumplir una condición
  i = i + 1
  e1 = c(e1, i^2)
}; print(e1)

# Números aleatorios
help("Distributions") # Nos muestra el listado de distribuciones aleatorias
  # Números aleatorios normales
n = 5
mu = 20
sigma = 2
X = rnorm(n,mu,sigma)
  # Números aleatorios Binomiales
Rep = 10; p = 0.7
Y = rbinom(n,Rep,p)

# Estadísticos 
n = 10000
lambda = 1/6
X = rexp(n,lambda)
  # Media
mean(X)
  # Desviación estándar y varianza
sd(X)
var(X)
  # Asimetría
skewness(X)
 # Curtosis
kurtosis(X)

#######################################################################
#############################  Data Frame's ###########################  
#######################################################################

# data.frame es un objeto que permite trabajar con bases de datos, cada fila
# es una observación y cada columna es una variable. 

n = 10 # cantidad de observaciones de cada una de las variables que formarán en data.frame
X1 = rnorm(n,10,3) # Muestra de una v.a. normal
X2 = rpois(n,5) # Muestra de una v.a. Poisson
X3 = rexp(n,1/5) # Muestra de una v.a. Exponencial
X4 = rbinom(n,15,0.4) # Muestra de una v.a. Binomial
X5 = rt(n,4)*sqrt(3) + 15 # Muestra de una v.a. t-Student
X6 = c(rep("Hola",2), rep("Chau",2),rep("No se", 6)) # variables tipo "string", de caracteres

datos = data.frame(cbind(X1,X2,X3,X4,X5,X6)) # Creamos la base de datos, el comando "cbind"
                                             # crea una matriz donde las columnas son cada una 
                                             # de las variables Xi's  

names(datos) # vemos el nombre de las variables o columnas

names(datos) = c("Var1", "Var2","Var3","Var4","Var5","Var6") # Cambiamos los nombres
                                                             # a las variables 
names(datos) # vemos el nombre de las variables o columnas

########## Extracción de información del data.frame

var1 = datos[1] # devuelve un data.frame fromado por la variable 1
var1y2y3 = datos[1:3] # devuelve un data.frame fromado por las variables 1, 2 y 3
var1y3y5 = datos[c(1,3,5)] # devuelve un data.frame fromado por las variables 1, 3 y 5
var1.1 = datos$Var1  # devuelve un vectos de datos de la variable 1
datos[10:15,2] # Devuelve los elementos de las filas 10 a 15 (ambos incluidos) 
               # de la columna 2 


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
                                       c("NsNr2","Jardin","Primerio","EGB","Secundario",
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

# Las expresiones que van desde la línea 183 hasta la 200 crean una variables 
# artificial similar a la variables parenteso de la base EPH. Esta expresión se
# llama función, es decir que estamos creando una función que puede ser utilizada
# más adelante asignándole la insumo "x".
componente = function(x){
  valores = unique(x)
  n = length(x)
  componente = rep(1,n)
  for(i in valores){
    cte = i
    k = 1
    for(j in 1:n){
      if(x[j] == cte & k == 1){
        k = k + 1
      }else if (x[j] == cte & k > 1){
        componente[j] = k
        k = k + 1
      }
    }
  }
  return(componente)
}

# aquí creamos dos data.frame que tienen 25 observaciones cada uno, y que entre
# ambas comparten 15 observaciones. Es decir, que 15 representan los mismos individuos
n1 = 25
n2 = 25
n0 = 15
id0 = sort(sample(1:15,15,replace = TRUE))
comp0 = componente(id0)
id1 = sort(sample(16:25,10,replace = TRUE))
comp1 = componente(id1)
id2 = sort(sample(26:35,10,replace = TRUE))
comp2 = componente(id2)

df1 = data.frame(matrix(c(id0,id1),ncol = 1), matrix(c(comp0,comp1),ncol = 1), "DF1")
df2 = data.frame(matrix(c(id0,id2),ncol = 1), matrix(c(comp0,comp2),ncol = 1), "DF2")
names(df1) = c("id","t","DF")
names(df2) = c("id","t","DF")
df1 = df1[with(df1,order(id,t)),]
row.names(df1) <- NULL
df2 = df2[with(df2,order(id,t)),]
row.names(df2) <- NULL

df1$x2 = rpois(n1,5)
df1$x3 = rpois(n1,100)

df2$y2 = rpois(n2,5)
df2$y3 = rpois(n2,100)


# Unimos dos bases que tienen dos variables de conexión
dfall = merge(df1,df2, by = c("id","t"), all= TRUE)
dfall$Fuente <- apply(dfall[c("DF.x", "DF.y")], 1, 
                          function(x) paste(na.omit(x), collapse = ""))
dfall$DF.x = NULL; dfall$DF.y = NULL

dfall

# Tablas resúmenes
tab = table(Ind_2019_04_subset$Sexo) # Frecuencias absolutas
addmargins(tab)
addmargins(round(prop.table(tab),3)) # Frecuencias absolutas

tab1 = table(Ind_2019_04_subset$Parentesco) # Frecuencias absolutas
addmargins(tab1)
addmargins(round(prop.table(tab1),3)) # Frecuencias absolutas
count(Ind_2019_04_subset, "Parentesco")

tab2 = table(Ind_2019_04_subset$Parentesco, Ind_2019_04_subset$Sexo)
addmargins(tab2)


#############################
#### Diagrama de Barras #####
#############################

tabla.Abs = table(Ind_2019_04_subset$Parentesco)
barplot(tabla.Abs, main="Situación de parentesco")
tabla.Rel = prop.table(tabla.Abs)
barplot(tabla.Rel*100, main="Situación de parentesco", ylab = "Porcentaje")

ggplot(Ind_2019_04_subset, aes(x = `Parentesco`)) + xlab("Situación de parentesco") + ylab("Frecuencia") +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig <- plot_ly(x = names(tab1), y = tab1, type = "bar", name ="Gráfico de barras")
fig

dataFig2 = data.frame(cbind(levels(Ind_2019_04_subset$Parentesco),(tab2[,1]),
                            (tab2[,2])))
dataFig2[,2] = as.numeric(dataFig2[,2])
dataFig2[,3] = as.numeric(dataFig2[,3])
names(dataFig2) = c("Parentesco","Hombre","Mujer")
fig2 = plot_ly(dataFig2, x = ~Parentesco, y = ~Hombre, type = "bar", name ="Hombres")
fig2 = fig2 %>% add_trace(y = ~Mujer, name = "Mujeres")
fig2 = fig2 %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
fig2

ggplot(Ind_2019_04_subset, aes(x = `Parentesco`)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Situación de parentesco") + 
  ylab("Frecuencia") + scale_y_continuous(labels = scales::percent, name = "Proporción") +
  facet_grid(~ Sexo) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Ind_2019_04_subset, aes(x = `Parentesco`)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Situación de parentesco") + 
  ylab("Frecuencia") + scale_y_continuous(labels = scales::percent, name = "Proporción") +
  facet_grid(~ Sexo) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Ind_2019_04_subset, aes(x = `EducaMax`)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Máximo Nivel Aducativo Alcanzado") + 
  ylab("Frecuencia") + scale_y_continuous(labels = scales::percent, name = "Proporción") +
  facet_grid(`SitLaboral` ~ Sexo) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Diagrama de Pareto #####
tabla.Pareto = table(Ind_2019_04_subset$EducaMax)
pareto.chart(tabla.Pareto,ylab = "Frecuencias", ylab2 = "Frecuencias Acumuladas", 
             main="Máximo Nivel educativo Alcanzado")


################################### Variables Cuantitativas #################################### 

################################################
####### Tabla de frecuencias e Histograma ######
################################################

# Primero necesitamos generar las clases
 
IngTotalFamPerc = Ind_2019_04_subset$IngTotalFamPerc 
summaryStats(IngTotalFamPerc, stats.in.rows = TRUE)

IPF_cat = cut(IngTotalFamPerc, breaks = c(1000, 5000, 10000, 20000, 30000, 
                                          50000))
Ind_2019_04_subset$IPF_cat = IPF_cat

ggplot(Ind_2019_04_subset, aes(x = `IPF_cat`)) + xlab("Ingresos Per-cápita Familiar") + ylab("Frecuencia") +
  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent, name = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Ind_2019_04_subset$IngFamPercCat = -NaN

Cat = c("0","1 - 5000","5001 - 10000", "10001 - 15000", "15001 - 20000", "20001 - 25000",
        "250001 - 30000"," Mayor 30K")

Ind_2019_04_subset$IngFamPercCat[Ind_2019_04_subset$IngTotalFamPerc == 0] = 0
Ind_2019_04_subset$IngFamPercCat[(Ind_2019_04_subset$IngTotalFamPerc > 0) & 
                                (Ind_2019_04_subset$IngTotalFamPerc <= 5000)] = 1
Ind_2019_04_subset$IngFamPercCat[(Ind_2019_04_subset$IngTotalFamPerc > 5000) & 
                                (Ind_2019_04_subset$IngTotalFamPerc <= 10000)] = 2
Ind_2019_04_subset$IngFamPercCat[(Ind_2019_04_subset$IngTotalFamPerc > 10000) & 
                                (Ind_2019_04_subset$IngTotalFamPerc <= 15000)] = 3
Ind_2019_04_subset$IngFamPercCat[(Ind_2019_04_subset$IngTotalFamPerc > 15000) & 
                                (Ind_2019_04_subset$IngTotalFamPerc <= 20000)] = 4
Ind_2019_04_subset$IngFamPercCat[(Ind_2019_04_subset$IngTotalFamPerc > 20000) & 
                                (Ind_2019_04_subset$IngTotalFamPerc <= 25000)] = 5
Ind_2019_04_subset$IngFamPercCat[(Ind_2019_04_subset$IngTotalFamPerc > 25000) & 
                                (Ind_2019_04_subset$IngTotalFamPerc <= 30000)] = 6
Ind_2019_04_subset$IngFamPercCat[(Ind_2019_04_subset$IngTotalFamPerc > 30000)] = 7

Ind_2019_04_subset$IngFamPercCat = factor(Ind_2019_04_subset$IngFamPercCat,labels = Cat)

ggplot(Ind_2019_04_subset, aes(x = `IngFamPercCat`)) + xlab("Ingresos Per-cápita Familiar") + ylab("Frecuencia") +
  geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels = scales::percent, name = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

HistIngFamPer = hist(Ind_2019_04_subset$IngTotalFamPerc[Ind_2019_04_subset$IngTotalFamPerc<150000],
                     breaks = 25, main = "Ingresos familiares per-cápita", ylab = "Frecuencia", 
                     xlab = "Ingresos en pesos")

#### Usando plotly ####
f <- list(family = "Courier New, monospace", size = 18, color = I("green"))
x <- list(title = "Frecuencia", titlefont = f)
y <- list(title = "y Axis", titlefont = f)

HistPlotlyIngFamPer = plot_ly(
  x = ~Ind_2019_04_subset$IngTotalFamPerc[Ind_2019_04_subset$IngTotalFamPerc<150000 & 
                                            Ind_2019_04_subset$IngTotalFamPerc>0],
               type = "histogram", stroke = I("Black"), nbinsx = 30,
               histnorm = "probability") %>% layout(yaxis=list(type='linear')) %>%
  layout(xaxis = x, yaxis = y)
HistPlotlyIngFamPer

HistIngFamPer = hist(Ind_2019_04_subset$IngTotalFamPerc[Ind_2019_04_subset$IngTotalFamPerc<150000 & 
                                                          Ind_2019_04_subset$IngTotalFamPerc>0],
                     breaks = 25, main = "Ingresos familiares per-cápita positivos", ylab = "Frecuencia", 
                     xlab = "Ingresos en pesos")


# Resumen estadístico del ingreso total individual por sexo
summaryStats(IngTotalInd ~ 1, data= na.omit(Ind_2019_04_subset),stats.in.rows = TRUE, 
             ci=FALSE, digits = 1, combine.groups = FALSE)

summaryStats(IngTotalInd ~ 1, data= na.omit(Ind_2019_04_subset[,"IngTotalInd"]),stats.in.rows = TRUE, 
             ci=FALSE, digits = 1, combine.groups = FALSE)

MediaIngInd = sum(Ind_2019_04_subset$IngTotalInd[Ind_2019_04_subset$IngTotalInd>=0], na.rm = TRUE)/
  (sum(Ind_2019_04_subset$IngTotalInd>=0,na.rm = TRUE))




