#Pre-requisitos 
#Antes de empezar a trabajar con los datos debemos fijar el directorio de trabajo.   

setwd("/Users/juandavidescobarescobar/Documents/Unir/Materias/Analisis e interpretacion de Datos/Actividad 1/")

#1. Carga de librerias

#Instalacion de librerias
if  (!requireNamespace("readr", quietly = TRUE)) {install.packages("readr")}
if  (!requireNamespace("dplyr", quietly = TRUE)) {install.packages("dplyr")}
if  (!requireNamespace("fdth", quietly = TRUE)) {install.packages("fdth")}
if  (!requireNamespace("berryFunctions", quietly = TRUE)) {install.packages("berryFunctions")}
if  (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")}
if  (!requireNamespace("PerformanceAnalytics", quietly = TRUE)) {install.packages("PerformanceAnalytics")}
if  (!requireNamespace("rpivotTable", quietly = TRUE)) {install.packages("rpivotTable")}
if  (!requireNamespace("sqldf", quietly = TRUE)) {install.packages("sqldf")}
if  (!requireNamespace("reshape2", quietly = TRUE)) {install.packages("reshape2")}
if  (!requireNamespace("ggeasy", quietly = TRUE)) {install.packages("ggeasy")}
if  (!requireNamespace("grid", quietly = TRUE)) {install.packages("grid")}
if  (!requireNamespace("gridBase", quietly = TRUE)) {install.packages("gridBase")}


#Carga de librerias
library(readr)
library(testthat)
library("plyr")
library("dplyr")
library(ggplot2)
library(Hmisc)
library(corrplot)
library(plyr)
library(fdth)
library(berryFunctions)
library(tidyverse)
library(PerformanceAnalytics)
library(rpivotTable)
library(sqldf)
library(reshape2)
library(ggeasy)
library(grid)
library(gridBase)
#-------------------------------------------------------------------------------------------------------------------
#                                                       EDA
#-------------------------------------------------------------------------------------------------------------------

#2. Carga de datos 
# Cargamos todos los conjuntos de datos que nos interesen en nuestro entorno de 
# desarrollo.

covid_survey <- 
  read_delim("/Users/juandavidescobarescobar/Documents/Unir/Materias/Analisis e interpretacion de Datos/Actividad 1/COVID-19 Survey Student Responses.csv",
             delim=",", col_names = TRUE, locale = locale(encoding = "UTF-8"))

View(covid_survey)
# Número total de observaciones = 1182

#3. Analisis exploratorio

str(covid_survey)

# Número total de variables = 19
# Numero de variables tipo cadena = 11
# Numero de variables tipo numerica = 8

# La variable "Time spent on TV" es de tipo numerica, pero cuenta con valores atipicos = [0 - 15], Atipicos [N, Bo tv, Blanks]
# dichos valores serán remplazados por el valor de cero ya que indican que no se invierte tiempo en ver TV, en pasos posteriores.

summary(covid_survey)

#Antes de realizar cualquier analisis cambiamos el nombre a la variable 

names(covid_survey)[names(covid_survey) == 'ID'] <- 'Id'
names(covid_survey)[names(covid_survey) == 'Region of residence'] <- 'RegionResidencia'
names(covid_survey)[names(covid_survey) == 'Age of Subject'] <- 'Edad'
names(covid_survey)[names(covid_survey) == 'Time spent on Online Class'] <- 'TiempoClaseOnline'
names(covid_survey)[names(covid_survey) == 'Rating of Online Class experience'] <- 'CalificacionClaseOnline'
names(covid_survey)[names(covid_survey) == 'Medium for online class'] <- 'MedioClaseOnline'
names(covid_survey)[names(covid_survey) == 'Time spent on self study'] <- 'TiempoAutoaprendizaje'
names(covid_survey)[names(covid_survey) == 'Time spent on fitness'] <- 'TiempoFitness'
names(covid_survey)[names(covid_survey) == 'Time spent on sleep'] <- 'TiempoDormir'
names(covid_survey)[names(covid_survey) == 'Time spent on social media'] <- 'TiempoRedesSociales'
names(covid_survey)[names(covid_survey) == 'Prefered social media platform'] <- 'PreferenciaRedSocial'
names(covid_survey)[names(covid_survey) == 'Time spent on TV'] <- 'TiempoTV'
names(covid_survey)[names(covid_survey) == 'Number of meals per day'] <- 'NumeroComidas'
names(covid_survey)[names(covid_survey) == 'Change in your weight'] <- 'CambioPeso'
names(covid_survey)[names(covid_survey) == 'Health issue during lockdown'] <- 'ProblemaSalud'
names(covid_survey)[names(covid_survey) == 'Stress busters'] <- 'EliminaEstres'
names(covid_survey)[names(covid_survey) == 'Time utilized'] <- 'TiempoUtilizado'
names(covid_survey)[names(covid_survey) == 'Do you find yourself more connected with your family, close friends , relatives  ?'] <- 'ConexionFamiliar'
names(covid_survey)[names(covid_survey) == 'What you miss the most'] <- 'QueExtrana'

# Rangos y valores:
"
1.  Id                                  = [R1 - R999], Count = 1182 uniques
2.  RegionResidencia                    = [Outside Delhi-NCR, Delhi-NCR] (Cualitativa-Nominal)
3.  Edad                                = [7 - 59] (Cuantitativa-Continua)
4.  TiempoClaseOnline                   = [0 - 10] (Cuantitativa-Continua)
5.  Rating of Online Class experience   = [Average, Excellent, Good, NA, poor, Very poor] (Cualitativa-Ordinal)
6.  MedioClaseOnline                    = [Any Gadget, Laptop/Desktop, NA, Smartphone, Smartphone or Laptop/Desktop, Tablet] (Cualitativa-Nominal)
7.  TiempoAutoaprendizaje               = [0 - 18] (Cuantitativa-Continua)
8.  TiempoFitness                       = [0 - 5] (Cuantitativa-Continua)
9.  TiempoDormir                        = [4 - 15] (Cuantitativa-Continua)
10. TiempoRedesSociales                 = [0 - 10] (Cuantitativa-Continua)
11. PreferenciaRedSocial                = [Elyment, Facebook, instagram, Linkedin, None, Omegle, Quora, Reddit, Snapshat, Talklife, Telegram, Twetter, What's app, Youtube] (Cualitativa-Nominal)
12. TiempoTV                            = [0 - 15], Atipicos [N, No tv, Blanks]
13. NumeroComidas                       = [1 - 8] (Cuantitativa-Discreta)
14. CambioPeso                          = [Decreased, Increased, Remain Constant] (Cualitativa-Ordinal)
15. ProblemaSalud                       = [YES, NO] (Cualitativa-Nominal)
16. EliminaEstres                       = [Activities] (Cualitativa-Nominal)
17. TiempoUtilizado                     = [YES, NO] (Cualitativa-Nominal)
18. Se encuentra más conectado con su familia, amigos, parientes? = [YES, NO] (Cualitativa-Nominal)
19. ¿QueExtraña?                        = [Activities and things] Atipicos [.] (Cualitativa-Nominal)
"

# Remplazar valores atipicos [N, No tv, Blanks]
covid_survey$TiempoTV[covid_survey$TiempoTV == "N"] <- 0
covid_survey$TiempoTV[covid_survey$TiempoTV == "No tv"] <- 0
covid_survey$TiempoTV[covid_survey$TiempoTV == ""] <- 0
covid_survey$TiempoTV[covid_survey$TiempoTV == "n"] <- 0
covid_survey$TiempoTV[is.na(covid_survey$TiempoTV)] <- 0

covid_survey$TiempoTV = as.numeric(covid_survey$TiempoTV)
str(covid_survey)
distinct(select(covid_survey, TiempoTV))

#Realizamos los histogramas variables numericas

par(mfrow=c(1,2))

Edad <- hist(covid_survey$Edad, main = "",
                xlab = "Edad", ylab = "Frecuencia",
                breaks = 1000, xlim = c(0,150))

TiempoClaseOnline <- hist(covid_survey$TiempoClaseOnline, main = "",
                  xlab = "TiempoClaseOnline", ylab = "Frecuencia",
                  breaks = 1000, xlim = c(0,150))

TiempoAutoaprendizaje <- hist(covid_survey$TiempoAutoaprendizaje, main = "",
                          xlab = "TiempoAutoaprendizaje", ylab = "Frecuencia",
                          breaks = 1000, xlim = c(0,150))

TiempoFitness <- hist(covid_survey$TiempoFitness, main = "",
                          xlab = "TiempoFitness", ylab = "Frecuencia",
                          breaks = 1000, xlim = c(0,150))

TiempoDormir <- hist(covid_survey$TiempoDormir, main = "",
                          xlab = "TiempoDormir", ylab = "Frecuencia",
                          breaks = 1000, xlim = c(0,150))

TiempoRedesSociales <- hist(covid_survey$TiempoRedesSociales, main = "",
                          xlab = "TiempoRedesSociales", ylab = "Frecuencia",
                          breaks = 1000, xlim = c(0,150))

TiempoTV <- hist(covid_survey$TiempoTV, main = "",
                          xlab = "TiempoTV", ylab = "Frecuencia",
                          breaks = 1000, xlim = c(0,150))

NumeroComidas <- hist(covid_survey$NumeroComidas, main = "",
                 xlab = "NumeroComidas", ylab = "Frecuencia",
                 breaks = 1000, xlim = c(0,150))

#-------------------------------------------------------------------------------------------------------------------
#                                                         Edad
#-------------------------------------------------------------------------------------------------------------------

# El resulado de los histogramas nos muestra valores no atipicos, por lo caul
# podemos comenzar a analizarlos.
covid_survey$Edad

# Agrupa los valores de la variable Edad y calcula la frecuencia abasoluta
tablaEdad <- as.data.frame(table(Edad = covid_survey$Edad))

# El valor con mayor frecuencia absoluta es la Moda = 20
tablaEdad

# Medidas de tendencia central
summary(covid_survey$Edad)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.00   17.00   20.00   20.17   21.00   59.00 

# Muestra la tabla de frecuencias completa
transform(tablaEdad,
          FreqAc = cumsum(Freq),
          Rel = round(prop.table(Freq), 3),
          RelAc = round(cumsum(prop.table(Freq)), 3))

# Agrupa los valores de la variable Edad y calcula la frecuencia abasoluta (POR CLASES | INTERVALOS)
tablaEdadInterv <- as.data.frame(table(Edad = factor(cut(covid_survey$Edad, 3))))

# Muestra la tabla de frecuencias completa
tablaEdadInterv <- transform(tablaEdadInterv,
                             #FreqAc = cumsum(Freq),
                             #Rel = round(prop.table(Freq), 3),
                             #RelAc = round(cumsum(prop.table(Freq)), 3),
                             Porcentaje = round(prop.table(Freq), 3) * 100
                             #RelPercent =  round(cumsum(round(prop.table(Freq), 3) * 100),3)
                             )


# Grafico rango de Edades
bp <- barplot(height = tablaEdadInterv$RelPercent, 
              names = tablaEdadInterv$Edad,
              col=c("green","orange", "blue"),
              main="Distribución demografica por edad",
              ylab = "Frecuencias relativas",
              xlab = "Rango Edades")

text(bp, 0, round(tablaEdadInterv$RelPercent, 1),cex=0.7,pos=3) 

# Calcular totales frecuencias Edad
totalFreq = with(tablaEdadInterv, sum(Freq))
totalPorcentaje = with(tablaEdadInterv, sum(Porcentaje))

tablaEdadInterv %>% add_row(Edad = "Total",
                                   Freq = totalFreq, 
                                   Porcentaje = totalPorcentaje)

#-------------------------------------------------------------------------------------------------------------------
#                                             Calificación Clase Online
#-------------------------------------------------------------------------------------------------------------------

# Agrupa los valores de la variable CalificasionClaseOnline 
tablaCalificasionClaseOnline <- as.data.frame(table(CalificacionClaseOnline = covid_survey$CalificacionClaseOnline))

# Muestra la tabla de frecuencias completa
tablaCalificasionClaseOnlineFreq <- transform(tablaCalificasionClaseOnline,
                                    #FreqAc = cumsum(Freq),
                                    #Rel = round(prop.table(Freq), 3),
                                    #RelAc = round(cumsum(prop.table(Freq)), 3),
                                    Porcentaje = round(prop.table(Freq), 3) * 100,
                                    PorcentajeAcum = round(cumsum(round(prop.table(Freq), 3) * 100),3) 
)

totalFreq = with(tablaCalificasionClaseOnlineFreq, sum(Freq))
totalPorcentaje = with(tablaCalificasionClaseOnlineFreq, sum(Porcentaje))
totalPorcentajeAcum = with(tablaCalificasionClaseOnlineFreq, sum(PorcentajeAcum))

tablaCalificasionClaseOnlineFreq %>% add_row(CalificacionClaseOnline = "Total",
                                             Freq = totalFreq, 
                                             Porcentaje = totalPorcentaje, 
                                             PorcentajeAcum = totalPorcentajeAcum)

#-------------------------------------------------------------------------------------------------------------------
#                                             Calificación Region Residencia
#-------------------------------------------------------------------------------------------------------------------
# Agrupa los valores de la variable CalificasionClaseOnline 
tablaRegionResidencia <- as.data.frame(table(RegionResidencia = covid_survey$RegionResidencia))

# Muestra la tabla de frecuencias completa
tablaRegionResidenciaFreq <- transform(tablaRegionResidencia,
                                       #FreqAc = cumsum(Freq),
                                       #Rel = round(prop.table(Freq), 3),
                                       #RelAc = round(cumsum(prop.table(Freq)), 3),
                                       Porcentaje = round(prop.table(Freq), 3) * 100,
                                       PorcentajeAcum = round(cumsum(round(prop.table(Freq), 3) * 100),3) 
)

totalFreq = with(tablaRegionResidenciaFreq, sum(Freq))
totalPorcentaje = with(tablaRegionResidenciaFreq, sum(Porcentaje))
totalPorcentajeAcum = with(tablaRegionResidenciaFreq, sum(PorcentajeAcum))

# Grafico porcentajes
colors <- c("yellow2","orange")
pie(tablaRegionResidenciaFreq$Porcentaje, labels = paste(tablaRegionResidenciaFreq$Porcentaje, "%"), col=colors)
legend(.9, .1, 
       legend = c("Delhi-NCR", "Outside Delhi-NCR"),
       fill =  colors)

# Agregar totales
tablaRegionResidenciaFreq %>% add_row(RegionResidencia = "Total",
                                       Freq = totalFreq, 
                                       Porcentaje = totalPorcentaje, 
                                       PorcentajeAcum = totalPorcentajeAcum)

#-------------------------------------------------------------------------------------------------------------------
#                                          Calificación Region Residencia vs Edad
#-------------------------------------------------------------------------------------------------------------------

# Agrupa los valores de la variable RegionResidencia 
tablaRegionResidenciaOutside <- subset(covid_survey, RegionResidencia == "Outside Delhi-NCR")
tablaRegionResidenciaDelhi <- subset(covid_survey, RegionResidencia == "Delhi-NCR")

with(tablaRegionResidenciaOutside, mean(Edad))
with(tablaRegionResidenciaOutside, sd(Edad))
with(tablaRegionResidenciaOutside, length(Edad))

with(tablaRegionResidenciaDelhi, mean(Edad))
with(tablaRegionResidenciaDelhi, sd(Edad))
with(tablaRegionResidenciaDelhi, length(Edad))

with(covid_survey, tapply(Edad, RegionResidencia, mean))
with(covid_survey, tapply(Edad, RegionResidencia, sd))
with(covid_survey, tapply(Edad, RegionResidencia, length))

summary(covid_survey)

#-------------------------------------------------------------------------------------------------------------------
#                                      Analisis Edad (TiempoClaseOnline, TiempoAutoaprendizaje)
#-------------------------------------------------------------------------------------------------------------------

#TiempoAutoaprendizaje

# Agrupa los valores de la variable Edad y calcula la frecuencia abasoluta (POR CLASES | INTERVALOS)
tablaTiempoAutoaprendizajeInterv <- as.data.frame(table(Tiempo_Auto_Aprendizaje = factor(cut(covid_survey$TiempoAutoaprendizaje, 3))))

# Muestra la tabla de frecuencias completa
tablaTiempoAutoaprendizajeInterv <- transform(tablaTiempoAutoaprendizajeInterv,
                                              Porcentaje = round(prop.table(Freq), 3) * 100
)
tablaTiempoAutoaprendizajeInterv

# Grafico rango de Edades
bp <- barplot(height = tablaEdadInterv$RelPercent, 
              names = tablaEdadInterv$TiempoAutoaprendizaje,
              col=c("green","orange", "blue"),
              main="Distribución demografica por edad",
              ylab = "Frecuencias relativas",
              xlab = "Rango Edades")

text(bp, 0, round(tablaTiempoAutoaprendizajeInterv$Porcentaje, 1),cex=0.7,pos=3) 

# Calcular totales frecuencias Edad
totalFreq = with(tablaTiempoAutoaprendizajeInterv, sum(Freq))
totalPorcentaje = with(tablaTiempoAutoaprendizajeInterv, sum(Porcentaje))

tablaEdadInterv %>% add_row(TiempoAutoaprendizaje = "Total",
                            Freq = totalFreq, 
                            Porcentaje = totalPorcentaje)

#-------------------------------------------------TiempoClaseOnline------------------------------------------
# Agrupa los valores de la variable Edad y calcula la frecuencia abasoluta (POR CLASES | INTERVALOS)
tablaInterv <- as.data.frame(table(TiempoRedesSociales = factor(cut(covid_survey$TiempoRedesSociales, 3))))

# Muestra la tabla de frecuencias completa
tablaInterv <- transform(tablaInterv,
                         Porcentaje = round(prop.table(Freq), 3) * 100
)

tablaInterv

resultAgg <- setNames(tablaInterv, c("Tiempo Redes Sociales", "Freq", "Porcentaje"))
resultAgg


#--------------------------------------------Grupos edades------------------------------------------
# tablaEdadJoven <-  subset(covid_survey, covid_survey$Edad <= 24.3, select=c(Edad, TiempoClaseOnline))
# tablaEdadJoven


covid_survey <- covid_survey %>% 
  mutate(RangoEdades = case_when(
                                .$Edad >= 6.95 & .$Edad  <= 24.3 ~ "(6.95 - 24.3]",
                                .$Edad >  24.3 & .$Edad  <= 41.7 ~ "(24.3 - 41.7]",
                                .$Edad >  41.7 & .$Edad  <= 59.1 ~ "(41.7 - 59.1]",
                                TRUE ~ "NA"
                               )
        )

covid_survey


roundMean = function(x) {
               round(mean(x), digits = 2)
            }

tablaRangosEdadClasesOnline <- subset(covid_survey, select=c(TiempoRedesSociales, RangoEdades))

tablaRangosEdadClasesOnline

resultAgg <- setNames(aggregate(tablaRangosEdadClasesOnline$TiempoRedesSociales, 
                                by = list(tablaRangosEdadClasesOnline$RangoEdades), 
                                FUN = roundMean), c("Edad", "Tiempo medio (Horas/dia)"))
resultAgg

#-------------------------------------------------------------------------------------------------------------------
#                                             Edad vs Calificacion clases Online
#-------------------------------------------------------------------------------------------------------------------
# sum(complete.cases(covid_survey))
# sum(!complete.cases(covid_survey))

distinct(covid_survey$CalificacionClaseOnline)

select(covid_survey$RangoEdades, covid_survey$TiempoClaseOnline)

tablaCalificaciones <- subset(covid_survey, select=c(RangoEdades, CalificacionClaseOnline))
tablaCalificaciones


sqldf(
  "
Select CalificacionClaseOnline, RangoEdades, COUNT(*) AS Cantidad
from covid_survey 
group by CalificacionClaseOnline, RangoEdades
having count(*) > 1
order by 1"
)

dftMP <- sqldf(
"
SELECT *
FROM(
Select CalificacionClaseOnline, COUNT(*) AS Cantidad
from covid_survey 
group by CalificacionClaseOnline
having count(*) > 1) as dt
order by 1"
)

dfResult <- transform(dftMP, Percent = round((Cantidad / 1182),3) * 100) 
dfResult

summary(dfResult)

tableR = table(CalificacionClaseOnline = covid_survey$CalificacionClaseOnline, 
               RangoEdades = covid_survey$RangoEdades)
tableR2 = prop.table(tableR, 2)

# Grafico rango de Edades
# bp <- barplot(tableR2, 
#               xlab = "Calificacion clases Online", 
#               col=1:5,
#               legend = rownames(tableR2))


#Grafico de barras y proporciones:
myDataLong <- melt(tableR2, CalificacionClaseOnline = c("rating"), value.name = "proporcion")
myDataLong

names(myDataLong)[1] <- paste("rating")
names(myDataLong)[2] <- paste("edades")
myDataLong

ggplot() + geom_bar(aes(y    = proporcion,
                        x    = edades,
                        fill = rating),
                    data = myDataLong,
                    stat = "identity") + ggtitle("Calificaciones por grupo de Edad") +  ggeasy::easy_center_title()




tblJovenes <- subset(myDataLong, RangoEdades == "(24.3 - 41.7]")
summary(tblJovenes)

# grafico torta

colors <- c("grey", "yellow2","orange", "#56B4E9", "red", "#4cba38")
pie(dfResult$Percent, 
    labels = paste(dfResult$Percent, "%"),
    main = "Calificaciones Clase Online",
    col=colors)
legend(1.2, .3, 
       legend = c("<NA>", "Average", "Excellent", "Good", "Poor", "Very poor"),
       fill =  colors)

#-------------------------------------------------------------------------------------------------------------------
#                                      Analisis estres
#-------------------------------------------------------------------------------------------------------------------


tablaEstres <- subset(covid_survey, select=c(RangoEdades, EliminaEstres))
tablaEstres

dftMP <- sqldf(
  "
SELECT * 
FROM(
Select EliminaEstres, COUNT(*) AS Cantidad
from covid_survey 
group by EliminaEstres
having count(*) > 1) as dt
order by 1"
)

dfResult <- transform(dftMP, Percent = round((Cantidad / 1182),3) * 100) 
dfResult <- subset(dfResult, select=c(Percent, EliminaEstres))
dfResult

dfResult = dfResult[with(dfResult, order(-Percent)), ]
dfResult
tab0a <- table(dfResult$Percent)


midpts <- barplot(height = dfResult$Percent,
                  names = dfResult$EliminaEstres,
                  main = "Supresor de estres",
                  xlab = "Porcentaje",
                  horiz = TRUE,  
                  las = 1, 
                  col = "yellow",
                  cex.names = 0.6)


#-------------------------------------------------------------------------------------------------------------------
#                                      Analisis Familia
#-------------------------------------------------------------------------------------------------------------------
tableR2 = table(TiempoUtilizado = covid_survey$TiempoUtilizado, 
               RangoEdades = covid_survey$RangoEdades)
tableR2

tableR3 = prop.table(tableR2, 2)

#Grafico de barras y proporciones:
myDataLong2 <- melt(tableR3, TiempoUtilizado = c("rating"), value.name = "proporcion")
myDataLong2

#45b3b7 azul
#f15e5a rojo

names(myDataLong2)[1] <- paste("rating")
names(myDataLong2)[2] <- paste("edades")
myDataLong2

ggplot() + geom_bar(aes(y    = proporcion,
                        x    = edades,
                        fill = rating),
                    data = myDataLong2,
                    stat = "identity") + ggtitle("Tiempo productivo por Edad") +  ggeasy::easy_center_title()

#---
dftMP <- sqldf(
  "
SELECT * 
FROM(
Select ProblemaSalud, COUNT(*) AS Cantidad
from covid_survey 
group by ProblemaSalud
having count(*) > 1) as dt
order by 1"
)

dfResult <- transform(dftMP, Percent = round((Cantidad / 1182),3) * 100) 
dfResult <- subset(dfResult, select=c(Percent, ProblemaSalud))
dfResult

# Grafico porcentajes
colors <- c("#f15e5a", "#45b3b7")
pie(dfResult$Percent, main = "Problemas de salud",labels = paste(dfResult$Percent, "%"), col=colors)
legend(1.3, .1, 
       legend = c("No", "Si"),
       fill =  colors
       )


#-------------------------------------------------------------------------------------------------------------------
#                                      Analisis Extraña
#-------------------------------------------------------------------------------------------------------------------
dftMP <- sqldf(
  "
SELECT * 
FROM(
Select QueExtrana, COUNT(*) AS Cantidad
from covid_survey 
group by QueExtrana
having count(*) > 1) as dt
order by 1"
)

dfResult <- transform(dftMP, Percent = round((Cantidad / 1182),3) * 100) 
dfResult <- subset(dfResult, select=c(Percent, QueExtrana))
dfResult

dfResult = dfResult[with(dfResult, order(-Percent)), ]
dfResult
tab0a <- table(dfResult$Percent)


midpts <- barplot(height = dfResult$Percent,
                  names = dfResult$QueExtrana,
                  main = "¿Que extraña?",
                  xlab = "Porcentaje",
                  horiz = TRUE,  
                  las = 1, 
                  col = "#45b3b7",
                  cex.names = 0.4)

#-------------------------------------------------------------------------------------------------------------------
#                                      Analisis Correlacion datos
#-------------------------------------------------------------------------------------------------------------------

covarianza <- 0.0
correlacion <- 0.0
columna <-0







