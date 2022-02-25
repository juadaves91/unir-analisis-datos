#Pre-requisitos 
#Antes de empezar a trabajar con los datos debemos fijar el directorio de trabajo.   
setwd("/Users/juandavidescobarescobar/Documents/Unir/Materias/Analisis e interpretacion de Datos/Actividad 3/")

#1. Carga de librerias

#Instalacion de librerias
if  (!requireNamespace("readr", quietly = TRUE)) {install.packages("readr")}
if  (!requireNamespace("ggpubr", quietly = TRUE)) {install.packages("ggpubr")}
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
if  (!requireNamespace("nortest", quietly = TRUE)) {install.packages("nortest")}
if  (!requireNamespace("car", quietly = TRUE)) {install.packages("car")}
if  (!requireNamespace("feather", quietly = TRUE)) {install.packages("feather")}
if  (!requireNamespace("astsa", quietly = TRUE)) {install.packages("astsa")}

#Importar de librerias
library(readr)
library(dplyr)
library(RSQLite)
library(gsubfn)
library(proto)
library(sqldf)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(ggpubr)
library(nortest)
library(car)
library(feather)
library(reshape2)
library(tseries)
library(xts)
library(astsa)
#-------------------------------------------------------------------------------------------------------------------
#                                         EDA - CONTRASTE DE HIPOTESIS (2 POBLACIONES)
#-------------------------------------------------------------------------------------------------------------------

#2. Carga de datos

directory_files = '/Users/juandavidescobarescobar/Documents/Unir/Materias/Analisis e interpretacion de Datos/Actividad 3/Datos 1 poblacion/Positivos/'
file_name = 'Casos_positivos_de_COVID-19_en_Colombia.csv' 
path = paste(directory_files,  file_name, sep="")

df_positivos_COL <- read_delim(path, delim=";", col_names = TRUE, locale = locale(encoding = "UTF-8"))

View(df_positivos_COL) # 520.620

#2. Muestra aleatoria de 500 datos


file_name_aleatory = 'Casos_positivos_de_COVID-19_en_Colombia_ALEATORIO.csv' 
path_aleatory = paste(directory_files,  file_name_aleatory, sep="")


if (file.exists(path_aleatory)) {
  
  df_positivos_COL_ALEATORIA <- read_delim(path_aleatory,
                                         delim=",", col_names = TRUE, locale = locale(encoding = "UTF-8"))
  print('File exist before')
} else {
  
  df_positivos_COL_ALEATORIA <- sample_n(df_positivos_COL, size= 35)
  write.csv(df_positivos_COL_ALEATORIA, file = path_aleatory)
  
  print('File exist NOT before')
}

View(df_positivos_COL_ALEATORIA)

#3. Totalizar o sumarizar vacunas de muestra aleatoria

df_positivos_COL_ALEATORIA_Acum <- sqldf("SELECT Sexo,
                                               COUNT(*) AS Cantidad
                                       FROM df_positivos_COL_ALEATORIA 
                                       GROUP BY  Sexo  
                                       ORDER BY  Cantidad")

# 1 M 18
# 2 F 17

View(df_positivos_COL_ALEATORIA_Acum)

min(df_positivos_COL$fecha_reporte_web)
max(df_positivos_COL$fecha_reporte_web)


#4. Estadistica descriptiva


edad_media <- mean(df_positivos_COL$Edad)
edad_media


df_positivos_COL_Acum <- sqldf("SELECT Sexo,
                                        COUNT(*) AS Cantidad
                                       FROM df_positivos_COL 
                                       GROUP BY  Sexo  
View(df_positivos_COL_Acum)


percnt_h = 237541 / 520620
percnt_f = 283079 / 520620

print(percnt_h)
print(percnt_f)

df_positivos_COL_Acum_2 <- sqldf("SELECT Nombre_grupo_etnico,
                                        COUNT(*) AS Cantidad
                                       FROM df_positivos_COL 
                                       GROUP BY  Nombre_grupo_etnico  
                                       ORDER BY  Cantidad desc
                                 ")
View(df_positivos_COL_Acum_2)


# Agrupa los valores de la variable Edad y calcula la frecuencia abasoluta
tablaEdad <- as.data.frame(table(Edad = df_positivos_COL$Edad))

# El valor con mayor frecuencia absoluta es la Moda = 20
tablaEdad

# Medidas de tendencia central
summary(df_positivos_COL$Edad)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   27.00   36.00   39.16   51.00  111.00 

# Muestra la tabla de frecuencias completa
# transform(tablaEdad,
#           FreqAc = cumsum(Freq),
#           Rel = round(prop.table(Freq), 3),
#           RelAc = round(cumsum(prop.table(Freq)), 3))
# 
# # Agrupa los valores de la variable Edad y calcula la frecuencia abasoluta (POR CLASES | INTERVALOS)
# tablaEdadInterv <- as.data.frame(table(Edad = factor(cut(df_positivos_COL$Edad, 3))))
# 
# # Muestra la tabla de frecuencias completa
# tablaEdadInterv <- transform(tablaEdadInterv,
#                              FreqAc = cumsum(Freq),
#                              Rel = round(prop.table(Freq), 3),
#                              RelAc = round(cumsum(prop.table(Freq)), 3),
#                              #Porcentaje = round(prop.table(Freq), 3) * 100
#                              RelPercent =  round(cumsum(round(prop.table(Freq), 3) * 100),3)
# )
# 
# 
# Rel_vector <- as.vector(tablaEdadInterv['RelPercent'])
# Edad_vector <- as.vector(tablaEdadInterv['Edad'])

# Grafico rango de Edades
# bp <- barplot(height = tablaEdadInterv$RelPercent, 
#               names = tablaEdadInterv$Edad,
#               col=c("#CCCCCC", "#9DACBB", "#6E8DAB", "#3F6D9B", "#104E8B"),
#               main="Distribución demografica por edad",
#               ylab = "Frecuencias relativas",
#               xlab = "Rango Edades")
# text(bp, 0, round(tablaEdadInterv$RelPercent, 1),cex=0.7,pos=3) 
# 
# # Calcular totales frecuencias Edad
# totalFreq = with(tablaEdadInterv, sum(Freq))
# totalPorcentaje = with(tablaEdadInterv, sum(Porcentaje))
# 
# tablaEdadInterv %>% add_row(Edad = "Total",
#                             Freq = totalFreq, 
#                             Porcentaje = totalPorcentaje)
# 
# View(tablaEdadInterv)

# Edad   Freq
# 1 (0.89,37.7] 272416
# 2 (37.7,74.3] 227529
# 3  (74.3,111]  20675


percent_part_1 = 272416 / 520620 
percent_part_2 = 227529 / 520620 
percent_part_3 = 20675 / 520620 

print(percent_part_1)
print(percent_part_2)
print(percent_part_3)


# grafico categoria sexo


# Agrupa los valores de la variable CalificasionClaseOnline 
# tablaSexo <- as.data.frame(table(Sexo = tablaEdadInterv$Sexo))
# 
# # Muestra la tabla de frecuencias completa
# tablaSexoFreq <- transform(tablaSexo,
#                              #FreqAc = cumsum(Freq),
#                              #Rel = round(prop.table(Freq), 3),
#                              #RelAc = round(cumsum(prop.table(Freq)), 3),
#                              Porcentaje = round(prop.table(Freq), 3) * 100,
#                              PorcentajeAcum = round(cumsum(round(prop.table(Freq), 3) * 100),3) 
# )
# 
# tablaSexoFreq
# 
# colors <- c("#CCCCCC", "#9DACBB", "#6E8DAB", "#3F6D9B", "#104E8B")
# pie(tablaSexoFreq$Porcentaje, labels = paste(tablaSexoFreq$Porcentaje, "%"), col=colors)
# legend(.9, .1, 
#        legend = c("H", "M"),
#        fill =  colors)

# Grafico Serie de tiempo, numero de contagios H y M en 2021 (Medellin)

df_serie_tiempo <- sqldf("SELECT fecha_reporte_web,
                                 Sexo,
                                 COUNT(*) AS Cantidad 
                         FROM df_positivos_COL 
                         GROUP BY  fecha_reporte_web, Sexo")

View(df_serie_tiempo)


# pivot M, F
df_serie_tiempo_pv <- dcast(df_serie_tiempo, fecha_reporte_web ~ Sexo, value.var="Cantidad", fun.aggregate=sum)

df_serie_tiempo_pv$fecha_reporte_web <-as.Date(df_serie_tiempo_pv$fecha_reporte_web, "%d/%m/%y")
class(df_serie_tiempo_pv$fecha_reporte_web)
class(df_serie_tiempo_pv)


df_serie_tiempo_pv <- df_serie_tiempo_pv %>%
                      mutate(Mes = month(df_serie_tiempo_pv$fecha_reporte_web), Anio = year(df_serie_tiempo_pv$fecha_reporte_web)  )

df_serie_tiempo_pv <- df_serie_tiempo_pv %>%
  mutate(Fecha = paste("01", "/", df_serie_tiempo_pv$Mes, "/", df_serie_tiempo_pv$Anio, sep = "")) 

df_serie_tiempo_pv$Fecha <-as.Date(df_serie_tiempo_pv$Fecha, "%d/%m/%y")
class(df_serie_tiempo_pv$Fecha)


df_serie_tiempo_pv <- sqldf("SELECT Fecha,
                                 M,
                                 F
                         FROM df_serie_tiempo_pv
                         GROUP BY Fecha
                            ")
View(df_serie_tiempo_pv)
min(Fecha)
max(Fecha)
# 
# df_hombres <- ts(df_serie_tiempo_pv, start = c(2020, 1), FRECUENCY = 12)
# View(df_hombres)

df_hombres <- sqldf("SELECT Fecha,
                            M
                         FROM df_serie_tiempo_pv ")

max(df_hombres$M) #743

df_hombres <- ts(df_hombres$M, start = 2021, frequency=12)


graphics.off()
plot(df_hombres, col='blue', main='Serie Hombres - Casos Covid-19', xlab='Hombres', ylab='Casos')


df_mujeres <- sqldf("SELECT Fecha,
                            F
                         FROM df_serie_tiempo_pv ")
max(df_mujeres$F) # 828

df_mujeres <- ts(df_mujeres$F, start = 2021, frequency=12)

graphics.off()
plot(df_mujeres, col='red', main='Serie Mujeres - Casos Covid-19', xlab='Mujeres', ylab='Casos')
  


#5. Contraste de hipotesis bilateral para la proporcion

View(df_positivos_COL_ALEATORIA)
plot(df_positivos_COL_ALEATORIA$Cantidad)

pbar = 18 / 35          # proporcion de la muestra
n = 35                 # tamaño de la muestra
p0 = 0.5                 # hipotesis nula
z = (pbar - p0) / sqrt(p0 * (1 - p0) / n)
z                       # estadistico de contraste

alpha = 0.05
z.half.alpha = qnorm((1 - alpha) / 2)
c(-z.half.alpha, z.half.alpha)


pval = 2 * pnorm(z, lower.tail=FALSE)
pval


# z = 0.1690309; no se encuentra en el IC (0.06270678 -0.06270678), por lo tanto existe base estadistica para rechazar H0
# p-valor = 0.8657724; p-valor > 0.05, por lo tanto existe base estadistica para rechazar H0

#-------------------------------------------------------------------------------------------------------------------
#                                         UTILIDAD PARA BORRAR CACHE DE R
#-------------------------------------------------------------------------------------------------------------------
# Borrar chache de R
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.