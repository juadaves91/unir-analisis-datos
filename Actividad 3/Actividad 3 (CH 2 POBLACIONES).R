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
if  (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")}
if  (!requireNamespace("echarts4r", quietly = TRUE)) {install.packages("echarts4r")}

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
library(tidyverse)
library(echarts4r)
#-------------------------------------------------------------------------------------------------------------------
#                                         EDA - CONTRASTE DE HIPOTESIS (2 POBLACIONES)
#-------------------------------------------------------------------------------------------------------------------

#2. Carga de datos

df_vih_MED <- 
  read_delim("/Users/juandavidescobarescobar/Documents/Unir/Materias/Analisis e interpretacion de Datos/Actividad 3/Datos 2 poblaciones/sivigila_vih.csv",
             delim=";", col_names = TRUE, locale = locale(encoding = "UTF-8"))

View(df_vih_MED) # 12,894

df_vih_BOG <- 
  read_delim("/Users/juandavidescobarescobar/Documents/Unir/Materias/Analisis e interpretacion de Datos/Actividad 3/Datos 2 poblaciones/osb_vihsida.csv",
             delim=";", col_names = TRUE, locale = locale(encoding = "UTF-8"))

df_vih_BOG <- transform(df_vih_BOG, casos_confirmados_vih = as.numeric(casos_confirmados_vih))
View(df_vih_BOG)  #308 (Acumulado por barrio)



# 4. Estadistica descriptiva 

# # Total de casos confirmados por VIH 
# 
# total_personas_med <- nrow(df_vih_MED) 
# total_personas_med # 3876
# 
# total_personas_bog <- sqldf("SELECT SUM(casos_confirmados_vih) AS casos_confirmados_vih FROM df_vih_BOG ")
# total_personas_bog # 32770
# 
# # Total de casos confirmados por VIH 
# 
# df_vih_MED <- sqldf("SELECT year,
#                     nombre_barrio,
#                     COUNT(*) AS casos_confirmados_vih,
#                     tip_cas
#              FROM df_vih_MED
#              GROUP BY  nombre_barrio, year, year
#              ORDER BY  year, COUNT(*)")
# df_vih_MED
# 
# total_personas_med <- sqldf("SELECT AVG(casos_confirmados_vih) AS casos_confirmados_vih FROM df_vih_MED ")
# total_personas_med # 4
# 
# total_personas_bog <- sqldf("SELECT AVG(casos_confirmados_vih) AS casos_confirmados_vih FROM df_vih_BOG ")
# total_personas_bog # 106
# 
# # Total de acumulado por Barrio - Medellin
# 
# df_vih_MED <- sqldf("SELECT nombre_barrio,
#                             SUM(casos_confirmados_vih) AS casos_confirmados_vih
#                    FROM df_vih_MED
#                    GROUP BY  nombre_barrio 
#                    ORDER BY  casos_confirmados_vih")
# View(df_vih_MED)
# 
# names(df_vih_MED)
# 
# df_vih_MED <- sqldf("SELECT CASE
#                                 WHEN casos_confirmados_vih <= 118 THEN 'Otros' 
#                                 ELSE nombre_barrio
#                             END AS nombre_barrio,
#                           casos_confirmados_vih
#                       FROM  df_vih_MED  
#                       order by 2
#                     ")
#       
# View(df_vih_MED)
# 
# df_vih_MED <- sqldf("SELECT nombre_barrio,
#                             SUM(casos_confirmados_vih) AS casos_confirmados_vih
#                       FROM  df_vih_MED  
#                       GROUP BY nombre_barrio
#                       order by 2 DESC
#                     ")
# View(df_vih_MED)
# 
# midpts <- barplot(height = df_vih_MED$casos_confirmados_vih,
#                   names = df_vih_MED$nombre_barrio,
#                   space = 1,
#                   main = "Casos confirmados VIH por localidad, Medellin 2008 - 2021",
#                   xlab = "Nro.casos",
#                   horiz = TRUE,  
#                   las = 1, 
#                   col = "#104E8B",
#                   cex.names = 0.4
#                   )
# 
# 
# df_vih_MED %>%
#   e_charts(nombre_barrio) %>%
#   e_pictorial(casos_confirmados_vih, symbol = icon_cruz,
#               symbolRepeat = T,
#               symbolSize = c(13,13))%>% 
#   e_theme("light") %>%
#   e_title("Casos confirmados VIH por localidad, Medellin 2008 - 2021", left='center') %>% 
#   e_color(color = 'red') %>%
#   e_flip_coords() %>%
#   e_legend(show = FALSE) %>%
#   e_x_axis(splitLine=list(show = T)) %>%
#   e_y_axis(splitLine=list(show = T)) %>%
#   e_labels(fontSize = 7, 
#            fontWeight ='bold', 
#            position = "right", 
#            offset=c(5, 0), 
#            color= "black")
# 
# 
# # Total de acumulado por Barrio - Bogota
# 
# 
# df_vih_BOG <- sqldf("SELECT localidad,
#                             SUM(casos_confirmados_vih) AS casos_confirmados_vih
#                    FROM df_vih_BOG
#                    GROUP BY  localidad 
#                    ORDER BY  casos_confirmados_vih")
# 
# df_vih_BOG <- sqldf("SELECT localidad, 
#                           casos_confirmados_vih
#                       FROM  df_vih_BOG  
#                       order by 2
#                     ")
# View(df_vih_BOG)
# 
# df_vih_BOG <- sqldf("SELECT CASE
#                                 WHEN casos_confirmados_vih <= 955 THEN 'Otros' 
#                                 ELSE localidad
#                             END AS localidad,
#                           casos_confirmados_vih
#                       FROM  df_vih_BOG  
#                       order by 2
#                     ")
# 
# View(df_vih_BOG)
# 
# 
# df_vih_BOG <- sqldf("SELECT localidad,
#                             SUM(casos_confirmados_vih) AS casos_confirmados_vih
#                       FROM  df_vih_BOG  
#                       GROUP BY localidad
#                       order by 2 DESC
#                     ")
# View(df_vih_BOG)
# 
# 
# midpts2 <- barplot(height = df_vih_BOG$casos_confirmados_vih,
#                    names = df_vih_BOG$localidad,
#                    space = 1,
#                    main = "Casos confirmados VIH por localidad, Bogota 2008 - 2021",
#                    xlab = "Nro.casos",
#                    horiz = TRUE,  
#                    las = 1, 
#                    col = "#6E8DAB",
#                    cex.names = 0.4
# )
# 
# 
# 
# x = df_vih_BOG$localidad
# y = df_vih_BOG$casos_confirmados_vih
# 
# icon_cruz <- "path://M464 96H384V48C384 21.5 362.5 0 336 0h-160C149.5 0 128 21.5 128 48V96H48C21.5 96 0 117.5 0 144v288C0 458.5 21.5 480 48 480h416c26.5 0 48-21.5 48-48v-288C512 117.5 490.5 96 464 96zM176 48h160V96h-160V48zM368 314c0 8.836-7.164 16-16 16h-54V384c0 8.836-7.164 16-15.1 16h-52c-8.835 0-16-7.164-16-16v-53.1H160c-8.836 0-16-7.164-16-16v-52c0-8.838 7.164-16 16-16h53.1V192c0-8.838 7.165-16 16-16h52c8.836 0 15.1 7.162 15.1 16v54H352c8.836 0 16 7.162 16 16V314z"
# icon <- "path://M315.1 271l-70.56-112.1C232.8 139.3 212.5 128 190.3 128H129.7c-22.22 0-42.53 11.25-54.28 30.09L4.873 271c-9.375 14.98-4.812 34.72 10.16 44.09c15 9.375 34.75 4.812 44.09-10.19l28.88-46.18L87.1 480c0 17.67 14.33 32 32 32c17.67 0 31.1-14.33 31.1-32l0-144h16V480c0 17.67 14.33 32 32 32c17.67 0 32-14.33 32-32V258.8l28.88 46.2C266.9 314.7 277.4 320 288 320c5.781 0 11.66-1.562 16.94-4.859C319.9 305.8 324.5 286 315.1 271zM160 96c26.5 0 48-21.5 48-48S186.5 0 160 0C133.5 0 112 21.5 112 48S133.5 96 160 96z"
# 
# df_vih_BOG %>%
#   e_charts(localidad) %>%
#   e_pictorial(casos_confirmados_vih, symbol = icon_cruz,
#               symbolRepeat = T,
#               symbolSize = c(13,13))%>% 
#   e_theme("light") %>%
#   e_title("Casos confirmados VIH por localidad, Bogota 2008 - 2021", left='center') %>% 
#   e_color(color = 'red') %>%
#   e_flip_coords() %>%
#   e_legend(show = FALSE) %>%
#   e_x_axis(splitLine=list(show = T)) %>%
#   e_y_axis(splitLine=list(show = T)) %>%
#   e_labels(fontSize = 8, 
#            fontWeight ='bold', 
#            position = "right", 
#            offset=c(5, 0), 
#            color= "black")






# 3 Homogenizar los datos, filtrar dataset MED por casos confirmados (tip_cas_ in (3=Confirmado por laboratorio , 4=Confirmado por clinica , 5= Confirmado por nexo epidemiológico.) )

df_vih_MED <- filter(df_vih_MED, tip_cas == 3 | tip_cas == 4 | tip_cas == 5)
unique(df_vih_MED[c("tip_cas")])

# 3.1 Homogenizar los datos dataset Medellin, agrupar sum datos por nombre_barrio
 
# df_vih_MED <- sqldf("SELECT year, 
#                     nombre_barrio, 
#                     COUNT(*) AS casos_confirmados_vih, 
#                     tip_cas
#              FROM df_vih_MED 
#              GROUP BY  nombre_barrio, year, year
#              ORDER BY  year, COUNT(*)")
# View(df_vih_MED)
#unique(df_vih_MED[c("casos_confirmados_vih")])


df_vih_MED <- sqldf("SELECT year,
                            sexo_,
                            nombre_barrio,
                            COUNT(*) AS casos_confirmados_vih
                     FROM df_vih_MED
                     GROUP BY  year, nombre_barrio, sexo_
                     ORDER BY  year")
View(df_vih_MED)
unique(df_vih_MED[c("casos_confirmados_vih")])

df_vih_MED$sexo_ <- as.factor(df_vih_MED$sexo_) 
class(df_vih_MED$sexo_)  

# Población hombres
df_vih_MED_Male <- sqldf("SELECT year,
                                 sexo_,
                                 nombre_barrio,
                                 casos_confirmados_vih
                    FROM df_vih_MED
                    WHERE sexo_ = 'M' ")
View(df_vih_MED_Male)

# Población mujeres
df_vih_MED_Female <- sqldf("SELECT year,
                                 sexo_,
                                 nombre_barrio,
                                 casos_confirmados_vih
                    FROM df_vih_MED
                    WHERE sexo_ = 'F' ")
View(df_vih_MED_Female)


# 3.2 Homogenizar los datos dataset Bogota, agrupar sum datos por nombre_barrio
di <- describe(df_vih_BOG)
di
str(di)
unique(df_vih_BOG[c("casos_confirmados_vih")])

df_vih_BOG <- transform(df_vih_BOG, casos_confirmados_vih = as.numeric(casos_confirmados_vih))


# 4. Analisis inferencial, contraste de hipotsis (Analisis diferencia significativa casos confirmados VIH en Medellin y bogota entre 2008 y 2020)


# METODO TRADICIONAL:

# Media muestral X^ (Medellin) = 
# Media muestral Y^ (Bogota) =

# Contraste de hipotesis Bilateral
# H0 = µ1 == µ2
# H1 = µ1 <> µ2


#          N.PERSONAS CONFIRMADAS VIH   
# MEDELLIN 3876
# BOGOTA   308

#          MUESTRA ALEATORIA SIMPLE
# MEDELLIN 200
# BOGOTA   150

df_vih_MED_ALEATORIA <- sample_n(df_vih_MED, size= 200)
df_vih_BOG_ALEATORIA <- sample_n(df_vih_BOG, size= 150)

x <- rnorm(df_vih_MED_ALEATORIA$casos_confirmados_vih)    
y <- rnorm(df_vih_BOG_ALEATORIA$casos_confirmados_vih)  

help(hist)

hist(x = x, 
     ylab="Nro. Personas comfirmadas", 
     xlab="N = 200", 
     main = paste("Nro. Personas comfirmadas VIH  Medellin [2008 - 2021]"),
     col = "#6E8DAB")

#col=c("#CCCCCC", "#9DACBB", "#6E8DAB", "#3F6D9B", "#104E8B")

hist(x = y, 
     ylab="Nro. Personas comfirmadas", 
     main = paste("Nro. Personas comfirmadas VIH  Bogota [2008 - 2021]"),
     xlab="N = 150",
     col = "#CCCCCC")

x_mean <- mean(x) 
y_mean <- mean(y)

n1 <- nrow(df_vih_MED) 
n2 <- nrow(df_vih_BOG)

s1 <- sd(x)
s2 <- sd(y)

s1_2 <- var(x)
s2_2 <- var(y)

Z <- (x_mean - y_mean) / sqrt( (s1_2/n1) + (s2_2/n2) )
Z_alpha_medio <- 0.05 / 2
Z_alpha_medio_val <- qnorm(1 - Z_alpha_medio)

IC = c(-Z_alpha_medio_val, Z_alpha_medio_val)

print(xval)
print(yval)
print(n1)
print(n2)
print(s1)
print(s2)
print(s1_2)
print(s2_2)
print(Z)
print(Z_alpha_medio)
print(IC)
print(Z_alpha_medio_val) # -1.96 <=  2.16742 <= 1.96 (Falso. rechazamos H0)


# METODO P-VALOR:

pval = 2 * pnorm(Z, lower.tail= FALSE) # 0.03020287
print(pval)  # 0.9479141 > 0.05 (Rechazamos H0)



#-------------------------------------------------------------------------------------------------------------------
#                                         UTILIDAD PARA BORRAR CACHE DE R
#-------------------------------------------------------------------------------------------------------------------
# Borrar chache de R
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.