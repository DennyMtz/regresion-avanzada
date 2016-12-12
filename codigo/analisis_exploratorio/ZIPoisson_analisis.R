#-Working directory-

wdir<-""
wdir<-"/home/denny/github/regresion-avanzada/codigo/analisis_exploratorio"
setwd(wdir)
library(ggplot2)
library(dplyr)
library(readr)
tabla_4anios_ok <- read.csv("../../datos/tabla_4anios_naive.csv")

#Transformamos las variables explicativas en %
########### El ifelse checa si el denominador es 0 y en ese caso pone 0 a la proporción
tabla_4anios_ok$POR_P_18YMAS <- ifelse(tabla_4anios_ok$POBTOT==0,0,tabla_4anios_ok$P_18YMAS/tabla_4anios_ok$POBTOT)
tabla_4anios_ok$POR_P18YM_PB <- ifelse(tabla_4anios_ok$POBTOT==0,0,tabla_4anios_ok$P18YM_PB/tabla_4anios_ok$POBTOT)
tabla_4anios_ok$POR_VPH_PISODT <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_PISODT/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_TV <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_TV/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_AUTOM <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_AUTOM/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_PC <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_PC/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_INTER <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_INTER/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$GRAPROES_1 <- tabla_4anios_ok$GRAPROES




p <- qplot(POR_VPH_AUTOM, homi_count, data=tabla_4anios_ok, main="Relación Homicidios y Viviendas con automóvil",
           xlab="Porcentaje de viviendas con automóvil", ylab="Homicidios")

p

p2 <- qplot(PROM_OCUP, homi_count, data=tabla_4anios_ok, main="Relación Homicidios y Cuartos por vivienda",
           xlab="Promedio de cuartos por vivienda", ylab="Homicidios")

p2


p3 <- qplot(POR_VPH_AUTOM, PROM_OCUP, data=tabla_4anios_ok, main="Relación Autómoviles y Cuartos por vivienda",
            xlab="Porcentaje de viviendas con automóvil", ylab="Promedio de cuartos por vivienda")

p3


tabla <- tabla_4anios_ok
variables <- c("homi_count", "PROM_OCUP", "POR_P_18YMAS","POR_P18YM_PB","POR_VPH_PISODT", "POR_VPH_TV", "POR_VPH_AUTOM", "POR_VPH_PC", "POR_VPH_INTER","GRAPROES_1")
datos <- tabla %>% select(one_of(variables))
summary(datos)


install.packages("Hmisc")
library(Hmisc)

describe(datos)

