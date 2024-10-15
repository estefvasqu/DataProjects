rm(list = ls())

library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library("writexl")


setwd("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Analisis/Conflicto camiones/vscode")

####### CAMIONES ##############
#atajo para el pipe esc y m

df.full <- read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Analisis/Conflicto camiones/vscode/archivo/camiones.xlsx") %>% mutate(FECHA = as.Date(FECHA))%>% drop_na(Region) 


variaciones = df.full %>% group_by(ano, numero_de_semana,  Descripcion) %>% summarise(total_semanal = sum(Cantidad))

prueba = variaciones %>% group_by(Descripcion) %>% mutate(var_= (total_semanal - lag(total_semanal))/lag(total_semanal))  %>% drop_na(var_) 

junto = left_join(df.full,prueba, by = c("ano", "numero_de_semana", "Descripcion"))


write_xlsx(prueba,"archivo/variaciones.xlsx")

