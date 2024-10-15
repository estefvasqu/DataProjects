rm(list=ls())
library(tidyverse)
library(readxl)
library(tidyr)
library("writexl")

comer = read.csv("C:/Users/evasquez/Downloads/comercializacion-main/comercializacion-main/Data/Output/Base.csv") %>% 
  filter(Grano == "Maíz" & Sector == "Total" & id <= 105) %>% select(-Total_a_Fijar, -Total_Fijado, -Total_sin_Precio,-Total_con_Precio, -Ano_Campaña, - Ano_Mes, -X) 


compras_totales = comer %>% group_by(Fecha, Numero_de_Semana,Ano,Campaña) %>% summarise(total_compras = sum(Total_Comprado)) 

b = compras_totales %>% group_by(Campaña) %>% mutate(var_compras= case_when(is.na(total_compras - lag(total_compras)) ~ total_compras, T ~ total_compras - lag(total_compras) )) %>% 
  rename(ano= Ano, semana = Numero_de_Semana)

# b[is.na(b)] = 0

c = b %>% group_by(semana,ano) %>% summarise(compras_semanales = sum(var_compras)) 

c$semanayano = paste(c$semana,c$ano,sep="-")

maiz = read_excel("C:/Users/evasquez/Desktop/Codigos.ESTEF/maiz.xlsx")
  
todo = left_join(maiz,c)


 # write_xlsx(todo,"maizfinal.xlsx")
