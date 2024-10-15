rm(list=ls())
library(tidyverse)
library(dplyr)
library("writexl")

## Exportaciones ##
tn = 1000
expo18 = read.csv("C:/Users/evasquez/Downloads/exports_2018_M/exponm18.csv", header = TRUE, sep = ";", dec = ".")
expo19 = read.csv("C:/Users/evasquez/Downloads/exports_2019_M/exponm19.csv", header = TRUE, sep = ";", dec = ".")
expo20 = read.csv("C:/Users/evasquez/Downloads/exports_2020_M/exponm20.csv", header = TRUE, sep = ";", dec = ".")
expo21 = read.csv("C:/Users/evasquez/Downloads/exports_2021_M/exponm21.csv", header = TRUE, sep = ";", dec = ".")
expo22 = read.csv("C:/Users/evasquez/Downloads/exports_2022_M/exponm22.csv", header = TRUE, sep = ";", dec = ".")


expo = rbind(expo18,expo19,expo20,expo21,expo22)

expo = expo %>% mutate(Cereal = case_when(NCM == 10011100 ~ "Trigo" , NCM == 10011900 ~ "Trigo", NCM == 10019100 ~ "Trigo",NCM == 10019900 ~ "Trigo", 
                                           NCM == 12011000 ~ "Soja",  NCM == 12019000 ~ "Soja",
                                           NCM == 10051000 ~ "Maiz",  NCM == 10059010 ~ "Maiz", NCM == 10059090 ~ "Maiz",
                                           NCM == 10039010 ~ "Cebada Cervecera",
                                           NCM == 12060010 ~ "Girasol",  NCM == 12060090 ~ "Girasol",
                                           NCM == 10039080 ~ "Cebada Forrajera",
                                           NCM == 10031000 ~ "Cebada Otras",  NCM == 10039090 ~ "Cebada Otras",
                                           NCM == 11081200 ~ "Almidon de Maiz",
                                           NCM == 11010010 ~ "Harina de Trigo",
                                           NCM == 23063010 ~ "Harina de Girasol",
                                           NCM == 23040010 ~ "Harina de Soja", NCM == 23040090 ~ "Harina de Soja",
                                           NCM == 10071000 ~ "Sorgo", NCM == 10079000 ~ "Sorgo",
                                           NCM == 38260000 ~ "Biodiesel",
                                           NCM == 15121110 ~ "Aceite de Girasol",  NCM == 15121911 ~ "Aceite de Girasol", NCM == 15121919 ~ "Aceite de Girasol",
                                           NCM == 15071000 ~ "Aceite de Soja",  NCM == 15079011 ~ "Aceite de Soja", NCM == 15079019 ~ "Aceite de Soja", NCM == 15079090 ~ "Aceite de Soja")) %>%
  drop_na(Cereal) 
  


#sustituyo las s por 0
expo$Pnet.kg.[expo$Pnet.kg. == "s               "] <- 0
expo$Pnet.kg.[expo$Pnet.kg. == "               s"] <- 0


#reemplazo las coma por puntos  y cambio el formato de la columna 
expo$Pnet.kg. = gsub(",", ".",expo$Pnet.kg.) %>%  as.numeric(expo$Pnet.kg.) 

#convierto de toneladas a miles de ton
expo1 = expo %>% mutate(Tn = Pnet.kg./tn) %>% select(Año, Mes, Cereal,Pdes, Tn) %>% filter(!Tn == 0) 

#destinos paises 
destinos = readxl::read_excel("C:/Users/evasquez/Bolsa de Cereales de Buenos Aires/Instituto Estudios Económicos - Documentos/Balances/Expo-Impo Data/pais_confidencial - copia.xls")
#convierto la columna de los codigo de paises
destinos$Pdes = as.integer(destinos$Pdes)


expo_destinos = left_join(expo1, destinos)

expo_destinos$Año =  as.Date(as.character(expo_destinos$Año ),format = "%Y")

expo_destinos$Año = format(as.Date(expo_destinos$Año),"%Y")

expo_destinos$Atributo <- "Exportaciones"



## anualizado 
expo_anuales = expo_destinos %>% group_by(Año, Cereal, Pais, Pdes) %>% 
  summarise(exportado = sum(Tn))


write_xlsx(list("Sheet1" = expo_anuales, "mensual" = expo_destinos), "expo_anuales.xlsx")


