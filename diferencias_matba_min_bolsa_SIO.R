rm(list=ls())
library(tidyverse)
library("writexl")

###### Bolsa, Ministerio, MAtba  y SIOGRANOS #### diferencias precios disponible

## FOB Bolsa y min
precios = readxl::read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/Sistema FOB/Analisis Octubre 2023/Inputs/Disponible.xlsx", sheet = "bolsa_min")
precios$Fecha = as.Date(precios$Fecha, "%d-%m-%Y")

## MATBA
matba = readxl::read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/Sistema FOB/Analisis Octubre 2023/Inputs/Disponible.xlsx", sheet = "matba")
matba$Fecha = as.Date(matba$Fecha, "%d/%m/%Y")

todo =left_join(matba,precios)

sio = readxl::read_excel("C:/Users/evasquez/Downloads/comercializacion.xlsx", sheet = "Hoja1")
sio$Fecha = as.Date(sio$Fecha, "%d-%m-%Y")

todo1 =left_join(todo,sio)

#write_xlsx(todo1,"C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/Sistema FOB/Analisis Octubre 2023/Resultados/Precios.xlsx")


###### Bolsa, Ministerio, MAtba #### diferencias precios futuros

precios = readxl::read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/Sistema FOB/Analisis Octubre 2023/Resultados/Diferencias_ministerio_bolsa2.2.xlsx") %>% 
  filter(Cereal == "Soja") %>% select(Fecha, Cereal, Mes, Año, FOB_Bolsa, FOB_Minagro, Embarque)
precios$Fecha = as.Date(precios$Fecha, "%d-%m-%Y")

## MATBA
matba = readxl::read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/Sistema FOB/Analisis Octubre 2023/Inputs/posiciones_MATBA.xlsx", sheet = "MATBA Soja")
matba$Fecha = as.Date(matba$Fecha, "%d/%m/%Y")


matba_min_bolsa = left_join(precios,matba) %>% drop_na(MATBA)

#write_xlsx(matba_min_bolsa,"C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/Sistema FOB/Analisis Octubre 2023/Resultados/matba_min_bolsa_soja.xlsx")
