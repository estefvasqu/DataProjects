rm(list=ls())
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library("writexl")

fob = readxl::read_excel("C:/Users/estef/Downloads/serie_fob.xlsx") %>% filter(!Cereal %in% c("Trigo 12%", "Trigo 10.5%" )) 
fob$Fecha = as.Date(fob$Fecha, "%d/%m/%Y")

min_max = readxl::read_excel("C:/Users/estef/Downloads/precios_min_max.xlsx") 
min_max$Fecha = as.Date(min_max$Fecha, "%d-%m-%Y")


precios =  left_join(fob, min_max)  %>% filter(!is.na(Precio))%>% 
  pivot_longer(cols = c(`Precio`, `Precio Mín`, `Precio Máx`),names_to = "Tipo",values_to = "Precios")



precios$posicion <- as.Date(paste(precios$Año, precios$Mes, "01", sep = "-"))
precios <- precios %>% arrange(posicion )




ggplotly(ggplot(precios, aes(x=posicion, y=Precios, group = Tipo, colour = Tipo)) +geom_line()  + facet_wrap( ~ Cereal, scales = "free"))


#write_xlsx(precios,"precios_publicados.xlsx")
