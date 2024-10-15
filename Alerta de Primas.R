
rm(list=ls())
setwd("U:/Precios FOB/Control")
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)


primas = readxl::read_excel("C:/Users/evasquez/Downloads/datos_pos.xlsx")
primas$Producto = gsub("Trigo 11.5%", "Trigo",primas$Producto)


primas$Fecha = as.Date(primas$Fecha, "%d/%m/%Y")

#Coeficiente de conversion#
Producto<-c("Soja", "Maiz", "Harina de Soja", "Aceite de Soja", "Aceite de Girasol", "Trigo 11.5%")
coeficiente<-c(0.367454, 0.39368, 1.1023, 0.22046, 1, 1 )
coeficientes<-data.frame(Producto, coeficiente, stringsAsFactors=FALSE)


#Convierto las primas en $/ton
primas_ajustadas = merge(primas, coeficientes, by = c("Producto"), all = TRUE) %>%
  mutate(prima_compradora = Comprador * coeficiente,
         prima_vendedora = Vendedor * coeficiente) 



#Hallo el promedio y el desvio por posicion y producto
resumen = primas_ajustadas  %>% group_by(Fecha, Producto, Mes, Año)%>%  
  dplyr::summarise(media_comprador = mean(prima_compradora, na.rm=T),
                   media_vendedor = mean(prima_vendedora, na.rm=T),
                   st_comprador= sd(prima_compradora, na.rm=T),
                   st_vendedor = sd(prima_vendedora, na.rm=T)) 


primas = left_join(primas_ajustadas, resumen)%>% filter(!Producto %in% c("Aceite de Girasol", "Trigo", "Trigo 11.5%" )) %>% 
  mutate_if(is.numeric, round, 2)


p_comprador = primas %>% select(Fecha, Producto, Empresa, Mes, Año, prima_compradora, media_comprador, st_comprador)%>%
  mutate(dist_comprador = (prima_compradora-media_comprador)/st_comprador) %>% filter(dist_comprador>3)%>% 
  mutate_if(is.numeric, round, 2)


p_vendedor = primas %>% select(Fecha, Producto, Empresa, Mes, Año, prima_vendedora, media_vendedor, st_vendedor)%>%
  mutate(dist_vendedora = (prima_vendedora-media_vendedor)/st_vendedor) %>% filter(dist_vendedora>3)%>% 
  mutate_if(is.numeric, round, 2)



####Comprador####
ggplotly(ggplot(primas,aes(x=as.factor(Mes),y=prima_compradora))+
           geom_boxplot()+  theme_classic()+
           facet_wrap(~ Producto,scales = "free_y"))


####Vendedor####
ggplotly(ggplot(primas,aes(x=as.factor(Mes),y=prima_vendedora))+
           geom_boxplot()+  theme_classic()+
           facet_wrap(~ Producto,scales = "free_y"))


rm(coeficientes, primas_ajustadas, resumen)





## NOTIFICAR SI DURANTE 3 DIAS SEGUIDOS ALGUNA EMPRESA O BROKER INFORMO PRIMAS QUE SUPEREN 3 DESVIOS  ##

vendedor_notificar <- p_vendedor %>% group_by(Producto, Empresa, Mes, Año) %>%
  summarise(dias = n()) %>% filter(dias == 3) 

comprador_notificar <- p_comprador %>% group_by(Producto, Empresa, Mes, Año) %>%
  summarise(dias = n()) %>% filter(dias == 3)


