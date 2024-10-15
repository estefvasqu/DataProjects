rm(list=ls())
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)


primas = readxl::read_excel("C:/Users/estef/Downloads/datos_pos.xlsx")
primas$Fecha = as.Date(primas$Fecha, "%d/%m/%Y")


cbot = readxl::read_excel("C:/Users/estef/Downloads/serie_precios_chicago.xlsx") %>% rename("Producto" = "Cereal", "Posicion"="Posición")
cbot$Fecha = as.Date(cbot$Fecha, "%d/%m/%Y")



#Coeficiente de conversion#
Producto<-c("Soja", "Maiz", "Harina de Soja", "Aceite de Soja", "Aceite de Girasol", "Trigo 11.5%")
coeficiente<-c(0.367454, 0.39368, 1.1023, 0.22046, 1, 1 )
coeficientes<-data.frame(Producto, coeficiente, stringsAsFactors=FALSE)


#Convierto las primas en $/ton
primas_ajustadas = merge(primas, coeficientes, by = c("Producto"), all = TRUE) %>%
  mutate(prima_compradora = Comprador * coeficiente,
         prima_vendedora = Vendedor * coeficiente) 




#Hallo el promedio y el desvio de primas por posicion y producto
resumen = primas_ajustadas  %>% group_by(Fecha, Producto, Mes, Año)%>%  
  dplyr::summarise(media_prima_compradora = mean(prima_compradora, na.rm=T),
                   media_prima_vendedor = mean(prima_vendedora, na.rm=T),
                   st_prima_comprador= sd(prima_compradora, na.rm=T),
                   st_prima_vendedor = sd(prima_vendedora, na.rm=T)) 


base_primas = left_join(primas_ajustadas, resumen)%>% mutate_if(is.numeric, round, 2)


#Hallo el promedio y el desvio de precios chicago por posicion y producto
precios_no_cbot = subset(primas_ajustadas, Producto %in% c("Trigo 11.5%", "Aceite de Girasol")) %>% select(-"Comprador", -"Vendedor")
precios_no_cbot$Precio <- 0
base = merge(primas_ajustadas, cbot, by=c("Fecha", "Producto", "Posicion")) %>% rename("Precio" = "Precio USD/Tn") %>% select(-"Precio Min USD/Tn",-"Precio Max USD/Tn", -"Comprador", -"Vendedor")
base_precios = rbind(base, precios_no_cbot) %>% mutate(precio_comprador = Precio + prima_compradora, precio_vendedor = Precio + prima_vendedora)%>% mutate_if(is.numeric, round, 2)


resumen_precios = base_precios %>% group_by(Fecha, Producto, Mes, Año) %>% dplyr::summarise(media_compradora = mean(precio_comprador, na.rm=T),
                                                                                            media_vendedor = mean(precio_vendedor, na.rm=T),
                                                                                            st_comprador= sd(precio_comprador, na.rm=T),
                                                                                            st_vendedor = sd(precio_vendedor, na.rm=T))
base_flat = left_join(base_precios,resumen_precios)%>% mutate_if(is.numeric, round, 2)


broker_companies <- c('ACI', 'Agro Oils', 'Agro Sud', 'JJH', 'Mc & P')
base_flat <- base_flat %>%
  mutate(Categoria = ifelse(Empresa %in% broker_companies, 'broker', 'empresa'))

base_flat$omitido_comprador = base_flat$precio_comprador < (base_flat$media_compradora - base_flat$st_comprador)
base_flat$omitido_vendedor = base_flat$precio_vendedor < (base_flat$media_vendedor - base_flat$st_vendedor)



library("writexl")
write_xlsx(base_flat,"C:/Users/estef/Downloads/precios.xlsx")


## Desvio mayor a 3 - PRIMAS ##

p_comprador = base_primas %>% select(Fecha, Producto, Empresa, Mes, Año, prima_compradora, media_prima_compradora, st_prima_comprador)%>%
  mutate(dist_comprador = (prima_compradora-media_prima_compradora)/st_prima_comprador) %>% filter(dist_comprador>3)%>% 
  mutate_if(is.numeric, round, 2)


p_vendedor = base_primas %>% select(Fecha, Producto, Empresa, Mes, Año, prima_vendedora, media_prima_vendedor, st_prima_vendedor)%>%
  mutate(dist_vendedora = (prima_vendedora-media_prima_vendedor)/st_prima_vendedor) %>% filter(dist_vendedora>3)%>% 
  mutate_if(is.numeric, round, 2)


## Desvio mayor a 3 - Flat ##

f_comprador = base_flat %>% select(Fecha, Producto, Empresa, Mes, Año, precio_comprador,media_compradora, st_comprador)%>%
  mutate(dist_comprador = (precio_comprador-media_compradora)/st_comprador) %>% filter(dist_comprador>3)%>% 
  mutate_if(is.numeric, round, 2)


f_vendedor = base_flat %>% select(Fecha, Producto, Empresa, Mes, Año, precio_vendedor, media_vendedor,st_vendedor)%>%
  mutate(dist_vendedora = (precio_vendedor-media_vendedor)/st_vendedor) %>% filter(dist_vendedora>3)%>% 
  mutate_if(is.numeric, round, 2)

base_primas$posicion <- as.Date(paste(base_primas$Año, base_primas$Mes, "01", sep = "-"))
base_primas <- base_primas %>% arrange(posicion)

base_flat$posicion <- as.Date(paste(base_flat$Año, base_flat$Mes, "01", sep = "-"))
base_flat <- base_flat %>% arrange(posicion)





####Grafico de Primas####

ggplotly(ggplot(base_primas,aes(x=posicion,y=prima_compradora))+
           geom_boxplot(fill = "blue")+  
           theme_classic()+  
           ggtitle("PRIMAS COMPRADORAS")+
           facet_wrap(~ Producto,scales = "free_y"))



ggplotly(ggplot(base_primas,aes(x=posicion,y=prima_vendedora))+
           geom_boxplot(fill = "blue")+  
           theme_classic()+  
           ggtitle("PRIMAS VENDEDORAS")+
           facet_wrap(~ Producto,scales = "free_y"))
  

####Grafico de FLAT####

ggplotly(ggplot(base_flat,aes(x=posicion,y=precio_comprador))+
           geom_boxplot(fill="red") +  
           theme_classic()+  
           ggtitle("FLAT COMPRADOR")+
           facet_wrap(~ Producto,scales = "free_y"))


ggplotly(ggplot(base_flat,aes(x=posicion,y=precio_vendedor))+
           geom_boxplot(fill="red") +  
           theme_classic()+  
           ggtitle("FLAT VENDEDOR")+
           facet_wrap(~ Producto,scales = "free_y"))



posicion = base_precios %>% distinct(Producto,Mes,Año,Posicion)%>% arrange(Año,Mes) %>% drop_na()

view(posicion)

primas$corregir = primas$Comprador < primas$Vendedor

primas %>% filter(corregir == "FALSE") 



rm(coeficientes, primas_ajustadas, resumen,base,base_precios,cbot,precios_no_cbot,primas,primas_ajustadas,resumen_precios)



# ## NOTIFICAR SI DURANTE 3 DIAS SEGUIDOS ALGUNA EMPRESA O BROKER INFORMO PRIMAS QUE SUPEREN 3 DESVIOS  ##
# 
# vendedor_notificar <- p_vendedor %>% group_by(Producto, Empresa, Mes, Año) %>%
#   summarise(dias = n()) %>% filter(dias == 3) 
# 
# comprador_notificar <- p_comprador %>% group_by(Producto, Empresa, Mes, Año) %>%
#   summarise(dias = n()) %>% filter(dias == 3)