rm(list=ls())
library(xtable)
library(tidyverse)
library(chron)
library(readxl)
library(dplyr)
library(plotly)

library(reshape2)


#fecha10


#Se crea un dataframe con todos los archivos de los Datos POs
df1 <- read_excel("C:/Users/evasquez/Downloads/datos_pos.xlsx")


#Se crea un dataframe con todos los archivos de los precios de chicago
df2 <- read_excel("C:/Users/evasquez/Downloads/serie_precios_chicago.xlsx")

#Cambiar nombres de las columnas del DF de Datos POS
colnames(df1) <- c("fecha", "empresa", "producto", "referencia", "posicion_mes","posicion_ano", "prima_compradora", "prima_vendedora")
df1$fecha = as.Date(df1$fecha, "%d/%m/%Y")

#Cambiar nombres de las columnas del DF de Chicago
colnames(df2) <- c("fecha", "producto", "referencia", "cbot", "cbot_min", "cbot_max" )
df2$fecha = as.Date(df2$fecha, "%d/%m/%Y")

#Se descartan las columnas de los maximos minimos del DF de Chicago
df2 <-  df2 %>% select("fecha", "producto", "referencia", "cbot")


#Se descartan las filas que contengan el producto Girasol en los Datos POS(bolsa)
df1 <- subset(df1, producto!="Girasol")


#Creamos una tabla de coeficientes y luego la unimos con la de primas
producto<-c("Soja", "Maiz", "Harina de Soja", "Aceite de Soja", "Aceite de Girasol", "Trigo 11.5%")
coeficiente<-c(0.367454, 0.39368, 1.1023, 0.22046, 1, 1 )
coeficientes_df<-data.frame(producto, coeficiente, stringsAsFactors=FALSE)

# Se junta el DF con primas para multiplicarlo por el coeficiente que representa bushel por dolar  
primas_ajustadas <- merge(df1,coeficientes_df,by = c("producto"), all = TRUE)


#convertimos las primas a USD/t
primas_ajustadas = primas_ajustadas %>%
  mutate(prima_compradora = prima_compradora * coeficiente,
         prima_vendedora = prima_vendedora * coeficiente)


#Separamos una base para los productos sin referencia CBOT y creamos la columna cbot
precios_no_cbot = subset(primas_ajustadas, producto %in% c("Trigo 11.5%", "Aceite de Girasol"))
precios_no_cbot$cbot <- 0


#Ahora juntamos la tabla primas con df2
base = merge(primas_ajustadas, df2, by=c("fecha", "producto", "referencia"))


#Y ahora juntamos las dos tablas fob
base_final = rbind(base, precios_no_cbot)
base_final = base_final[order(base_final$producto, base_final$posicion_mes),]

#Calculamos el precio 
base_final$p_comprador = base_final$cbot + base_final$prima_compradora
base_final$p_vendedor = base_final$cbot + base_final$prima_vendedora


 

prueba= base_final %>% group_by(fecha, producto, posicion_mes, posicion_ano)%>%  
  dplyr::summarise(media_comprador = mean(p_comprador, na.rm=T),
                              media_vendedor = mean(p_vendedor, na.rm=T),
                              st_comprador = sd(p_comprador, na.rm=T),
                              st_vendedor = sd(p_vendedor, na.rm=T))
                       

total = left_join(base_final, prueba) %>% mutate(dist_comprador = (p_comprador-media_comprador)/st_comprador,
                                                 dist_vendedor = (p_vendedor-media_vendedor)/st_vendedor) %>% 
          select(fecha, producto, empresa, posicion_mes, posicion_ano, p_comprador, media_comprador, dist_comprador,
                                                p_vendedor, media_vendedor, dist_vendedor )



#probar
test = total %>% filter(dist_comprador> 1)




secuencia = seq(0, 4, by= 0.5)


comprador = c()
for ( i in secuencia){ 
  comprador[[paste(i)]] <- total  %>% dplyr::summarize(n_days = n(),
                                                      n_gt6 = sum(dist_comprador > i, na.rm = T),
                                                      omite_comprador =(n_gt6 / n_days)*100)
  desvios_comp = append(comprador, i)
}



vendedor = c()
for ( i in secuencia){ 
  vendedor[[paste(i)]] <- total  %>% dplyr::summarize(n_days = n(),
                                                       n_gt6 = sum(dist_vendedor > i, na.rm = T),
                                                       omite_vendedor =(n_gt6 / n_days)*100)
  desvios_vend = append(vendedor, i)
}


tabla_comprador = do.call(rbind.data.frame, comprador)



tabla_vendedor =do.call(rbind.data.frame, vendedor)






 # library("writexl")
 # write_xlsx(tabla_vendedor,"U://Precios FOB//vendedor.xlsx")
