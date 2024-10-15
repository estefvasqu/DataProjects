rm(list=ls())

library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)

##########IMPORTANTEEEEEEEEE###########
##CAMBIAR EL DIA DE AYER###

date = "2024-09-30"

fob = readxl::read_excel("C:/Users/estef/Downloads/serie_fob.xlsx") %>% filter(!Cereal %in% c('Trigo 12%', 'Girasol', 'Trigo 10.5%')) 
  
  
fob$Fecha = as.Date(fob$Fecha, "%d/%m/%Y")


fob = fob %>% mutate(Meses = case_when(Mes == 1 ~ "Enero" , Mes == 2 ~ "Febrero" ,
                                   Mes == 3 ~ "Marzo" , Mes == 3 ~ "Marzo" 
                                 , Mes == 4 ~ "Abril" , Mes == 5 ~ "Mayo"
                                 , Mes == 6 ~ "Junio", Mes == 7 ~ "Julio", 
                                 Mes == 8 ~ "Agosto", Mes == 9 ~ "Septiembre",
                                 Mes == 10 ~ "Octubre", Mes == 11 ~ "Noviembre", Mes == 12 ~ "Diciembre")) %>% arrange(Fecha, Mes)


fob$Precio = round(fob$Precio)


level_order <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')


ayer_hoy = fob
ayer_hoy$posicion <- as.Date(paste(ayer_hoy$Año, ayer_hoy$Mes, "01", sep = "-"))
ayer_hoy <- ayer_hoy%>% arrange(posicion)




grafico = ggplot(ayer_hoy,
       aes(x = posicion,
           y = Precio,
           color = as.factor(Fecha), group = 1)) +
  geom_line() +  
  facet_wrap(~ Cereal, scales="free_y") + theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplotly(grafico)

##########IMPORTANTEEEEEEEEE###########
## CAMBIAR EL DIA DE AYER 

# ayer = fob %>% filter(Fecha == "2023-03-13") %>%  #cambiarrrrrr
#   select(-Fecha) %>% rename(Precio_Ayer = Precio)

ayer = fob %>% filter(Fecha == date) %>%  #cambiarrrrrr
  select(-Fecha) %>% rename(Precio_Ayer = Precio)

hoy= fob %>% filter(Fecha == max(Fecha))%>% select(-Fecha) %>% rename(Precio_Hoy = Precio)


  
diff = left_join(hoy, ayer) 

diferencia_final = diff %>% mutate(diferencia_precio = Precio_Hoy - Precio_Ayer, 
                        porcentaje = ((Precio_Hoy - Precio_Ayer)/ Precio_Ayer)*100)  %>% arrange(Año)


diferencia_final$porcentaje <- round(diferencia_final$porcentaje  ,digit=2)
diferencia_final$colour <- ifelse(diferencia_final$diferencia_precio < 0, "negative","positive") 
 
diferencia_final$posicion <- as.Date(paste(diferencia_final$Año, diferencia_final$Mes, "01", sep = "-"))
diferencia_final <- diferencia_final%>% arrange(posicion)




p = ggplot(diferencia_final, aes(x= posicion, y=diferencia_precio)) +
  geom_bar(stat="identity", aes(fill = colour)) + facet_wrap( ~ Cereal)+
  geom_text(aes(label = diferencia_precio),vjust = 2,position=position_dodge(width=0.9), color="black", size=3.5)+
   theme(axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "white", size = 0.2),
        panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
   scale_fill_manual(values=c(positive="green",negative="firebrick1")) + 
  ggtitle("Diferencias entre ayer y hoy") + xlab("Meses") + ylab("Diferencia") 
  
  
 ggplotly(p)


 porcentaje = ggplot(diferencia_final, aes(x= posicion, y=porcentaje, group=1)) +
   geom_line() + facet_wrap( ~ Cereal)+ 
    geom_text(aes(label = paste0((porcentaje), "%")), size = 3,angle=180, vjust =1)+
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+ 
   ggtitle("Diferencias porcentual") + xlab("Meses") + ylab("Variacion") 
 

ggplotly(porcentaje)



