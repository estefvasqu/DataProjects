rm(list = ls(all.names = TRUE)) 
library(tidyverse)
library(readxl)
library("lubridate")
library(writexl)
library(plotly)
library(grDevices)


#Descargar FOBs de 1 MES

precios <-  readxl::read_excel("C:/Users/evasquez/Downloads/serie_fob.xlsx") %>% drop_na()
precios$Fecha = as.Date(precios$Fecha, "%d/%m/%Y")


precios$Posicion <- as.Date(paste(precios$Año, precios$Mes, "01", sep = "-"))


#precios = precios %>% mutate(ID_fecha = as.numeric(factor(Fecha))) %>% mutate(ID_semana = case_when(ID_fecha <= 5 ~ 1,
                                                                                                    # ID_fecha <= 14 & ID_fecha >=  10 ~ 4,
                                                                                                    # ID_fecha >= 15 ~ 5))

precios = precios %>% mutate(ID_fecha = as.numeric(factor(Fecha))) %>% mutate(max_ID_fecha = max(ID_fecha)) %>%  mutate(ID_semana = case_when(ID_fecha <= 5 ~ 1,
                                                                                                    ID_fecha <= 14 & ID_fecha >=  10 ~ 4,
                                                                                                    ID_fecha > (max_ID_fecha - 5) ~ 5)) %>% select(-max_ID_fecha)

resumen= precios %>% group_by(Cereal, Año, Mes,Posicion, ID_semana)  %>% dplyr::summarise(Promedio = mean(Precio, na.rm=T)) %>% 
  drop_na(ID_semana) %>% arrange(Cereal, Año, Mes, Posicion, ID_semana)
  
  
  

resumen$Cereal <- replace(resumen$Cereal, resumen$Cereal == "Maiz", "Maíz")


lines = c("5" = "solid", "4" = "longdash", "1" = "dotted")

colors <- c("#404040", "#92C5DE", "#00204Dff")


ggplotly(ggplot(resumen,aes(x=Posicion,y=Promedio, group = as.factor(ID_semana)))+
          geom_line(aes(linetype=as.factor(ID_semana), color= as.factor(ID_semana)),size=0.8)+
           scale_linetype_manual(values = lines)+
           scale_color_manual(values = colors)+
           theme_classic()+
           theme(text = element_text(size = 12), 
                 axis.title.x = element_text(size =10), 
                 axis.text.x = element_text(size = 12,color = "black"),  
                 axis.title.y = element_text(size = 10, color = "black"),
                 strip.text.x = element_text(size = 12,face ="bold"),
                 #strip.background =  element_rect(colour = "black",fill="transparent", linetype="solid")
                 strip.background = element_blank()) +
           ggtitle("")+
           xlab("")+
           ylab("")+
           facet_wrap(~ Cereal,scales = "free_y"))


write_xlsx(resumen,"C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/CURVA FOB/Curva FOB.xlsx")
write_xlsx(precios,"C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2023/CURVA FOB/prueba_precios.xlsx")




# Obtener el mes actual
mes_actual <- as.numeric(format(Sys.Date(), "%m"))
ano_actual <- as.numeric(format(Sys.Date(), "%Y"))

# Filtrar por el mes y año actuales
resumen_actual <- resumen %>% filter(Mes == mes_actual & Año == ano_actual)

# Calcular la variación porcentual y redondear a 1 decimal
variacion <- resumen_actual %>%
  filter(ID_semana %in% c(1, 4, 5)) %>%
  spread(ID_semana, Promedio) %>%
  mutate(
    Variacion_5_a_4 = round((`5` - `4`) / `4` * 100, 1),
    Variacion_5_a_1 = round((`5` - `1`) / `1` * 100, 1)
  ) %>%
  select(Cereal, Año, Mes, Posicion, Variacion_5_a_4, Variacion_5_a_1)

# Mostrar el resultado
print(variacion)