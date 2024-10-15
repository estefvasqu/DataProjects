rm(list=ls())
library(tidyverse)
library(dplyr)
library(tidyr)
library("writexl")

balances = readxl::read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Balances/Base BCEs.xlsm", sheet = "BASE Full") 
balances$`2010/11`= as.numeric(balances$`2010/11`)

balances = balances %>% subset(select = c(1:30))%>% mutate_if(is.numeric, round, 2) %>% pivot_longer(cols=c(6:28),names_to='Campaña', values_to='Valor') 
  
balances = balances %>% select(Commodity, Attribute, Fuente, Campaña, Meses, Referencia, Valor, Unit, `Numero/Mes`) %>% filter(Campaña %in%  c("2017/18", "2018/19","2019/20", "2020/21", "2021/22", "2022/23" ))

#### Elimino la molienda que no necesito
balances =  subset(balances, !(Commodity %in% c("Trigo", "Maíz") & Attribute %in% c("Molienda", "Molienda")))%>% 
  rename("Producto" = "Commodity", "Atributo" = "Attribute", "id" = "Numero/Mes") 
balances$Meses = gsub('Ene', 'Enero',
                      gsub('Jan', 'Enero', 
                      gsub('Feb', 'Febrero',
                           gsub('Mar', 'Marzo',
                                gsub('Abr', 'Abril',
                                     gsub('Apr', 'Abril',
                                     gsub('May', 'Mayo',
                                          gsub('Jun', 'Junio',
                                               gsub('Jul', 'Julio',
                                                    gsub('Ago', 'Agosto',
                                                         gsub('Sep', 'Septiembre',
                                                              gsub('Oct', 'Octubre',
                                                                   gsub('Nov', 'Noviembre',
                                                                        gsub('Dic', 'Diciembre',
                                                                             gsub('Dec', 'Diciembre',
                                                                                  gsub('TOTAL', 'Total',
                                                                                  balances$Meses))))))))))))))))

write_xlsx(balances,"C:/Users/evasquez/Desktop/DASHBOARD/Tablero_Económico - Modificaciones/datos_balances.xlsx")


