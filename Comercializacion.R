rm(list=ls())
#Loading the rest package
library('rvest')
library(tidyverse)

#Specifying the url for desired website to be scraped
url <- "https://www.magyp.gob.ar/sitio/areas/ss_mercados_agropecuarios/areas/granos/_archivos/000058_Estad%C3%ADsticas/000020_Compras%20y%20DJVE%20de%20Granos.php"

#Reading the HTML code from the website
webpage <- read_html(url)


# Armo las tablas ---------------------------------------------------------

trigo <- html_nodes(webpage,xpath='//*[@id="TabbedPanels1"]/div/div[1]/table[1]') 
trigo <- html_table(trigo)
trigo <- data.frame(trigo)
fecha = strsplit(colnames(trigo)[1], "AL.")[[1]][2]
trigo["Fecha"] = lubridate::dmy(fecha)
colnames(trigo)[1] = "Sector"
trigo["Grano"] = "Trigo"

maiz <- html_nodes(webpage,xpath='//*[@id="TabbedPanels1"]/div/div[2]/table') 
maiz <- html_table(maiz,fill= TRUE)
maiz <- data.frame(maiz)
maiz["Fecha"] = lubridate::dmy(fecha)
colnames(maiz)[1] = "Sector"
maiz["Grano"] = "Maíz"

cebcer <- html_nodes(webpage,xpath='//*[@id="TabbedPanels1"]/div/div[4]/table') 
cebcer <- html_table(cebcer,fill= TRUE)
cebcer <- data.frame(cebcer)
cebcer["Fecha"] = lubridate::dmy(fecha)
colnames(cebcer)[1] = "Sector"
cebcer["Grano"] = "Cebada Cervecera"

cebfor <- html_nodes(webpage,xpath='//*[@id="TabbedPanels1"]/div/div[5]/table') 
cebfor <- html_table(cebfor,fill= TRUE)
cebfor <- data.frame(cebfor)
cebfor["Fecha"] = lubridate::dmy(fecha)
colnames(cebfor)[1] = "Sector"
cebfor["Grano"] = "Cebada Forrajera"

soja <- html_nodes(webpage,xpath='//*[@id="TabbedPanels1"]/div/div[6]/table') 
soja <- html_table(soja,fill= TRUE)
soja <- data.frame(soja)
soja["Fecha"] = lubridate::dmy(fecha)
colnames(soja)[1] = "Sector"
soja["Grano"] = "Soja"

gira <- html_nodes(webpage,xpath='//*[@id="TabbedPanels1"]/div/div[7]/table') 
gira <- html_table(gira,fill= TRUE)
gira <- data.frame(gira)
gira["Fecha"] = lubridate::dmy(fecha)
colnames(gira)[1] = "Sector"
gira["Grano"] = "Girasol"

sorgo <- html_nodes(webpage,xpath='//*[@id="TabbedPanels1"]/div/div[3]/table') 
sorgo <- html_table(sorgo,fill= TRUE)
sorgo <- data.frame(sorgo)
sorgo["Fecha"] = lubridate::dmy(fecha)
colnames(sorgo)[1] = "Sector"
sorgo["Grano"] = "Sorgo"

#junto
data = rbind(trigo,maiz,cebcer,cebfor,soja,gira,sorgo)

#elimino
rm(trigo,maiz,cebcer,cebfor,soja,gira,sorgo,webpage)
#agrego mes, año, numero de semana
data <- data %>% mutate(
  Mes = lubridate::month(data$Fecha),
  Ano = lubridate::year(data$Fecha),
  Numero_de_Semana = lubridate::week(data$Fecha)
) 

#renombro
#OJO REVIRSAR SIEMPRE LOS NOMBRES DE COLUMNA DJVE
data <- data %>% rename(Campaña = Cosecha,
               Total_Comprado = Total.Comprado..1., 
               Total_a_Fijar = Total.a.Fijar..3.,
               Total_Fijado = Total.Fijado..4., 
               DJVE_Acumuladas = DJVE..............Acumulado..6.
               )
#selecciono
data <- data %>% 
  select(Fecha, Numero_de_Semana, Mes, Ano, Campaña, Grano, Sector, Total_Comprado, Total_a_Fijar, Total_Fijado, DJVE_Acumuladas)

#me quedo con los acumulados actuales, todos los impares
odd_indexes<-seq(1,nrow(data),2)
data <- data[odd_indexes, ]

#Cambio sector
data <- data %>% mutate(
  Sector = case_when( grepl("Exportador", Sector) ~ "Sector Exportador",
                      grepl("Industria", Sector) ~"Sector Industrial",
                      T ~ "Total"))


#saco los puntos
data$Total_Comprado <- gsub('.',"" ,data$Total_Comprado,fixed = T)
data$Total_a_Fijar <- gsub('.',"" ,data$Total_a_Fijar,fixed = T)
data$Total_Fijado <- gsub('.',"" ,data$Total_Fijado,fixed = T)
data$DJVE_Acumuladas <- gsub('.',"" ,data$DJVE_Acumuladas,fixed = T)


#saco las comas
data$Total_Comprado <- gsub(',',"." ,data$Total_Comprado,fixed = T)
data$Total_a_Fijar <- gsub(',',"." ,data$Total_a_Fijar,fixed = T)
data$Total_Fijado <- gsub(',',"." ,data$Total_Fijado,fixed = T)
data$DJVE_Acumuladas <- gsub(',',"." ,data$DJVE_Acumuladas,fixed = T)


#primero tengo que remplazar los últimos string que son (*)
data$Total_Comprado = gsub("    (*)","",data$Total_Comprado, fixed=T)
data$Total_a_Fijar = gsub("    (*)","",data$Total_a_Fijar, fixed=T)
data$Total_Fijado = gsub("    (*)","",data$Total_Fijado, fixed=T)
data$DJVE_Acumuladas = gsub("    (*)","",data$DJVE_Acumuladas, fixed=T)

#primero tengo que remplazar los últimos string que son (*)
data$Total_Comprado = gsub(" (*)","",data$Total_Comprado, fixed=T)
data$Total_a_Fijar = gsub(" (*)","",data$Total_a_Fijar, fixed=T)
data$Total_Fijado = gsub(" (*)","",data$Total_Fijado, fixed=T)
data$DJVE_Acumuladas = gsub(" (*)","",data$DJVE_Acumuladas, fixed=T)


#primero tengo que remplazar los últimos string que son (*)
data$Total_Comprado = gsub("(*)","",data$Total_Comprado, fixed=T)
data$Total_a_Fijar = gsub("(*)","",data$Total_a_Fijar, fixed=T)
data$Total_Fijado = gsub("(*)","",data$Total_Fijado, fixed=T)
data$DJVE_Acumuladas = gsub("(*)","",data$DJVE_Acumuladas, fixed=T)

#paso a numeric
data$Total_Comprado = as.numeric(data$Total_Comprado)
data$Total_a_Fijar = as.numeric(data$Total_a_Fijar)
data$Total_Fijado = as.numeric(data$Total_Fijado)
data$DJVE_Acumuladas = as.numeric(data$DJVE_Acumuladas)


# Genero nuevas variables para el rbind -----------------------------------

data <- data %>% rowwise() %>% 
  mutate(Total_sin_Precio = sum(Total_a_Fijar,-Total_Fijado, na.rm = T),
         Total_con_Precio = sum(Total_Comprado,-Total_sin_Precio,na.rm = T)) 

#Nuevos id
data$Ano_Campaña<-as.numeric(str_sub(data$Campaña,-2,-1))+2000 #genero variable ano de camapaña
data$Ano_Mes<-paste0(data$Ano,data$Mes)

#Tengo que generar un ID que sea:
# puede haber 1 año mas todavía
data <- data %>% mutate(id = ifelse(Ano < Ano_Campaña, Numero_de_Semana, 
                                    ifelse(Ano == Ano_Campaña,53 + Numero_de_Semana, 53+53+Numero_de_Semana)))



# Cargo data --------------------------------------------------------------

base = read.csv("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Dashboards/Monitor Economico/Inputs/Comercializacion/Base.csv",sep = ',', fileEncoding = "utf-8", check.names = F)%>% select(-1) %>% mutate(Fecha = as.Date(Fecha))

prod_expo <- readxl::read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Dashboards/Monitor Economico/Inputs/Comercializacion/prod_expo.xlsx") %>% rename("Campaña"=Campana)


if (max(base$Fecha) < max(data$Fecha)){
  print("Actualizando la base ...")
  a <- rbind(base, data)
  #revisar este sino solamente dejamos rbind y listo
  a <- a %>% 
    group_by(Ano, Campaña, Grano, Sector) %>% 
    mutate(Total_Comprado = ifelse(is.na(Total_Comprado),lag(Total_Comprado),Total_Comprado),
           Total_a_Fijar = ifelse(is.na(Total_a_Fijar), lag(Total_a_Fijar),Total_a_Fijar),
           DJVE_Acumuladas = ifelse(Sector == "Sector Exportador" & is.na(DJVE_Acumuladas), lag(DJVE_Acumuladas), DJVE_Acumuladas),
           DJVE_Acumuladas = ifelse(Sector == "Total" & is.na(DJVE_Acumuladas), lag(DJVE_Acumuladas), DJVE_Acumuladas)
    )
 
  write.csv(a, "./Inputs/Comercializacion/prod_expo.xlsx")

  print("Actualizando la base de la app...")
  b <- left_join(a, prod_expo)
  
  #hacer case_when - total con prod, exportador con expo, industria con prod?
  b <- b %>%  mutate(
    Total_Comprado_Porc_Prod = ifelse(Total_Comprado/Producción*100 > 100, 100, Total_Comprado/Producción*100),
    Total_sin_Precio_Porc_Prod = ifelse(Total_sin_Precio/Producción*100 > 100, 100, Total_sin_Precio/Producción*100),
    Total_con_Precio_Porc_Prod = ifelse(Total_con_Precio/Producción*100 > 100, 100, Total_con_Precio/Producción*100),
    DJVE_Acumuladas_Porc_Prod = ifelse(DJVE_Acumuladas/Producción*100 > 100, 100, DJVE_Acumuladas/Producción*100),
    Total_Comprado_Porc_Expo = ifelse(Total_Comprado/Exportaciones*100 > 100, 100, Total_Comprado/Exportaciones*100),
    Total_sin_Precio_Porc_Expo = ifelse(Total_sin_Precio/Exportaciones*100 > 100, 100, Total_sin_Precio/Exportaciones*100),
    Total_con_Precio_Porc_Expo = ifelse(Total_con_Precio/Exportaciones*100 > 100, 100, Total_con_Precio/Exportaciones*100),
    DJVE_Acumuladas_Porc_Expo = ifelse(DJVE_Acumuladas/Exportaciones*100 > 100, 100, DJVE_Acumuladas/Exportaciones*100)
    
  )
  
  write.csv(b, "./Outputs/comercializacion.csv")
  print(paste0("Base actualizada al ",max(data$Fecha)))
} else {
  print(paste0("La base ya esta actualizada al ",max(a$Fecha)))
}

