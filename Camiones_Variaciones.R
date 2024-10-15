rm(list = ls())
library(gdata)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)
library(data.table)

#Determinamos el directorio donde alojaremos los archivos
setwd("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Analisis/Conflicto camiones/vscode")


####### CAMIONES ##############

df.full <- read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Analisis/Conflicto camiones/vscode/archivo/camiones.xlsx") %>% mutate(FECHA = as.Date(FECHA))%>% drop_na(Region)

print(paste0("Ultimo dato actualizado el ",max(df.full$FECHA)))

# Primera etapa
# Extracción de datos de la página web de Williams Agroservicios
ano=as.character(2023)
mes=c("05")
# dia=c( "01", "02","03")
# dia= as.character(26:31)
dia=c("01", "02","03","04", "05","06","07","08","09","10","11","12","13","14","15","16","17","18")
#"01", "02","03","04", "05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22", "23","24","25","26","27","28","29","30","31"

grid_fechas=expand.grid(ano,mes,dia)

fechas=c()
for (i in 1:length(grid_fechas$Var1)){
  a=paste0(grid_fechas$Var1[i],"-",grid_fechas$Var2[i],"-",grid_fechas$Var3[i],".xls");
  fechas=append(fechas,a)
}

url=c()
for (i in fechas){
  b=paste0("http://www1.williamsagroservicios.com.ar/files/camiones/",i);
  url=append(url,b)
}

for (i in 1:length(fechas)){
  tryCatch({
    download.file(url[i],destfile = fechas[i],quiet=TRUE,mode="wb")}, error=function(e){cat("ERROR :",conditionMessage(e), "/n")})
}



#### SEGUNDA ETAPA ####
#Buscamos los archivos que se encuentran en el directorio.
filenames = list.files(pattern=".xls")  

df = filenames %>%  lapply(read_excel) 

datos=df
for (i in 1:length(filenames)) {
  datos[[i]][,"FECHA"]= NULL;
  datos[[i]][,"FECHA"]= paste(filenames[i]);
  ifelse((i/length(filenames))%in%seq(0,1,0.01),print(paste0("Avance: ",(i/length(filenames))*100,"%")),0)
}

for (i in 1:length(datos)){
  datos[[i]] = datos[[i]][,colSums(is.na(datos[[i]]))<nrow(datos[[i]])];
  datos[[i]] = datos[[i]][rowSums(is.na(datos[[i]]))<ncol(datos[[i]][,1:(ncol(datos[[i]])-1)]),];
  datos[[i]] = datos[[i]][-which(datos[[i]][,1]=="PUERTOS"),];
  a=datos[[i]][1,];
  colnames(datos[[i]])= a[1:(ncol(datos[[i]])-1)];
  colnames(datos[[i]])[ncol(datos[[i]])]="FECHA";
  ifelse((i/length(datos))%in%seq(0,1,0.01),print(paste0("Avance: ",(i/length(datos))*100,"%")),0)
}

for( i in 1:length(datos)){
  colnames(datos[[i]])[1:2]=c("ELEVADORES","LOCALIDAD");
  ifelse((i/length(datos))%in%seq(0,1,0.01),print(paste0("Avance: ",(i/length(datos))*100,"%")),0)
}

df.wo.NA=NULL
for( i in 1:length(datos)){
  df.wo.NA[[i]]=datos[[i]][,which(!is.na(colnames(datos[[i]])))]
}

cantidad=NULL
for(i in 1:length(df.wo.NA)){
  cantidad=append(cantidad,ncol(df.wo.NA[[i]]))
}


#limpiando datos
data=which(cantidad>=5)
pivotear=c("TOTALES", "TRIGO", "MAIZ", "SORG", "CEBADA", "SOJA",  "GIRAS")

df_long=NULL
for( i in data){
  columnas=colnames(df.wo.NA[[i]]);
  df_long[[i]]=df.wo.NA[[i]] %>% pivot_longer(cols= all_of(which(toupper(columnas) %in% pivotear)),
                                              names_to = "Descripcion",
                                              values_to = "Cantidad",
                                              values_drop_na = TRUE);
  ifelse((i/length(df.wo.NA))%in%seq(0,1,0.01),print(paste0("Avance: ",(i/length(df.wo.NA))*100,"%")),0)
}

#Creamos una variable vac?a
unido=NULL
unido= as.data.frame(rbindlist(df_long, use.names=T, fill=T, idcol=NULL))

#comenzamos a limpiar el dataset
limpio=unido[,c(1:2,4:7)]
df.limpio=limpio[which(!limpio$Descripcion==limpio$Cantidad),]
df.limpio$Cantidad=as.numeric(df.limpio$Cantidad)
df.limpio$FECHA=as.Date(str_remove_all(df.limpio$FECHA,pattern = ".xls"))
df.limpio = df.limpio[which(!df.limpio$Cantidad<=0 & !df.limpio$Cantidad>=500000),]
df.limpio = df.limpio[which(!is.na(df.limpio$ELEVADORES)),]

quitar=c("Total","Mes","Año","Semana","TOTAL")


`%notin%` <- Negate(`%in%`)
df.limpio <- df.limpio %>% filter(ELEVADORES %notin% quitar)

#for (i in quitar){
#  a=grep(i,df.limpio$ELEVADORES);
#  df.limpio.final=df.limpio[-a,]
#}

df.limpio$LOCALIDAD[is.na(df.limpio$LOCALIDAD)] <- 0

df.limpio <- df.limpio %>% 
  mutate(Region = case_when(ELEVADORES =="Fca Vicentin"   & LOCALIDAD  == "San Lorenzo" ~ "Rosario y Zona",
                            ELEVADORES =="Mol. Cañuelas"   & LOCALIDAD  == "Rosario" ~ "Rosario y Zona",
                            ELEVADORES =="Fca Vicentin"   & LOCALIDAD  == "Ricardone" ~ "Rosario y Zona",
                            ELEVADORES =="Molinos Agro"   & LOCALIDAD  == "San Lorenzo" ~ "Rosario y Zona",
                            ELEVADORES =="Mol.Rio de la Plata"   & LOCALIDAD  == "San Lorenzo" ~ "Rosario y Zona",
                            ELEVADORES =="Mol.Rio de la Plata"   & LOCALIDAD  == "Rosario" ~ "Rosario y Zona",
                            ELEVADORES =="Pto Vicentin"   & LOCALIDAD  == "San Lorenzo" ~ "Rosario y Zona",
                            ELEVADORES =="A.C.A."   & LOCALIDAD  == "San Lorenzo" ~ "Rosario y Zona",
                            ELEVADORES =="Muelle Pampa"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="ADM"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="Nidera"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="El Transito/Toepfer"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="Fca.  Buyatti"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="Cofco Intl"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="Pta.Quebracho"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="Terminal 6"   & LOCALIDAD  == "P S Martin" ~ "Rosario y Zona",
                            ELEVADORES =="Cofco Intl"   & LOCALIDAD  == "Timbues" ~ "Rosario y Zona",
                            ELEVADORES =="Renova"   & LOCALIDAD  == "Timbues" ~ "Rosario y Zona",
                            ELEVADORES =="Ldc Arg."   & LOCALIDAD  == "Timbues" ~ "Rosario y Zona",
                            ELEVADORES =="ACA"   & LOCALIDAD  == "Timbues" ~ "Rosario y Zona",
                            ELEVADORES =="Louis Dreyfus"   & LOCALIDAD  == "Timbues" ~ "Rosario y Zona",
                            ELEVADORES =="Louis Dreyfus"   & LOCALIDAD  == "Gral.Lagos" ~ "Rosario y Zona",
                            ELEVADORES =="Cofco Arg."   & LOCALIDAD  == "Timbues" ~ "Rosario y Zona",
                            ELEVADORES =="AGD"   & LOCALIDAD  == "Timbues" ~ "Rosario y Zona",
                            ELEVADORES =="Fca Santa Clara- MRP"   & LOCALIDAD  == "Rosario" ~ "Rosario y Zona",
                            ELEVADORES =="Mol. Cañuelas"   & LOCALIDAD  == "Rosario " ~ "Rosario y Zona",
                            ELEVADORES =="Unidad 6 / 7"   & LOCALIDAD  == "Rosario" ~ "Rosario y Zona",
                            ELEVADORES =="Mol Semino"   & LOCALIDAD  == "Carcaraña" ~ "Rosario y Zona",
                            ELEVADORES =="Bunge Arg."  & LOCALIDAD  == "San Jeronimo" ~ "Rosario y Zona",
                            ELEVADORES =="Molino Chabas"   & LOCALIDAD  == "Chabas" ~ "Rosario y Zona",
                            ELEVADORES =="Aceitera Chabas"   & LOCALIDAD  == "Chabas" ~ "Rosario y Zona",
                            ELEVADORES =="Cargill / Punta Alvear"   & LOCALIDAD  == "Punta Alvear" ~ "Rosario y Zona",
                            ELEVADORES =="Ldc Arg."   & LOCALIDAD  == "Gral.Lagos" ~ "Rosario y Zona",
                            ELEVADORES =="ADM"   & LOCALIDAD  == "Arroyo Seco" ~ "Rosario y Zona",
                            ELEVADORES =="Toepfer"   & LOCALIDAD  == "Arroyo Seco" ~ "Rosario y Zona",
                            ELEVADORES =="Serv.Portuarios"   & LOCALIDAD  == "V.Constitucion" ~ "Rosario y Zona",
                            ELEVADORES =="Serv.Portuarios"   & LOCALIDAD  == "San Nicolas" ~ "Rosario y Zona",
                            ELEVADORES =="ELEVADORES"   & LOCALIDAD  == "San Nicolas" ~ "Rosario y Zona",
                            ELEVADORES =="Elevador"   & LOCALIDAD  == "San Nicolas" ~ "Rosario y Zona",
                            ELEVADORES =="Bunge Arg."   & LOCALIDAD  == "Villa Ramallo" ~ "Rosario y Zona",
                            ELEVADORES =="Bunge Arg."   & LOCALIDAD  == "Ramallo" ~ "Rosario y Zona",
                            ELEVADORES =="Molinos Cañuelas"   & LOCALIDAD  == "Pilar" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Mol. Cañuelas"   & LOCALIDAD  == "Pilar" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Ingredion"   & LOCALIDAD  == "Baradero" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Molinos Cañuelas"   & LOCALIDAD  == "San Justo" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Molinos Cañuelas"   & LOCALIDAD  == "Cañuelas" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="M Campodonico"   & LOCALIDAD  == "La Plata" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Mol.C.Norte"   & LOCALIDAD  == "Burzaco" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Lagomarsino"   & LOCALIDAD  == "Avellaneda" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Lagomarsino"   & LOCALIDAD  == "I.Casanova" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Lagomarsino"   & LOCALIDAD  == "Navarro" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Mol Argentino"   & LOCALIDAD  == "Open door" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Cofco Intl"   & LOCALIDAD  == "Lima" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Cofco Arg."   & LOCALIDAD  == "Lima" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Ardion S.A."   & LOCALIDAD  == "Baradero" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Arcor S.A."   & LOCALIDAD  == "San Pedro" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Granja 3 Arroyos"   & LOCALIDAD  == "Cap. del Señor" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Molino Cañuelas"   & LOCALIDAD  == "Las Palmas" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="TLP"   & LOCALIDAD  == "Las Palmas" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="Chs Argentina SA"   & LOCALIDAD  == "Pto Del Guazu" ~ "Darsena - Bs As - Entre Rios",
                            ELEVADORES =="A.C.A.-F.A.C.A."   & LOCALIDAD  == 0 ~ "Necochea",
                            ELEVADORES =="Renova"   & LOCALIDAD  == 0 ~ "Necochea",
                            ELEVADORES =="Cargill - Necochea"   & LOCALIDAD  == 0 ~ "Necochea",
                            ELEVADORES =="Oleag.Moreno-Necochea"   ~ "Necochea",
                            ELEVADORES =="Terminal Quequen"   & LOCALIDAD  == 0 ~ "Necochea",
                            ELEVADORES == 'Sitio "0"'   & LOCALIDAD  == 0 ~ "Necochea",
                            ELEVADORES =="Adm Agro"   & LOCALIDAD  == 0 ~ "Bahia Blanca",
                            ELEVADORES =="Terminal Bahia Blanca"   & LOCALIDAD  == 0 ~ "Bahia Blanca",
                            ELEVADORES =="Piedrabuena"   & LOCALIDAD  == 0 ~ "Bahia Blanca",
                            ELEVADORES =="Cargill B.Blanca"   & LOCALIDAD  == 0 ~ "Bahia Blanca",
                            ELEVADORES =="Oleag.Moreno-B.Blanca"   & LOCALIDAD  == 0 ~ "Bahia Blanca",
                            ELEVADORES =="Ldc Arg."   & LOCALIDAD  == 0 ~ "Bahia Blanca",
                            ELEVADORES =="LDC Bahia Blanca"   & LOCALIDAD  == 0 ~ "Bahia Blanca",
                            ELEVADORES =="Aliba  Bahia Blanca"   & LOCALIDAD  == 0 ~ "Bahia Blanca"))


test = df.limpio  %>% drop_na(Region)
test$numero_de_semana = lubridate::isoweek(test$FECHA)
test$ano = strftime(test$FECHA, format = "%Y")    
test = test%>% arrange(FECHA) %>% select(FECHA,Region,LOCALIDAD,Descripcion,Cantidad,numero_de_semana,ano)


#### TERCERA ETAPA ####

# Hago el join con la base
# Check si esos datos existen
if (max(df.full$FECHA) %in% unique(test$FECHA)){
  print("La fecha que se quiere actualizar ya se encuentra en la base histórica")
  } else {
  df.full.update <- rbind(df.full, test)%>% drop_na(Region)
  print(cat(paste0(unique(test$FECHA), " agregada a la base/n")))
  #guardo archivo generado
  write.xlsx(df.full.update,"archivo/camiones.xlsx")
  print(paste0("Base actualizada, último dato ",max(df.full.update$FECHA)))
  }

# Elimino los xls descargados
filenames = list.files(pattern=".xls")
file.remove(filenames)
