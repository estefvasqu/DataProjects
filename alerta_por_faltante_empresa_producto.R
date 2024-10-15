  #Script que identifica por empresa y producto la falta de reporte durante tres días seguidos

# librerias ---------------------------------------------------------------

rm(list=ls())
#library(plyr)

library(tidyverse)
library(readxl)


# Carga de datos ----------------------------------------------------------


###############################################################################.
#########   Pasos a seguir para obtener el archivo a utilizar  ################
###############################################################################.

#Para obtener el archivo se debe ir a https://www.bolsadecereales.com/admin
#Loggearse con el usuario de agustint
#Ir a Configuración - Pos - Descarga de datos
#Elegir la fecha de los últimos 3 días hábiles disponibles
#Descarga de "datos ingresados por empresa"
#Mover el archivo a la unidad U:/Precios FOB (en caso de no tenerla crearla)

reporte = read_excel("C:/Users/evasquez/Downloads/datos_pos.xlsx")

# A todos los tipos de trigo (11.5% o 12%) los agrupamos bajo el nombre "Trigo"
reporte[grep("Trigo", reporte$Producto), "Producto"] = "Trigo"

reporte = reporte[, 1:3]
colnames(reporte) <- c("fecha", "empresa", "producto")
reporte$fecha = as.Date(reporte$fecha, "%d/%m/%Y")

###############################################################################.
############         PASO IMPORTANTE       ####################################
###############################################################################.

# saco feriados y fin de semana ----------------------------
reporte<-reporte[(reporte$fecha != "2023-02-25") & (reporte$fecha != "2023-02-26"), ]
#elimina los valores duplicados 
reporte <- unique(reporte)


# Genero los requisitos de cada empresa -----------------------------------

#AGD  Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja, Aceite de Girasol
#(1) Construccion de los requisitos de AGD para todo el mes
requisitos_agd = data.frame(empresa=c("AGD", "AGD", "AGD", "AGD", "AGD"),
                            producto=c("Maiz", "Trigo", "Harina de Soja", "Aceite de Soja", "Aceite de Girasol"))
fechas = data.frame(fecha = unique(reporte$fecha))


requisitos_agd = merge(fechas, requisitos_agd)

#(2) Todos los datos de AGD del mes
agd <- subset(reporte, empresa == "AGD")

#(3) = (1) - (2), y obtengo las faltas que cometio AGD durante el mes
faltantes_agd = setdiff(requisitos_agd, agd)

#ADM  Soja, Maiz, Trigo
requisitos_adm = data.frame(empresa=c("ADM", "ADM", "ADM"),
                            producto=c("Soja", "Maiz", "Trigo"))
requisitos_adm = merge(fechas, requisitos_adm)

adm <- subset(reporte, empresa == "ADM")
faltantes_adm = setdiff(requisitos_adm, adm)

#AFA (Soja), Maiz, Trigo, Harina de Soja y Aceite de Soja
requisitos_afa = data.frame(empresa=c("AFA", "AFA", "AFA", "AFA"),
                            producto=c("Maiz", "Trigo", "Harina de Soja", "Aceite de Soja"))
requisitos_afa = merge(fechas, requisitos_afa)

afa <- subset(reporte, empresa == "AFA")
faltantes_afa = setdiff(requisitos_afa, afa)

#Amaggi Argentina S.A. Soja, Maiz, Trigo
requisitos_amaggi = data.frame(empresa=c("Amaggi Argentina S.A.", "Amaggi Argentina S.A.", "Amaggi Argentina S.A."),
                               producto=c("Soja", "Maiz", "Trigo"))
requisitos_amaggi = merge(fechas, requisitos_amaggi)

amaggi <- subset(reporte, empresa == "Amaggi Argentina S.A.")
faltantes_amaggi = setdiff(requisitos_amaggi, amaggi)

#ACA Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja
requisitos_aca = data.frame(empresa=c("ACA", "ACA", "ACA", "ACA", "ACA"),
                            producto=c("Soja", "Maiz","Trigo","Harina de Soja", "Aceite de Soja"))
requisitos_aca = merge(fechas, requisitos_aca)

aca <- subset(reporte, empresa == "ACA")
faltantes_aca = setdiff(requisitos_aca, aca)

#Bunge Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja
requisitos_bunge = data.frame(empresa=c("Bunge", "Bunge","Bunge", "Bunge", "Bunge", "Bunge"),
                              producto=c("Soja", "Maiz", "Trigo","Harina de Soja", "Aceite de Soja", 
                                         "Aceite de Girasol"))
requisitos_bunge = merge(fechas, requisitos_bunge)

bunge <- subset(reporte, empresa == "Bunge")
faltantes_bunge = setdiff(requisitos_bunge, bunge)


#Cargill Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja, Aceite de Girasol
requisitos_cargill = data.frame(empresa=c("Cargill", "Cargill", "Cargill", "Cargill", "Cargill", "Cargill"),
                                producto=c("Soja", "Maiz", "Trigo","Harina de Soja", "Aceite de Soja", "Aceite de Girasol"))
requisitos_cargill = merge(fechas, requisitos_cargill)

cargill <- subset(reporte, empresa == "Cargill")
faltantes_cargill = setdiff(requisitos_cargill, cargill)

#CHS Soja, Maiz, Trigo
requisitos_chs = data.frame(empresa=c("CHS", "CHS", "CHS"),
                            producto=c("Soja", "Maiz", "Trigo"))
requisitos_chs = merge(fechas, requisitos_chs)

chs <- subset(reporte, empresa == "CHS")
faltantes_chs = setdiff(requisitos_chs, chs)

#COFCO Int. Argentina S.A. Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja, Aceite de Girasol
requisitos_cofco = data.frame(empresa=c("Cofco Int. Argentina S.A.", "Cofco Int. Argentina S.A.", "Cofco Int. Argentina S.A.", "Cofco Int. Argentina S.A.", "Cofco Int. Argentina S.A.", "Cofco Int. Argentina S.A."),
                              producto=c("Soja", "Maiz", "Trigo", "Harina de Soja", "Aceite de Soja", "Aceite de Girasol"))
requisitos_cofco = merge(fechas, requisitos_cofco)

cofco <- subset(reporte, empresa == "Cofco Int. Argentina S.A.")
faltantes_cofco = setdiff(requisitos_cofco, cofco)

#Curcija S.A. Maiz y (Trigo)
requisitos_curcija = data.frame(empresa=c("Curcija S.A."),
                                producto=c("Maiz"))
requisitos_curcija = merge(fechas, requisitos_curcija)

curcija <- subset(reporte, empresa == "Curcija S.A.")
faltantes_curcija = setdiff(requisitos_curcija, curcija)

#Gear (Soja), Maiz, Trigo
requisitos_gear = data.frame(empresa=c("Gear", "Gear"),
                             producto=c("Maiz", "Trigo"))
requisitos_gear = merge(fechas, requisitos_gear)

gear <- subset(reporte, empresa == "Gear")
faltantes_gear = setdiff(requisitos_gear, gear)

#LDC Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja
requisitos_ldc = data.frame(empresa=c("LDC", "LDC", "LDC","LDC","LDC"),
                            producto=c("Soja", "Maiz", "Trigo", "Harina de Soja", "Aceite de Soja"))
requisitos_ldc = merge(fechas, requisitos_ldc)

ldc <- subset(reporte, empresa == "LDC")
faltantes_ldc = setdiff(requisitos_ldc, ldc)

#Molinos Agro Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja
requisitos_molinos = data.frame(empresa=c( "Molinos Agro", "Molinos Agro", "Molinos Agro", "Molinos Agro"),
                                producto=c( "Maiz", "Trigo", "Harina de Soja", "Aceite de Soja"))
requisitos_molinos = merge(fechas, requisitos_molinos)

molinos <- subset(reporte, empresa == "Molinos Agro")
faltantes_molinos = setdiff(requisitos_molinos, molinos)

#Glencore Soja, Maiz, Trigo, Harina de Soja, Aceite de Soja
requisitos_glencore = data.frame(empresa=c("Glencore", "Glencore", "Glencore", "Glencore", "Glencore", "Glencore"),
                                 producto=c("Soja", "Maiz", "Trigo", "Harina de Soja", "Aceite de Soja", "Aceite de Girasol"))
requisitos_glencore = merge(fechas, requisitos_glencore)

glencore <- subset(reporte, empresa == "Glencore")
faltantes_glencore = setdiff(requisitos_glencore, glencore)

#combina las tablas de las empresas

faltantes = rbind(faltantes_aca, faltantes_adm, faltantes_agd, faltantes_afa,
                  faltantes_amaggi, faltantes_bunge, faltantes_cargill, faltantes_chs,
                  faltantes_cofco, faltantes_curcija, faltantes_gear, faltantes_ldc,
                  faltantes_molinos, faltantes_glencore)

rm(faltantes_aca, faltantes_adm, faltantes_agd, faltantes_afa,
   faltantes_amaggi, faltantes_bunge, faltantes_cargill, faltantes_chs,
   faltantes_cofco, faltantes_curcija, faltantes_gear, faltantes_ldc,
   faltantes_molinos, faltantes_glencore,
   aca, adm, afa, agd, amaggi, bunge, cargill, chs, cofco, curcija,
   fechas, gear, glencore, ldc, molinos, reporte,
   requisitos_aca, requisitos_adm, requisitos_afa, requisitos_agd, requisitos_amaggi, 
   requisitos_bunge, requisitos_cargill, requisitos_chs, requisitos_cofco, requisitos_curcija,
   requisitos_gear, requisitos_glencore, requisitos_ldc, requisitos_molinos)


# Finish him! -------------------------------------------------------------

empresas_notificar <- faltantes %>% group_by(empresa, producto) %>%
  summarise(n = n()) %>% filter(n == 3)

if (nrow(empresas_notificar)==0){
  print("No hay empresas que avisar")
} else {
  print("Ojo que hay que avisar a estas empresas:")
  print(empresas_notificar[,1:2])
  #print("Buscar en 'P:/Estudios_Gerencia/Precios FOB/mails-notificaciones-faltas-empresas.pdf'")
}
