
#--------------------------------------------------------------------------------
# Tema:       Solucion del Ejercicio - 1
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      11-08-2021
# Datos:      Encuesta Telefonica sobre COVID-19 y Mercado Laboral, INEGI.
#             https://www.inegi.org.mx/investigacion/ecovidml/2020/#Microdatos
# Github:     https://github.com/jcms2665/Tools-for-Demography/tree/main/R
# Notas:      Se recomienda seguir el procedimiento paso por paso     


# Objetivo:   Replicar el tabulado 2.1 (archivo encovid_ml_indicadores, fila 45) 
#             de la ECOVID-ML. Este tabulado muestra la distribucion de los 
#             ocupados en 3 categorias: Subordinados, independientes y sin pago. 

#--------------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias
library(foreign);library(questionr); library(ggplot2); library(dplyr) 


#2.  Directorio de trabajo
setwd(".../Datos")


#3.  Importar datos
encovid<-read.dbf("ecovid0720.dbf")%>%data.frame()


#4. Tabular la variable de interes 
wtd.table(encovid$POS_OCU)


#5. Filtrar la base: solo interesa los codigos 1, 2 y 3
encovid=encovid%>%filter(POS_OCU>=1) 


#6. Etiquetar los datos creando la variable "ocup"
encovid$ocup <- factor(encovid$POS_OCU,levels = c(1,2,3),labels = c("Ocupado","Desocupado","Disponible"))


#7. Tabular la nueva variable
wtd.table(encovid$ocup)                       


#8. Ponderar el tabulado con weights
wtd.table(encovid$ocup, weights = encovid$FAC_PER)                       


#9. Ponderar el tabulado con weights
wtd.table(encovid$ocup, weights = encovid$FAC_PER)%>%prop.table()                    



