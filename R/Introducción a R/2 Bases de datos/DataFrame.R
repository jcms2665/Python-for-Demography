
#--------------------------------------------------------------------------------
# Tema:       DataFrame
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      Encuesta Telefonica sobre COVID-19 y Mercado Laboral, INEGI.
#             https://www.inegi.org.mx/investigacion/ecovidml/2020/#Microdatos
# Github:     https://github.com/jcms2665/Tools-for-Demography/tree/main/R
# Notas:       

# Contenido             

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Exploracion inicial
#           4.1 Etiquetar variables
#           4.1 Nuevas variables etiquetadas
#       5. Recodificar variables
#           5.1 Validar tipo de dato
#           5.2 Cambiar formato
#           5.3 Crear nueva variable 
#           5.4 Generar rangos 
#       6. Subconjuntos de datos
#           6.1 Variables (columnas)
#           6.2 Casos (filas)
#           6.3 Casos (filas) y variables (columnas)
#       7. Reto

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign);library(questionr); library(ggplot2); library(dplyr) 

#---------------------------------------
# NOTA:
# Si aparece un error, revisa en la
# pestana "packages" si el paquete 
# este instalado
#---------------------------------------


#2.  Directorio de trabajo

getwd()
setwd(".../Datos")

#---------------------------------------
# NOTA:
# Si aparece un error, revisa el tipo
# de diagonal que tienes. 
# Se debe usar "/" o "//"
#---------------------------------------



#3.  Importar datos

    #3.1 Opcion 1
    inegi<-data.frame(read.dbf("ecovid0720.dbf"))
    
    #3.2 Opcion 2
    encovid<-read.dbf("ecovid0720.dbf")%>%data.frame()

    
    #---------------------------------------
    # NOTA:
    # El operador %>% sirve para vincular
    # propiedades y funciones
    #---------------------------------------
    
    
    rm("inegi")                               # rm sirve para quitar elementos

    
    
#4.  Exploracion inicial

names(encovid)
head(encovid,2)

# PB1 = Sexo
wtd.table(encovid$PB1)



  #4.1  Etiquetar variables
  

    # Para etiquetar una variable se usa la funcion "factor" con
    # tres argumentos: 
    
    #  i.   La variable que se va etiquetar:  encovid$PB1
    #  ii.  Los valores (levels)              c(1,2)
    #  iii. Las etiquetas (labels)            c("Hombre","Mujer")
    
    
    encovid$PB1 <- factor(encovid$PB1,levels = c(1,2),labels = c("Hombre","Mujer"))
    wtd.table(encovid$PB1)
    
    
    #-----------------------------------
    # NOTA:
    # Al etiquetar una variable, se 
    # sustituye su contenido original 
    #-----------------------------------



  # 4.2 Etiquetar y crear nuevas variables
    
    # CLASE2 = Poblacion ocupada
    wtd.table(encovid$CLASE2)
    
    # Codigos: 1=Ocupado, 2=Desocupado, 3=Disponible, 4=No disponible
    encovid$ocup <- factor(encovid$CLASE2,levels = c(1,2,3,4),labels = c("Ocupado","Desocupado","Disponible","No disponible"))
    
    wtd.table(encovid$ocup, weights = encovid$FAC_PER)                      # Con "weights=" se ponderan los datos 
    
    wtd.table(encovid$ocup, weights = encovid$FAC_PER)%>%prop.table()       # Frecuencias ponderadas 
    
    

#5. Recodificar variables

    #5.1 Validar tipo de dato
    
    # PB2 = Edad
    class(encovid$PB2)
    
    #-----------------------------------
    # NOTA:
    # El 80% de los errores ocurren 
    # por no considerar el tipo de 
    # variable que tenemos
    #-----------------------------------
    

    #5.2 Cambiar formato
    
    encovid$PB2 <-as.numeric(as.character(encovid$PB2))
    class(encovid$PB2)
    
  
    #5.3 Crear nueva variable 
    
    length(encovid)                             # numero de variables
    encovid$edad_recod<-0                       # agregar una variable
    length(encovid)
  
  
    #5.4 Generar rangos  
    
    encovid$edad_recod[encovid$PB2 >= 0 & encovid$PB2 <=30] <- 1
    encovid$edad_recod[encovid$PB2 >= 31 & encovid$PB2 <=59] <- 2
    encovid$edad_recod[encovid$PB2 >= 60] <- 3
  
    wtd.table(encovid$edad_recod)

    encovid$edad_recod<-factor(encovid$edad_recod,levels = c(1,2,3),labels = c("Menos de 30","31-59","60 y m?s"))
    wtd.table(encovid$edad_recod)
    wtd.table(encovid$edad_recod)%>%prop.table()

    

#6. Subconjuntos con datos reales

    #6.1 Seleccionar VARIABLES (columnas)
    encovid.1 <-encovid %>%select("PB1", "POS_OCU","FAC_PER")
    
  

    #6.2 Seleccionar CASOS (filas)
    encovid.2 <-encovid %>%filter(PB2<30)
    
    
    #6.3 Seleccionar CASOS (filas) y VARIABLES (columnas)
    encovid.3 <- encovid %>%select("PB1", "POS_OCU","FAC_PER")%>%filter(PB2<30)
    
    #------------------------------------------
    # NOTA:
    # Por que ocurre el error? 
    #------------------------------------------
    
    encovid.3 <- encovid%>%filter(PB2<30)%>%select("PB1", "POS_OCU","FAC_PER")
    
    
    
