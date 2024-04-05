###
#
# Programación problem set 2
# Predicción del ingreso.
#
###

# Plan de acción: 
# Acá el objetivo es construir modelos que predigan el ingreso del hogar (o sea 
# una variable continúa), para después comparar el valor del ingreso predicho con
# la línea de pobreza que se tiene para cada hogar. Con esa comparación construis
# una variable sintética de pobreza. Lo que te interesa entonces es medir la 
# precisión del modelo para predicir el ingreso: mientras mejor estime el ingreso,
# la predicción de la pobreza será mejor.
# 
# El primer paso será plantear distintas combinaciones de las covariables para 
# emplear en el entrenamiento de los modelos. Luego cada combinación de X lo meterás 
# en las siguientes formas funcionales:
#   Reg lineal
#   Reg lineal con Red elástica
#   Random Forest o CART
#   Boosting
# 
# Al final quédate con los modelos que mejor predigan de acá para hacerles el análisis
# detallado en el documento.


# Preparación del ambiente ------------------------------------------------
rm(list=setdiff(ls(), c("train_final", "test_final")))

libraries = c("ggplot2", "tidyverse", "skimr", "stargazer", "gridExtra", "ggpubr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)
