###
#
# Programación problem set 2
# Predicción de la pobreza.
#
###

# Plan de acción: 
# Acá el objetivo es directamente usar modelos para clasificar un hogar como pobre o 
# no pobre. Es decir que chao la regresión lineal. Apoyáte en los mismos conjuntos 
# de covariables que definitiste para predecir el ingreso. Combina cada uno de esos
# conjuntos con las siguientes formas funcionales:
#   Logit y Probit
#   Logit y Probit enfocados en F1 score
#   Logit y Probit enfocados en F1 score con regularización
#   Random Forest o CART.

# Al final quédate con los modelos que mejor predigan de acá para hacerles el análisis
# detallado en el documento.

 
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
