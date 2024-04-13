###
#
# Programación problem set 2
# Preparación envíos a kaggle 
#
###


# Preparación del ambiente ------------------------------------------------

rm(list=ls())

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

# Modelos de regresión ----------------------------------------------------
#Se importan las predicciones de los modelos de regresión.

Pred_regresiones = readRDS(paste0(path,"Stores/Predicciones/Predicciones_regresiones.rds"))

#Pred_regresiones = cbind(train_final$id, Pred_regresiones)

#Conservo las variables de interés (o sea el id y las predicciones).
aux = c("train_final$id", grep("^Pobre", colnames(Pred_regresiones), 
                               value = TRUE))

Pred_regresiones = Pred_regresiones[, colnames(Pred_regresiones) %in% aux]

#Se define una función auxiliar para obtener las predicciones en el formato
#adecuado para la subida.

Formato = function(DB, nombre_columna, nombre_archivo){
  aux = DB[,c("train_final$id", nombre_columna)]
  #Se cambian los nombres a los adecuados.
  colnames(aux) = c("id", "pobre")
  
  #Se exporta en formato csv.
  write.csv(aux, paste0(path, "Stores/Predicciones/",
                        nombre_archivo,".csv"),
                        row.names = F)
}

#Se corre la función para cada uno de los modelos.
Formato(Pred_regresiones, "Pobre_regresion", "regression_linearmodel")
Formato(Pred_regresiones, "Pobre_ENet", "regression_elasticnet")
Formato(Pred_regresiones, "Pobre_arbol_cp", "regression_CART")
Formato(Pred_regresiones, "Pobre_RF", "regression_Random_forest")
Formato(Pred_regresiones, "Pobre_RF_CV", "regression_Random_forest_CV")
Formato(Pred_regresiones, "Pobre_boost", "regression_boosting")






























