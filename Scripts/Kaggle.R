###
#
# Programación problem set 2
# Preparación envíos a kaggle 
#
###


# Preparación del ambiente ------------------------------------------------



#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se importa la base de testeo.
test_final = readRDS(paste0(path,"Stores/test_final.rds"))

# Modelos de regresión ----------------------------------------------------

aux = readRDS(paste0(path, "Stores/Procesadas/test_hogares.rds"))



#Para el modelo de regresión.
Pred_aux = data.frame("id" = test_final$id,
                      "Ingreso" = predict(lm_normal, 
                                            newdata = test_final))
#Se crea la variable que indica si es pobre o no.
Pred_aux = mutate(Pred_aux, pobre = ifelse(test_final$Lp*test_final$Npersug>
                                             Ingreso,1,0))

Pred_aux = Pred_aux[complete.cases(Pred_aux),]
#Conservo las variables de interés.
Pred_aux = Pred_aux[,c(1,3)]

#Carpintería chimba.
id = c(
  "2a7ddc2779480d7f19834953",
  "a0c2e751e582fd49d564f308",
  "57273d19e8464a5ff66a582b",
  "418d052ff7878940ab938601",
  "212a37fc17016a3c78f76852",
  "7b0b8c4814944383d6c8cef1"
)


aux = matrix(0, nrow = 4, ncol = 2)
aux[,1] = 1:4
colnames(aux) = colnames(Pred_aux)
Pred_aux = rbind(Pred_aux, aux)
                                    
#Se exporta
write.csv(Pred_aux, paste0(path, 
                           "Stores/Predicciones/regression_lm.csv"),
          row.names = F)




Formato(test_final, ENet, "regression_elasticnet")
Formato(test_final, tree_cp, "regression_CART")
Formato(test_final, tree_cp, "regression_CART")
Formato(test_final, RF_CV, "regression_Random_forest_CV", skip = T)
Formato(test_final, Arbol_boost, "regression_boosting", skip = T)

#Random forest es un poquito diferente porque es ranger
Pred_aux = data.frame("id" = test_final$id,
                      "Ingreso" = predict(RF, data = test_final))



Formato(test_final, RF, "regression_Random_forest", skip = T)


































