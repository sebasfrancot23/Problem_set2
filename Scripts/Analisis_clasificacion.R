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
rm(list=setdiff(ls(), c("train_final", "test_final", "Grilla")))

libraries = c("ggplot2", "tidyverse", "pROC", "caret", "stats", "xtable", 
              "MLmetrics") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

set.seed(8475987)

#Para entender mejor las categorías.
train_final = readRDS(paste0(path, "Stores/train_final.rds"))
train_final = train_final %>% 
  mutate(Pobre = factor(Pobre,levels = c(0,1), labels = c("No_pobre", "Pobre")))
train_final = train_final[complete.cases(train_final),]
# Logit y probit ----------------------------------------------------------
#Como el propósito ahora es es predecir pobre, se cambia la ecuación.
model = Pobre ~ numero_cuartos + tipo_propiedad+Nper+
  Depto+valor_arriendo+Edad_promedio+maxEduc_hogar+
  menores+antiguedad_puesto_promedio+Oficio_hogar+
  Tiempo_trabajo_hogar+Tamaño_empresa_hogar+
  Pet_Oc_prop+Afiliado_SS+Ingreso_arriendos_pension +
  Otros_ingresos


#Primero el logit
Logit = glm(model, family = binomial(link = "logit"), data = train_final)

#El probit
Probit = glm(model, family = binomial(link = "probit"), data = train_final)

#Con las estimaciones, se predice P(Yi=1)
Pred_aux = data.frame(train_final$Pobre, 
                      "Pr_logit" = predict(Logit, newdata = train_final, 
                                           type = "response"),
                      "Pr_probit" = predict(Probit, newdata = train_final, 
                                            type = "response"))

#Como es una estimación desbalanceada (mira el gráfico de barras de Pobre), para 
#mejorar la predicción de Pobre se recurre a calcular la curva 
#ROC y encontrar su punto óptimo.

#La curva roc
roc_logit = roc(Pred_aux$train_final.Pobre ~ Pred_aux$Pr_logit, plot = T, auc = F)
roc_probit = roc(Pred_aux$train_final.Pobre ~ Pred_aux$Pr_probit, plot = T, auc = F)

#Para gráficar el auc.
auc_logit = auc(roc_logit)
auc_probit = auc(roc_probit)

#Una vez especificada la curva ROC, nos quedamos con el treshold óptimo.
Threshold_logit = coords(roc_logit, "best", ret = "threshold", maximize = "s")
Threshold_probit = coords(roc_logit, "best", ret = "threshold", maximize = "s")

#Con ambos treshold se realizan las predicciones y se calculan las matrices 
#de confusión.
Pred_aux = Pred_aux %>% mutate(Pobre_logit = 
                                 ifelse(Pr_logit>=Threshold_logit[1,1], 1, 0)) %>%
  mutate(Pobre_logit = factor(Pobre_logit, levels = c(0,1), 
                              labels = c("No_pobre", "Pobre"))) %>%
  mutate(Pobre_probit = ifelse(Pr_probit>=Threshold_probit[1,1], 1, 0)) %>%
  mutate(Pobre_probit = factor(Pobre_probit, levels = c(0,1), 
                              labels = c("No_pobre", "Pobre")))
  
#Ahora las matrices de confusión.
#Para el logit
aux_logit = confusionMatrix(data = Pred_aux$Pobre_logit, reference = Pred_aux$train_final.Pobre,
                positive = "Pobre")

#Para el probit
aux_probit = confusionMatrix(data = Pred_aux$Pobre_probit, reference = Pred_aux$train_final.Pobre,
                positive = "Pobre")

#Se exportan las matrices.
xtable(aux_logit$table)
saveRDS(aux_logit$table, paste0(path,"Stores/Confusion_logit_sintuning.rds"))

xtable(aux_probit$table)
saveRDS(aux_probit$table, paste0(path,"Stores/Confusion_probit_sintuning.rds"))

#Pasos:
# Predecir las probabilidades
# Gráficar la curva roc
# Encontrar el treshold óptimo de la curva ROC
# Calcular la matriz de confusión con ese treshold
# Calcular el F1 score


# Model tunning ----------------------------------------------------------
#Se vuelve a estimar un logit y un probit pero esta vez restringiendo la 
#estimación a intentar maximizar el F1.

#Para ello usamos Caret. Primero la carpintería.
multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

ctrl_multiStats<- trainControl(method = "none",
                               number = 0,
                               summaryFunction = multiStats,
                               classProbs = TRUE,
                               verbose=FALSE,
                               savePredictions = T)

logit_tunning = train(model, method = "glmnet", data = train_final,
                      family = "binomial", trControl = ctrl_multiStats,
                      preProcess = c("center", "scale"),
                      metric = "F1")

#El model tunning se combina con la métrica de alternative cutoff. Para ello se
#calcula curva roc nuevamente.

#La predicción de la probabilidad.
Pred_aux$Pr_logit_tunning = predict(logit_tunning, newdata = train_final,
                                    type = "prob")[[2]]

#Calculo de la curva roc
logit_tunning_roc = roc(Pred_aux$train_final.Pobre ~ Pred_aux$Pr_logit_tunning, 
                        plot = T, auc = F)
auc_logit_tunning = auc(logit_tunning_roc)

#El treshold óptimo.
Threshold_logit_tunning = coords(logit_tunning_roc, "best", ret = "threshold", 
                                 maximize = "s")
#Las predicciones de pobre.
Pred_aux = Pred_aux %>% mutate(Pobre_logit_tunning = 
                                 ifelse(Pr_logit_tunning>=Threshold_logit_tunning[1,1], 
                                        1, 0)) %>%
  mutate(Pobre_logit_tunning = factor(Pobre_logit_tunning, levels = c(0,1), 
                              labels = c("No_pobre", "Pobre")))

#La matriz de confusión para obtener el F1 score.
aux_logit_tunning = confusionMatrix(data = Pred_aux$Pobre_logit_tunning, 
                                    reference = Pred_aux$train_final.Pobre,
                            positive = "Pobre")

# Logit con tunning y regularización. -------------------------------------
#Al model tunning se le aplica ahora elastic net y el alternative cut off.
#La grilla se definió en el código anterior.



#Se definen los parámetros del CV
ctrl_multiStats<- trainControl(method = "cv",
                               number = 5,
                               summaryFunction = multiStats,
                               classProbs = TRUE,
                               verbose=FALSE,
                               savePredictions = T)

Grilla = expand.grid(alpha = seq(0,1,0.2),
                     lambda = seq(0,10,10))
#El train para realizar la búsqueda de los hiperparámetros óptimos.
logit_tunning_enet = train(model, method = "glmnet", data = train_final,
                      family = "binomial", trControl = ctrl_multiStats,
                      preProcess = c("center", "scale"),
                      tuneGrid = Grilla,
                      metric = "F1")

#Los mejores hiperparámetros son:
Best_tune = logit_tunning_enet$bestTune

#Nos quedamos con las predicciones en los mejores parámetros
aux = logit_tunning_enet$pred
aux = filter(aux, alpha == Best_tune[1,1] & lambda == Best_tune[1,2])

Pred_aux$Pr_logit_enet = aux[["Pobre"]]
rm(aux)

#Se calcula la curva roc para obtener el mejor punto de corte.
logit_enet_roc = roc(Pred_aux$train_final.Pobre ~ Pred_aux$Pr_logit_enet, 
                     plot = T, auc = F)
auc_logit_enet = auc(logit_enet_roc)

#Threshold óptimo
Threshold_logit_enet = coords(logit_enet_roc, "best", ret = "threshold", 
                                 maximize = "s")
#Las predicciones de pobre.
Pred_aux = Pred_aux %>% mutate(Pobre_logit_enet = 
                                 ifelse(Pr_logit_enet>=Threshold_logit_enet[1,1], 
                                        1, 0)) %>%
  mutate(Pobre_logit_enet = factor(Pobre_logit_enet, levels = c(0,1), 
                                      labels = c("No_pobre", "Pobre")))

#La matriz de confusión para obtener el F1 score.
aux_logit_enet = confusionMatrix(data = Pred_aux$Pobre_logit_enet, 
                                    reference = Pred_aux$train_final.Pobre,
                                    positive = "Pobre")

# Curvas ROC. -------------------------------------------------------------
#Luego de haber estimado distintos modelos Logit Y Probit se gráfican sus curvas
#roc para entender cuál está siendo el mejor predictor en términos del TPR. 

png(filename = paste0(path, "Views/Curvas_roc.png"),
    width = 1464, height = 750)
plot(roc_logit, col = "blue", main = "Curvas ROC de Logit y Probit", print.auc = F)
plot(roc_probit, col = "red", add = TRUE, lty = 2, print.auc = F)
plot(logit_tunning_roc, col = "green", add = TRUE, lty = 2, print.auc = F)
plot(logit_enet_roc, col = "orange", add = TRUE, lty = 2, print.auc = F)
legend("bottomright", legend = c(paste("Logit (AUC =", round(auc_logit, 3), ")"),
                                 paste("Probit (AUC =", round(auc_probit, 3), ")"),
                                 paste("Model tunning (AUC =", 
                                       round(auc_logit_tunning,3), ")"),
                                 paste("Red Elástica (AUC =", 
                                       round(auc_logit_enet, 3), ")")),
       col = c("blue", "red", "green", "orange"), lty = 1:2)
dev.off


# Métricas modelos --------------------------------------------------------
#En un solo dataframe se condensan los F1 scores de los modelos.
F1_DB = data.frame("Modelo" = c("Logit", "Probit", "Logit_tunning", "Logit_enet"),
                   "F1" = c(aux_logit$byClass["F1"], aux_probit$byClass["F1"],
                            aux_logit_tunning$byClass["F1"], 
                            aux_logit_enet$byClass["F1"]))
xtable(F1_DB)
saveRDS(F1_DB, paste0(path,"Stores/F1_clasificacion.rds"))
























