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

libraries = c("tidyverse", "stats", "stargazer", "caret", "glmnet", "xtable",
              "rpart.plot", "ranger") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se establece una semilla
set.seed(8475987)
train_final = readRDS(paste0(path,"Stores/train_final.rds"))
# Modelo de regresión. ---------------------------------------------------

#Por simplicidad se define la ecuación en una variable.
model = Ingreso_disponible ~ numero_cuartos + tipo_propiedad+Npersug+
  Depto+valor_arriendo+Edad_promedio+maxEduc_hogar+
  menores+antiguedad_puesto_promedio+Oficio_hogar+
  Tiempo_trabajo_hogar+Tamaño_empresa_hogar+
  Pet_Oc_prop+Afiliado_SS+Ingreso_arriendos_pension +
  Otros_ingresos

#Se conservan las variables de interés.
aux = c("Clase", "Dominio", "total_Pet", "total_Oc")
train_final = select(train_final, -aux)

#Hay un missing value por ahí estorbando, lo eliminamos.
train_final = train_final[complete.cases(train_final),]


#Se estima el modelo de regresión.
lm_normal = lm(model, train_final)

#Se calcula el RMSE.
lm_normal_pred = predict(lm_normal, newdata = train_final)
lm_normal_RMSE = caret::RMSE(lm_normal_pred, train_final$Ingreso_disponible)


# Regresión con regularización. -------------------------------------------
#Se empleará Red Elástica, por lo que primero se preparan los parámetros a emplear.

#Primero la grilla para el lambda (peso de la penalidad). Para nos apoyamos en 
#Se preparan los vectores.
X = model.matrix(~ numero_cuartos + tipo_propiedad+Npersug+
                   Depto+valor_arriendo+Edad_promedio+maxEduc_hogar+
                   menores+antiguedad_puesto_promedio+Oficio_hogar+
                   Tiempo_trabajo_hogar+Tamaño_empresa_hogar+
                   Pet_Oc_prop+Afiliado_SS+Ingreso_arriendos_pension +
                   Otros_ingresos, train_final)
X = X[,-1]
Y = train_final$Ingreso_disponible

#Se corre la función glmnet para obtener una grilla de valores de lambda.
aux_lambda_1 <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

#Para ahorrar ram
rm(X)
rm(Y)

#Se definen los parámetros para realizar la búsqueda del parámetro óptimo.
Grilla = expand.grid(alpha = seq(0,1,0.02),
                     lambda = aux_lambda_1$lambda)

#Se definen los parámetros del CV
fitControl = trainControl(
  method = "cv", 
  number = 5) 

#Ahora sí el CV para encontrar los parámetros de la red elástica que minimicen
#el MSE.
ENet<-train(model,
            data=train_final,
            method = 'glmnet', 
            trControl = fitControl,
            tuneGrid = Grilla)

#La relación entre el lambda y el alpha gráficamente:
png(filename = paste0(path, "Views/Enet_regresion.png"),
    width = 1464, height = 750)
plot(ENet)
dev.off()

#Los párametros óptimos.
Parametros = ENet$bestTune #Fíjate que alpha es 0, entonces elastic net escogió 
#Ridge completamente.


#Las métricas del modelo óptimo.
Enet_matrix = as.data.frame(ENet$results)
Enet_matrix = filter(Enet_matrix, alpha == Parametros[1,1] & 
                       lambda == Parametros[1,2])


# Árboles -----------------------------------------------------------------
#Le vamos a encimar CV para encontrar el mejor valor de la poda.
ctrl = trainControl(method = "cv",
                    number = 10)

tree_cp = train(model, data = train_final,
                 method = "rpart",
                 trControl = fitControl,
                 tuneGrid = expand.grid(cp = 
                                          seq(0.01, 0.9, length.out = 50)))

#El mejor valor de poda.
tree_cp$bestTune$cp


# Random forest -----------------------------------------------------------
#Ya sabes que los árboles tienen una robustez mala, así que aplicamos bagging 
#para disminuir esa varianza. De paso le metemos random forest para que no estén 
#correlacionados. 

#Primero usamos la regla sqrt(p) para el tamaño del subconjunto de variables 
#para partir la muestra.

ctrl = trainControl(method = "none",
                    number = 0,
)

Grilla = expand.grid(
  mtry = c(4),
  splitrule = "variance",
  min.node.size = 2000)


RF <- train(
  model,
  data=train_final,
  method = "ranger",
  trControl = ctrl,
  tuneGrid=Grilla,
  importance="impurity",
  ntree = 100
)

#Ahora metamos CV para sacar el mejor valor posible de mtry
#la profundidad del árbol (con el mínimo de observaciones)

#Carpintería
ctrl = trainControl(method = "cv",
                    number = 5,
)

Grilla = expand.grid(
  mtry = c(3:6),
  splitrule = "variance",
  min.node.size = (seq(2000,10000,1000)))


RF_CV <- train(
  model,
  data=train_final,
  method = "ranger",
  trControl = ctrl,
  tuneGrid=Grilla,
  importance="impurity",
  ntree = 100
)

# Boosting ----------------------------------------------------------------
#Esto necesita una grilla especial de 4 hiperparámetros.
#Lo que haremos es CV para probar qué combinación de esos hiperparámetros es la mejor.

Grilla_boost = expand.grid(n.trees= c(200,300,400), #O el número de aprendizajes de 
                           #boosting (cuántos árboles va a estimar)
                           interaction.depth = c(7:10), #Qué tan profundo serán los
                           #árboles que se estimarán en cada iteración. 
                           shrinkage = 0.01, #Qué tanto vamos a relantizar el
                           #aprendizaje.
                           n.minobsinnode = c(10000) #Cuántas observaciones debe 
                           #tener un nodo para volverse final.
)

Arbol_boost = train(model,
                    data = train_final,
                    method = "gbm",
                    trControl = fitControl,
                    tuneGrid = Grilla_boost,
                    verbose = F)


# Metricas modelos --------------------------------------------------------
#Ahora se calculará el F1 Score dentro de muestra. Así que por cada modelo se 
#calculará a pie esta métrica.

#Se define una función auxiliar para calcular el F1 score.
F1_function = function(DB, i, j){
  # DB es la base de datos
  # i es el nombre de la columna que tiene los datos de entrenamiento.
  # j es el nombre de la columna con la predicción.
  
  #Para calcular el F1 necesito el precision y el recall.
  
  #Se calcula el número de verdaderos positivos (TP)
  aux = filter(DB, !!sym(i) == 1 & !!sym(j)==1)
  TP = dim(aux)[1]
  #Se calculan los Falsos negativos (FN)
  aux = filter(DB,  !!sym(i)== 1 & !!sym(j)==0)
  FN = dim(aux)[1]
  
  #Se calcula el TPR o Recall.
  Recall_aux = TP/(TP+FN)
  
  #Se calcula el número de Falsos Positivos (FP).
  aux = filter(DB, !!sym(i) == 0 & !!sym(j)==1)
  FP = dim(aux)[1]
  
  #Se calcula el Precision
  Precision = TP/(TP+FP)
  
  #Se calcula el F1 score.
  F1_score = 2*((Precision*Recall_aux)/(Precision+Recall_aux))
  
  return(F1_score)
}

#Primero para el modelo de regresión.
Pred_aux = data.frame(train_final$id,
                      train_final$Ingreso_disponible, 
                      train_final$Lp, train_final$Npersug, 
                      train_final$Pobre, 
                      "Ingreso_pred_reg" = lm_normal_pred)

#Con base en la predicción y la línea de pobreza se estima si el hogar es pobre
#o no.
#No hay que comparar directamente con la línea de pobreza, sino con Lp*Npersug.
#Es como si la Lp representaa el ingreso mínimo por persona que debe existir 
#en el hogar. Entonces si Lp por en l número de personas es menor al ingreso
#de todo el hogar, el hogar es pobre.
Pred_aux$Pobre_regresion = ifelse(Pred_aux$train_final.Lp*Pred_aux$train_final.Npersug>
                                    Pred_aux$Ingreso_pred_reg,
                                  1, 0)
#Se evalúa la función
F1_regresion = F1_function(Pred_aux, "train_final.Pobre", "Pobre_regresion")  

#Ahora para la red elástica.
Pred_aux$Ingreso_pred_ENet = predict(ENet, newdata =  train_final) #El ingreso.
Pred_aux$Pobre_ENet = ifelse(Pred_aux$train_final.Lp*Pred_aux$train_final.Npersug>
                               Pred_aux$Ingreso_pred_ENet,
                             1,0)
#Se calcula el F1 Score
F1_ENet = F1_function(Pred_aux, "train_final.Pobre", "Pobre_ENet")  

#El árbol con poda.
Pred_aux$Ingreso_pred_Arbol_cp = predict(tree_cp, newdata = train_final)
Pred_aux$Pobre_arbol_cp = ifelse(red_aux$train_final.Lp*Pred_aux$train_final.Npersug>
                                   Pred_aux$Ingreso_pred_Arbol_cp, 1, 0)

#El F1 score.
F1_Arbol_cp = F1_function(Pred_aux, "train_final.Pobre", "Pobre_arbol_cp")

#Ahora para el random forest.
Pred_aux$Ingreso_pred_RF = predict(RF, newdata = train_final)
Pred_aux$Pobre_RF = ifelse(red_aux$train_final.Lp*Pred_aux$train_final.Npersug>
                             Pred_aux$Ingreso_pred_RF, 1, 0)

#El F1 score.
F1_RF = F1_function(Pred_aux, "train_final.Pobre", "Pobre_RF")

#El RF con CV
Pred_aux$Ingreso_pred_RF_CV = predict(RF_CV, newdata = train_final)
Pred_aux$Pobre_RF_CV = ifelse(red_aux$train_final.Lp*Pred_aux$train_final.Npersug>
                                   Pred_aux$Ingreso_pred_RF_CV, 1, 0)

#El F1 score.
F1_RF_CV = F1_function(Pred_aux, "train_final.Pobre", "Pobre_RF_CV")

#Boosting.
Pred_aux$Ingreso_pred_boost = predict(Arbol_boost, newdata = train_final)
Pred_aux$Pobre_boost = ifelse(red_aux$train_final.Lp*Pred_aux$train_final.Npersug>
                                Pred_aux$Ingreso_pred_boost, 1, 0)

#El F1 score.
F1_boost = F1_function(Pred_aux, "train_final.Pobre", "Pobre_boost")

#En un data.frame
F1_DB = data.frame("Modelo" = c("Regresión", "Elastic Net", "Árbol_cp", 
                                "RF", "RF_CV", "Boosting"),
                  "F1" = c(F1_regresion, F1_ENet, F1_Arbol_cp, F1_RF,
                           F1_RF_CV, F1_boost))
xtable(F1_DB)
saveRDS(F1_DB, paste0(path,"Stores/F1_ingreso.rds"))


#Se guardan los RMSE del ingreso_disponible.
#Para el RF toca a pie
aux = (Pred_aux$train_final.Ingreso_disponible-Pred_aux$Ingreso_pred_RF)^2 |>
  mean() |> sqrt()
          
RMSE = data.frame("Modelo" = c("Regresión", "Elastic Net", "Árbol_cp",
                               "RF", "RF_CV", "Boosting"),
                  "RMSE" = c(lm_normal_RMSE, Enet_matrix[1,"RMSE"],
                             tree_cp$results[which.min(tree_cp$results$RMSE),"RMSE"],
                             aux, RF_CV$results[which.min(RF_CV$results$RMSE),"RMSE"],
                             Arbol_boost$results[which.min(Arbol_boost$results$RMSE),
                                                 "RMSE"]))

xtable(RMSE)
saveRDS(RMSE, paste0(path,"Stores/RMSE_ingreso.rds"))

#Y las predicciones de los modelos.
#saveRDS(Pred_aux, paste0(path,"Stores/Predicciones/Predicciones_regresiones.rds"))

# Hiperparámetros óptimos -------------------------------------------------

Hiperparametros = data.frame("Modelo" = c("Enet","Árbol_CP", "RF_CV"),
                             "Alpha" = c(Parametros[1,"alpha"], 
                                         tree_cp$bestTune$cp, NA),
                             "Lambda" = c(Parametros[1,"lambda"], NA, NA),
                             "mtry" = c(NA, NA, RF_CV$bestTune[1,"mtry"]),
                             "min.node.size" = c(NA, NA, RF_CV$bestTune[1,"min.node.size"])
                             )

xtable(Hiperparametros)
saveRDS(Hiperparametros, paste0(path,"Stores/Hiperparametros_regresion.rds"))

#Por simplicidad para el latex mando los del boosting aparte.
Boosting = data.frame(Arbol_boost$bestTune)
saveRDS(Boosting, paste0(path,"Stores/Hiperparametros_boosting.rds"))


















