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

libraries = c("tidyverse", "stats", "stargazer", "caret", "glmnet") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se establece una semilla
set.seed(8475987)

#Se importa la base de datos train.
train_final = readRDS(paste0(path, "Stores/train_final.rds"))


# Modelo de regresión. ---------------------------------------------------

#Por simplicidad se define la ecuación en una variable.
model = Ingreso_disponible ~ numero_cuartos + tipo_propiedad+Nper+
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
X = model.matrix(~ numero_cuartos + tipo_propiedad+Nper+
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
fitControl <- trainControl(
  method = "cv", 
  number = 10) 

#Ahora sí el CV para encontrar los parámetros de la red elástica que minimicen
#el MSE.
ENet<-train(model,
            data=train_final,
            method = 'glmnet', 
            trControl = fitControl,
            tuneGrid = Grilla) #Con que la grilla tenga ambos, train sabe
#que estamos hablando de elastic net.

#Ahora la cuestión se complica un poco porque cada par alpha y lambda da una
#estimación diferente. Gráficamente:
plot(ENet)

#El mixing percentage es el valor de alpha, así que por cada combinación de Lasso
#y Ridge tendremos modelos diferentes para cada lambda (por eso hay distintas curvas,
#una por cada valor de alpha que estemos probando).

#Los párametros óptimos.
ENet$bestTune
#Las métricas del modelo óptimo.
Enet_RMSE<-min(ENet$results$RMSE)
Enet_RMSE

















