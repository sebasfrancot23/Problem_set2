###
#
# Programación problem set 2
# Estadísticas descriptivas.
#
###
# Plan de acción: 
# A las variables continuas sacales las estadísticas de siempre: N, media, sd, percentiles.
# A las categóricas hace un gráfico de barras para ver el número de datos por categoría.
# Si hay continuas con valores atípicos, hace un histograma para analizarlos mejor.

# Preparación del ambiente ------------------------------------------------
rm(list=setdiff(ls(), c("train_final", "test_final")))

libraries = c("ggplot2", "tidyverse", "skimr", "stargazer", "gridExtra", "ggpubr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se juntan las bases en una sola para sacar las estadísticas.
DB = bind_rows(train_final, test_final)


# Análisis de missing values. ---------------------------------------------
#Tabla con total de missings y proporción al total de datos.

Missings = skim(DB[,-(1:3)]) %>% select(skim_variable, n_missing)
Missings$proporcion = round((Missings$n_missing/dim(DB)[1])*100,2)

#En Latex.
stargazer(Missings, type = "latex", title = "Valores faltantes",
          label = "Tabla_missings", summary = FALSE)

saveRDS(Missings, paste0(path,"Stores/analisis_Missings.rds"))

# Estadísticas vars continuas ---------------------------------------------
#Las variables continuas son.
continuas = c("numero_cuartos", "Nper", "Ingreso_disponible", "valor_arriendo", 
              "menores", "antiguedad_puesto_promedio", "total_Pet", "Tiempo_trabajo_hogar",
              "total_Oc", "Edad_promedio")

DB_continuas = DB[,colnames(DB) %in% continuas]

#Por propósitos de hacerlo más legible vamos a mostrar el arriendo en miles.
DB_continuas$valor_arriendo = DB_continuas$valor_arriendo/1000
#Y el ingreso
DB_continuas$Ingreso_disponible = DB_continuas$Ingreso_disponible/1000
  
#Se calculan distintas estadísticas:
#Número de observaciones que no son missing
Observations = apply(DB_continuas, MARGIN = 2, function(x){
  Non_missings = sum(!is.na(x))
  return(Non_missings)
}) 

#Media
mean_aux = round(apply(DB_continuas, MARGIN = 2, mean, na.rm = TRUE),2)

#SD
sd_aux = round(apply(DB_continuas, MARGIN = 2, sd, na.rm = TRUE),2)

#Percentiles.
percentiles = t(round(apply(DB_continuas, MARGIN = 2, quantile, na.rm = TRUE,
                           probs = c(c(0.05, 0.25, 0.5, 0.75, 0.95))),2))

#En una matriz
Estadisticas_continuas = cbind(Observations, mean_aux, sd_aux, percentiles)
colnames(Estadisticas_continuas) = c("No. Observaciones", "Media", "Desv. Estándar", 
                                 "Per. 5", "Per. 25", "Per. 50", "Per. 75", 
                                 "Per 95")
#Hacia latex
stargazer(Estadisticas_continuas, type = "latex", title = "Estadísticas descriptivas",
          subtitle = "variables continuas", label = "Tabla_continuas",
          summary = FALSE)

saveRDS(Estadisticas_continuas, paste0(path,"Stores/Estadisticas_vars_continuas.rds"))



# Estadísticas discretas --------------------------------------------------
#Se seleccionan las variables discretas.
Discretas = c("tipo_propiedad", "Pobre", "Depto", "maxEduc_hogar", "Tamaño_empresa_hogar",
              "Afiliado_SS", "Ingreso_arriendos_pension", "Otros_ingresos", 
              "Oficio_hogar")

DB_discretas = DB[,colnames(DB) %in% Discretas]
DB_discretas = na.omit(DB_discretas)

#Se hace un gráfico de barras por cada var discreta.
for (i in colnames(DB_discretas)){
  if (i=="Depto" | i=="Oficio_hogar"){
    png(filename = paste0(path, "Views/Hist_", i, ".png"),
        width = 1464, height = 750)
    aux = ggplot(DB_discretas, aes(y = as.factor(!!sym(i))), x = value) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.8, 
               width = 0.5) +
      #geom_text(stat = "count", aes(label = after_stat(count)), 
                hjust = -0.1) +
      scale_x_discrete(expand = c(0,800)) +
      labs(title = paste0("Distribución variable ", i), 
           x = "Conteo") +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 10),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(), 
            axis.line = element_line(color = "black", size = 1)) 
    print(aux)
    dev.off()
  } else {
    png(filename = paste0(path, "Views/Hist_", i, ".png"),
        width = 1464, height = 750)
    aux = ggplot(DB_discretas, aes(x = as.factor(!!sym(i)))) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.8, 
               width = 0.5) +
      geom_text(stat = "count", aes(label = after_stat(count)), 
                vjust = -0.5) +
      labs(title = paste0("Distribución variable ", i), 
           y = "Conteo") +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(), 
            axis.line = element_line(color = "black", size = 1)) 
    print(aux)
    dev.off()
  }
}































