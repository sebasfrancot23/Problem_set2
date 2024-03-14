###################################
#
# Programación problem set 2
# Importación y limpieza de datos.
# Integrantes:
# Sebastian Franco Torres
#
#######################


# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("rvest", "tidyverse", "skimr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)



# Importación base de datos -----------------------------------------------
list.files(path = paste0(path,"Stores/Pre_procesadas/"))

#Se importa la información para los hogares.
train_hogares = read.csv(paste0(path, "Stores/Pre_procesadas/train_hogares.csv"))

#Variables que nos importan
train_hogares = train_hogares %>% select(-P5010, -P5100, -Npersug, -Ingtotug,
                                         -Ingpcug, -Li, -Indigente,
                                         -Nindigentes, -Npobres, -Fex_c, -Fex_dpto) %>%
  rename(numero_cuartos = P5000, tipo_propiedad = P5090, Ingreso_disponible = Ingtotugarr) %>% 
  mutate(valor_arriendo = ifelse(is.na(P5140), P5130,
                                 P5140)) %>% #Creamos una variable que contenga el valor del 
  #arriendo del hogar
  select(-P5130, -P5140)

#Se exporta la base procesada.
saveRDS(train_hogares, paste0(path, "Stores/Procesadas/train_hogares.rds"))

#Se elimina del ambiente para ahorrar ram
rm(train_hogares)

#Se importa la información para las personas.
train_personas = read.csv(paste0(path, "Stores/Pre_procesadas/train_personas.csv")) 




























