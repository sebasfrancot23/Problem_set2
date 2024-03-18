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

libraries = c("tidyverse", "skimr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)



# Importación hogares -----------------------------------------------
list.files(path = paste0(path,"Stores/Pre_procesadas/"))

#Se importa la información para el train de los hogares.
train_hogares = read.csv(paste0(path, "Stores/Pre_procesadas/train_hogares.csv"))

#Variables que nos importan
train_hogares = train_hogares %>% 
  select(id, Clase, Dominio, P5000, P5090, P5130, P5140, Nper, Ingtotugarr, 
         Lp, Pobre, Depto) %>%
  rename(numero_cuartos = P5000, tipo_propiedad = P5090, Ingreso_disponible = Ingtotugarr) %>% 
  mutate(valor_arriendo = ifelse(is.na(P5140), P5130,
                                 P5140)) %>% #Creamos una variable que contenga el valor del 
  #arriendo del hogar.
  select(-P5130, -P5140)

#Se exporta la base procesada.
saveRDS(train_hogares, paste0(path, "Stores/Procesadas/train_hogares.rds"))

#Se elimina del ambiente para ahorrar ram
rm(train_hogares)

#Ahora lo mismo para el test de hogares.
test_hogares = read.csv(paste0(path, "Stores/Pre_procesadas/test_hogares.csv"))

#Variables que nos importan
test_hogares = test_hogares %>% 
  select(id, Clase, Dominio, P5000, P5090, P5130, P5140, Nper, Lp, Depto) %>%
  rename(numero_cuartos = P5000, tipo_propiedad = P5090) %>% 
  mutate(valor_arriendo = ifelse(is.na(P5140), P5130,
                                 P5140)) %>% #Creamos una variable que contenga 
  #el valor del arriendo del hogar.
  select(-P5130, -P5140)

#Se exporta la base procesada.
saveRDS(test_hogares, paste0(path, "Stores/Procesadas/test_hogares.rds"))

#Se elimina del ambiente para ahorrar ram
rm(test_hogares)


# Importación personas ----------------------------------------------------
#Se importa la información para las personas.
train_personas = read.csv(paste0(path, "Stores/Pre_procesadas/train_personas.csv")) 

#Se conservan las variables de interés
train_personas = train_personas %>% 
  select(id, Orden, Clase, Dominio, Estrato1, P6020, P6040, P6090, P6210, 
        P6426, P6430, P6920, P6800, P6870, P7495, P7505,  
        Pet, Oc, Des, Ina, Oficio) %>%
  rename(Sexo = P6020, Edad = P6040, Afiliado_SS = P6090, maxEduc = P6210, 
        antiguedad_puesto = P6426, Posicion_actual = P6430, 
        Cotiza_pension = P6920, Tiempo_trabajo_promedio = P6800, 
        Numero_trabajadores = P6870, Ingreso_arriendos_pension = P7495,
        Otros_ingresos = P7505)

#Se limpian los datos
train_personas = train_personas %>%
    #Primero se recodifican las variables dicótomas en 0 y 1.
  mutate(Afiliado_SS = ifelse(Afiliado_SS==2, 0, Afiliado_SS)) %>%
  mutate(Cotiza_pension = ifelse(Cotiza_pension==2, 0, Cotiza_pension)) %>%
  #Si es un 9 es porque no sabe. Esa gente de qué nos va a servir. 
  mutate(Afiliado_SS = ifelse(Afiliado_SS==9, NA, Afiliado_SS))

#Cómo se identifica un hogar en la base de datos? Por id, fíjate que el número de
#valores únicos de id no es el mismo que el número de filas de la base.
length(table(train_personas$id)) == dim(train_personas)[1] #Dio falso.

# Creación variables por hogar. -------------------------------------------
#Por cada hogar de la muestra de personas se crean las siguientes variables.

aux = train_personas %>% group_by(id) %>%
  #Se promedia la edad del hogar.
  mutate(Edad_promedio = mean(Edad)) %>% 
  #El grado de educación con el que cuenta el mayor número de personas del hogar.
  mutate(maxEduc_hogar = as.numeric(names(which.max(table(maxEduc))))) %>%
  #Cambios menores
  select(id, Edad_promedio, maxEduc_hogar) %>%
  distinct()

aux_antiguedad = train_personas %>% filter(Pet==1) %>%
  #Si la persona está desocupada o inactiva, antiguedad es cero.
  mutate(antiguedad_puesto = ifelse(!is.na(Oc), antiguedad_puesto, 0)) %>%
  group_by(id) %>%  #Se promedia la antiguedad en el puesto actual de trabajo
  mutate(antiguedad_puesto_promedio = mean(antiguedad_puesto)) %>%
  select(id, antiguedad_puesto_promedio) %>% distinct()
  
aux_menores = train_personas %>% filter(Edad<18 & is.na(Oc)) %>%
  group_by(id) %>% #Cuántas personas hay en el hogar se deben mantener (menores de 
  #18 años y que que no trabajen)
  mutate(menores = n()) %>% select(id, menores) %>% distinct()

#la variable con el tipo de oficio al que más se dedican en el hogar, y el tipo
#de posición en el trabajo (ver variable Oficio y p6430).
aux_Pet = train_personas %>% filter(Pet==1) %>% 
  #Si Oficio es NA es porque no está ocupado.
  mutate(Oficio = ifelse(is.na(Oficio), 100, Oficio)) %>%
  #Si Posicion_actual es NA es porque está desocupado
  mutate(Posicion_actual = ifelse(is.na(Posicion_actual), 0, 
                                  Posicion_actual)) %>%
  group_by(id) %>%
  #La moda
  mutate(Oficio_hogar = as.numeric(names(which.max(table(Oficio))))) %>%
  mutate(Posicion_hogar = as.numeric(names(which.max(table(Posicion_actual))))) %>%
  #Número de personas en edad de trabajar.
  mutate(total_Pet = n()) %>% 
  select(id, total_Pet, Oficio_hogar, Posicion_hogar, Numero_afilidos) %>% 
  distinct() 

aux_Oc = train_personas %>% filter(Pet==1 & !is.na(Oc)) %>% group_by(id) %>% 
  mutate(total_Oc = n()) %>% 
  select(id, total_Oc) %>% distinct() 

#En una sola base.
aux_prop = merge(aux_Pet, aux_Oc, by = "id", all = T)
#Fíjese que hay hogares que no tienen personas ocupadas a pesar que sí tengan 
#personas en edad de trabajar. Acá se reemplaza el NA por cero.
aux_prop = aux_prop %>% mutate(total_Oc = ifelse(is.na(total_Oc), 0, total_Oc))

#Se crea la variable de la proporción del número de personas ocupadas en el hogar
#en relación al número de personas en edad de trabajar.
aux_prop$Pet_Oc_prop = aux_prop$total_Oc/aux_prop$total_Pet 
  
#Revísate el word por qué este cambio.
aux_prop = aux_prop[,-4] #Quite la Posición_actual por hogar.

  
aux = train_personas %>% filter(Pet==1) 

%>% group_by(id) %>%
  summarize(count_x_1 = sum(Afiliado_SS == 1, na.rm = TRUE))



























