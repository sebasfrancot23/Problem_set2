###################################
#
# Programación problem set 2
# Importación y limpieza de datos.
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
  select(id, Clase, Dominio, P5000, P5090, P5130, P5140, Ingtotugarr, 
         Lp, Depto, Npersug) %>%
  rename(numero_cuartos = P5000, tipo_propiedad = P5090, Ingreso_disponible = Ingtotugarr) %>% 
  mutate(valor_arriendo = ifelse(is.na(P5140), P5130,
                                 P5140)) %>% #Creamos una variable que contenga el valor del 
  #arriendo del hogar.
  #Con base en el comentario de Ignacio, creamos una nueva variable indicadora de 
  #pobreza.
  mutate(Pobre = ifelse(Ingreso_disponible<Lp*Npersug, 1, 0)) %>%
  select(-P5130, -P5140)

#Se exporta la base procesada.
saveRDS(train_hogares, paste0(path, "Stores/Procesadas/train_hogares.rds"))

#Se elimina del ambiente para ahorrar ram
rm(train_hogares)

#Ahora lo mismo para el test de hogares.
test_hogares = read.csv(paste0(path, "Stores/Pre_procesadas/test_hogares.csv"))

#Variables que nos importan
test_hogares = test_hogares %>% 
  select(id, Clase, Dominio, P5000, P5090, P5130, P5140, Npersug, Lp, Depto) %>%
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

#Se define una función auxiliar para limpiar la base de las personas.
limpieza_personas = function(DB){
  
  #Se conservan las variables de interés
  DB = DB %>% 
    select(id, Orden, Clase, Dominio, P6020, P6040, P6090, P6210, 
           P6426, P6430, P6920, P6800, P6870, P7495, P7505,  
           Pet, Oc, Des, Ina, Oficio) %>%
    rename(Sexo = P6020, Edad = P6040, Afiliado_SS = P6090, maxEduc = P6210, 
           antiguedad_puesto = P6426, Posicion_actual = P6430, 
           Cotiza_pension = P6920, Tiempo_trabajo_promedio = P6800, 
           Numero_trabajadores = P6870, Ingreso_arriendos_pension = P7495,
           Otros_ingresos = P7505)
  
  #Se limpian los datos
  DB = DB %>%
    #Primero se recodifican las variables dicótomas en 0 y 1.
    mutate(Afiliado_SS = ifelse(Afiliado_SS==2, 0, Afiliado_SS)) %>%
    mutate(Cotiza_pension = ifelse(Cotiza_pension==2, 0, Cotiza_pension)) %>%
    #Si es un 9 es porque no sabe. Esa gente de qué nos va a servir. 
    mutate(Afiliado_SS = ifelse(Afiliado_SS==9, NA, Afiliado_SS))
  
  # Creación variables por hogar. -------------------------------------------
  
  
  #Por cada hogar de la muestra de personas se crean las siguientes variables.
  aux_DB = DB %>% group_by(id) %>%
    #Se promedia la edad del hogar.
    mutate(Edad_promedio = mean(Edad)) %>%
    #El grado de educación con el que cuenta el mayor número de personas del hogar.
    mutate(maxEduc_hogar = as.numeric(names(which.max(table(maxEduc))))) %>%
    #Cambios menores
    select(id, Edad_promedio, maxEduc_hogar) %>%
    distinct()
  
  aux_menores = DB %>% filter(Edad<18 & is.na(Oc)) %>%
    group_by(id) %>% #Cuántas personas hay en el hogar se deben mantener (menores de
    #18 años y que que no trabajen)
    mutate(menores = n()) %>% select(id, menores) %>% distinct()
  
  #Se unen ambas bases.
  aux_DB = merge(aux_DB, aux_menores, by = "id", all = T)
  
  #Cuando menores es NA es porque ese hogar no tiene ningún menor de 18 años.
  aux_DB = aux_DB %>% mutate(menores = ifelse(is.na(menores),
                                              0, menores))
  
  #Se calculo el tiempo promedio en el puesto actual por hogar.
  aux_antiguedad = DB %>% filter(Pet==1) %>%
    #Si la persona está desocupada o inactiva, antiguedad es cero.
    mutate(antiguedad_puesto = ifelse(!is.na(Oc), antiguedad_puesto, 0)) %>%
    group_by(id) %>%  #Se promedia la antiguedad en el puesto actual de trabajo
    mutate(antiguedad_puesto_promedio = mean(antiguedad_puesto)) %>%
    select(id, antiguedad_puesto_promedio) %>% distinct()
  
  aux_DB = merge(aux_DB, aux_antiguedad, by = "id", all = T)
  
  #la variable con el tipo de oficio al que más se dedican en el hogar, y el tipo
  #de posición en el trabajo que más ocurre en un hogar
  #(ver variable Oficio y p6430).
  aux_Pet = DB %>% filter(Pet==1) %>%
    #Si Oficio es NA es porque no está ocupado.
    mutate(Oficio = ifelse(is.na(Oficio), 100, Oficio)) %>%
    #Si Posicion_actual es NA es porque está desocupado (o eso creía).
    mutate(Posicion_actual = ifelse(is.na(Posicion_actual), 0,
                                    Posicion_actual)) %>%
    #Si Tiempo_trabajo_promedio es NA es porque el individuo no está empleado,
    #se reemplaza por un cero.
    mutate(Tiempo_trabajo_promedio = ifelse(is.na(Tiempo_trabajo_promedio), 0,
                                            Tiempo_trabajo_promedio)) %>%
    group_by(id) %>%
    #La moda
    mutate(Oficio_hogar = as.numeric(names(which.max(table(Oficio))))) %>%
    mutate(Posicion_hogar = as.numeric(names(which.max(table(Posicion_actual))))) %>%
    #Horas trabajadas en promedio por hogar.
    mutate(Tiempo_trabajo_hogar = mean(Tiempo_trabajo_promedio)) %>%
    #Número de personas en edad de trabajar.
    mutate(total_Pet = n()) %>%
    select(id, total_Pet, Oficio_hogar, Posicion_hogar, Tiempo_trabajo_hogar) %>%
    distinct()
  
  aux_Oc = DB %>% filter(Pet==1 & !is.na(Oc)) %>% group_by(id) %>%
    #Número de personas ocupadas por hogar.
    mutate(total_Oc = n()) %>%
    #Se calcula el tamaño de la empresa en el cual trabaja el mayor número de
    #personas en el hogar.
    mutate(Tamaño_empresa_hogar=
             as.numeric(names(which.max(table(Numero_trabajadores))))) %>%
    select(id, total_Oc, Tamaño_empresa_hogar) %>% distinct()
  
  #En una sola base.
  aux_prop = merge(aux_Pet, aux_Oc, by = "id", all = T)
  
  #Fíjese que hay hogares que no tienen personas ocupadas a pesar que sí tengan
  #personas en edad de trabajar. Acá se reemplaza el NA por cero.
  aux_prop = aux_prop %>% mutate(total_Oc = ifelse(is.na(total_Oc), 0, total_Oc))
  
  #Lo mismo pasa con Tamaño_empresa, así que creamos una nueva categoría que indica
  #si ese hogar trabaja o no.
  aux_prop = aux_prop %>% mutate(Tamaño_empresa_hogar =
                                   ifelse(is.na(Tamaño_empresa_hogar), 0,
                                          Tamaño_empresa_hogar))
  
  #Se crea la variable de la proporción del número de personas ocupadas en el hogar
  #en relación al número de personas en edad de trabajar.
  aux_prop$Pet_Oc_prop = aux_prop$total_Oc/aux_prop$total_Pet
  
  #Revísate el word por qué este cambio.
  aux_prop = aux_prop[,-4] #Quite la Posición_actual por hogar.
  
  aux_DB = merge(aux_DB, aux_prop, by = "id", all = T)
  
  #Se crean tres variables que cuentan cuántas personas en el hogar están afiliadas
  #a SS, cuántas reciben ingresos por pensión y/o arriendos y cuántas reciben 
  #otros ingresos(variable P7505).
  aux_dicotomas = DB %>% filter(Pet==1) %>% group_by(id) %>%
    summarize(Afiliado_SS = sum(Afiliado_SS == 1, na.rm = T),
              Ingreso_arriendos_pension = sum(Ingreso_arriendos_pension==1,
                                              na.rm = T),
              Otros_ingresos = sum(Otros_ingresos==1, na.rm = T)) 
  
  aux_DB = merge(aux_DB, aux_dicotomas, by = "id", all = T)
  
  return(aux_DB)
}

#Se importa la información para las personas.
#Primero el train.
train_personas = read.csv(paste0(path, "Stores/Pre_procesadas/train_personas.csv")) 

#Cómo se identifica un hogar en la base de datos? Por id, fíjate que el número de
#valores únicos de id no es el mismo que el número de filas de la base.
length(table(train_personas$id)) == dim(train_personas)[1] #Dio falso.

#Se corre la función
train_personas = limpieza_personas(train_personas)

#Se exporta la base procesada.
saveRDS(train_personas, paste0(path, "Stores/Procesadas/train_personas.rds"))

#Se elimina del ambiente para ahorrar ram
rm(train_personas)

#Ahora con el test
test_personas = read.csv(paste0(path, "Stores/Pre_procesadas/test_personas.csv")) 

test_personas = limpieza_personas(test_personas)
saveRDS(test_personas, paste0(path, "Stores/Procesadas/test_personas.rds"))
rm(test_personas)


# Unión bases de datos ----------------------------------------------------
#Primero el train.
train_hogares = readRDS(paste0(path, "Stores/Procesadas/train_hogares.rds"))
train_personas = readRDS(paste0(path, "Stores/Procesadas/train_personas.rds"))

#Se juntan.
train_final = merge(train_hogares, train_personas, by = "id", all = T)

#Solo hay un hogar con nivel de educación 9. Más adelante va a ser problemático
#en la clasificación, así que se elimina.
train_final = train_final %>%
  filter(maxEduc_hogar!=9)

#Algunas variable son cualitativas, se vuelven factores.
factores = function(DB){
  DB = DB %>% 
    mutate(tipo_propiedad = factor(tipo_propiedad)) %>%
    mutate(Depto= factor(Depto)) %>%
    mutate(maxEduc_hogar= factor(maxEduc_hogar)) %>%
    mutate(Oficio_hogar= factor(Oficio_hogar)) %>%
    mutate(Tamaño_empresa_hogar= factor(Tamaño_empresa_hogar))%>%
  #También, se imputa el valor 98 o 99 del arriendo por la media.
    mutate(valor_arriendo = ifelse(valor_arriendo==98 | valor_arriendo==99,
                                   mean(valor_arriendo), valor_arriendo))

  return(DB)
}

train_final = factores(train_final)

#Se exporta.
saveRDS(train_final, paste0(path, "Stores/train_final.rds"))

#Ahora el test.
test_hogares = readRDS(paste0(path, "Stores/Procesadas/test_hogares.rds"))
test_personas = readRDS(paste0(path, "Stores/Procesadas/test_personas.rds"))

#Se juntan.
test_final = merge(test_hogares, test_personas, by = "id", all = T)
#Solo hay un hogar con nivel de educación 9. Más adelante va a ser problemático
#en la clasificación, así que se elimina.
test_final = test_final %>%
  filter(maxEduc_hogar!=9)

#La función.
test_final = factores(test_final)



#Se exporta.
saveRDS(test_final, paste0(path, "Stores/test_final.rds"))











