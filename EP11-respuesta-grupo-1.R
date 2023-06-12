# Grupo número 1
# Integrante Ángel Bastián Vilches Urrutia
# Integrante Matías Andrés Colil Colil
# Integrante Matías Fernando Yáñez Muñoz

# Librerías
if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}
if (!require(ggpubr)) {
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr)
}
if(!require(tidyr)){
  install.packages("tidyr", dependencies = TRUE)
  require(tidyr)
}
if(!require(ez)){
  install.packages("ez", dependencies = TRUE)
  require(ez)
}


# Obtención de datos
datos <- read.csv2(file.choose(),
                   stringsAsFactors = TRUE)

################################### Grupo 1 ################################### 
################################ Pregunta 1 ###################################
cat("############################ Pregunta 1 ###############################\n")

# Propongan una pregunta de investigación original, que involucre la comparación
# de las medias de dos grupos independientes (más abajo se dan unos ejemplos).
# Fijando una semilla propia, seleccionen una muestra aleatoria de hogares 
# (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.

# Para la resolución de la presente interrogante, la pregunta propuesta es:
# ¿En promedio, el número de hombres y mujeres solteros/as pertenecientes a la
# Región Metropolitana es el mismo?

# La pregunta anteriormente planteada responde a la comparación entre la media
# de dos grupos independientes de personas encuestadas

# A continuación se fija una semilla propia
set.seed(369)

# Se selecciona una muestra aleatoria de hogares considerando: 250 < n < 500
muestra_hogares <- sample_n(datos, 369)

# Se seleccionan los datos de interés según la interrogante propuesta
muestra_hogares <- muestra_hogares %>% select(sexo, region, ecivil)

# Se obtiene los datos referentes a hombres solteros de la Región Metropolitana
# de acuerdo a la muestra obtenida anteriormente
hombres_solteros_rm <- muestra_hogares %>% filter(sexo == "Hombre" &
                                                  ecivil == "Soltero(a)")
# Se obtiene los datos referentes a mujeres solteras de la Región Metropolitana
# de acuerdo a la muestra obtenida anteriormente
mujeres_solteras_rm <- muestra_hogares %>% filter(sexo == "Mujer" &
                                                  ecivil == "Soltero(a)")

################################################################################
# Gráficar
################################################################################

# Se responde a la pregunta propuesta utilizando una simulación Monte Carlo.
# Primeramente, se define la hipótesis nula y alternativa junto con su 
# respectiva notación matemática:
# H0: La media de hombres y mujeres soltero/as pertenecientes a la Región
#     Metropolitana es igual. (μA - μB = 0)
# HA: La media de hombres y mujeres soltero/as pertenecientes a la Región
#     Metropolitana es diferente. (μA - μB != 0)

# En este punto, se identifica la necesidad de utilizar pruebas de 
# permutaciones, específicamente, para comparar una variable continua (media)
# en dos muestras independientes (según contexto de enunciado y pregunta 
# propuesta). Para ello, y según el uso de Monte Carlo se establecen las 
# anteriores hipótesis a contrastar identificando como estadístico la media.
# En segundo lugar, se crea una cantidad de permutaciones, a partir de la 
# muestra original
R <- 9369 # Terminada en 9 para facilitar el computo

n_hombres_solteros_rm <- nrow(hombres_solteros_rm)
n_mujeres_solteras_rm <- nrow(mujeres_solteras_rm)

# Se calcular la diferencia de la media entre las observaciones

dif_observaciones <- mean(n_hombres_solteros_rm) - mean(n_mujeres_solteras_rm)

# Dado que no se sugiere tener especial cuidado con la prueba se establece un
# nivel de significación de 0.05

alfa <- 0.05

# A continuación aplicamos la simulación de Monte Carlo de acuerdo con el nivel
# de significancia anteriormente establecido y la siguiente función dedicada a 
# la realización de permutaciones y junto con ello la obtención de la diferencia
# de medias de entre hombres y mujeres solteros/as de la RM
permutar <- function(i, muestra1, muestra2) {
  n1          <- length(muestra1)
  combinada   <- c(muestra1, muestra2)
  n_comb      <- length(combinada)
  permutacion <- sample(combinada,
                        n_comb,
                        replace = FALSE) # Replace = FALSE indica la no reposición
  nueva_muestra1 <- permutacion[1:n1]
  nueva_muestra2 <- permutacion[(n1 + 1):n_comb]
  return(mean(nueva_muestra1) - mean(nueva_muestra2))
}

# Se hace llamado a la función anteriormente definida para generar la 
# distribución respectiva
dist <- lapply(1:R,
               permutar,
               n_hombres_solteros_rm,
               n_mujeres_solteras_rm)

# Finalmente se calcula ep p-valor
numerador   <- sum((dist) > abs(dif_observaciones)) + 1
denominador <- R + 1
p_valor     <- numerador / denominador

cat("Prueba de hipótesis:")
print(p_valor)

# De acuerdo al p-valor obtenido (0.000106), el cual es significativamente menor
# que el nivel de significancia establecido (0.05), es decir, p-valor < alfa.
# Se rechaza la hipótesis nula en favor de la hipótesis alternativa. Por lo 
# tanto se concluye, con un 95% de confianza, que la media de hombres y mujeres
# soltero/as pertenecientes a la Región Metropolitana es diferente.
rm(dist, hombres_solteros_rm, muestra_hogares, mujeres_solteras_rm, alfa, 
   denominador, dif_observaciones, n_hombres_solteros_rm, n_mujeres_solteras_rm,
   numerador, p_valor, R, permutar)

################################### Grupo 1 ################################### 
################################ Pregunta 2 ###################################
cat("############################ Pregunta 2 ###############################\n")

# Propongan una pregunta de investigación original, que involucre la comparación
# de las medias de más de dos grupos independientes (más abajo se dan unos
# ejemplos). Fijando una semilla distinta a la anterior, seleccionen una muestra
# aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis
# post-hoc con bootstrapping aunque este no sea necesario.

# Para la resolución de la presente interrogante, la pregunta propuesta es:
# ¿En promedio, la edad de las personas es igual en las regiones de 
# Tarapacá, Valparaiso, La Araucanía y Metropolitana de Santiago?

# A continuación se fija una semilla propia distinta a la anterior
set.seed(963)

# Se selecciona una muestra aleatoria de hogares considerando: 400 < n < 600
muestra_hogares <- sample_n(datos, 569)

# Se seleccionan los datos de interés según la interrogante propuesta
muestra_hogares <- muestra_hogares %>% select(region, edad, id.vivienda)
muestra_hogares <- muestra_hogares %>% filter(region == "Region de Tarapaca" |
                                              region == "Region de Valparaiso" |
                                              region == "Region de La Araucania" |
                                              region == "Region Metropolitana de Santiago")
muestra_hogares <- droplevels(muestra_hogares)

# Se obtiene los datos referentes a la región de Tarapacá de acuerdo a la
# muestra obtenida anteriormente
reg_tarapaca <- muestra_hogares %>% filter(region == "Region de Tarapaca")

# Se obtiene los datos referentes a la región de Valparaiso de acuerdo a la
# muestra obtenida anteriormente
reg_valparaiso <- muestra_hogares %>% filter(region == "Region de Valparaiso")

# Se obtiene los datos referentes a la región de La Araucanía de acuerdo a la
# muestra obtenida anteriormente
reg_araucania <- muestra_hogares %>% filter(region == "Region de La Araucania")

# Se obtiene los datos referentes a la región Metropolitana de Santiago de
# acuerdo a la muestra obtenida anteriormente
reg_santiago <- muestra_hogares %>% filter(region == 
                                             "Region Metropolitana de Santiago")

# Se responde a la pregunta propuesta utilizando Bootstrapping
# Primeramente, se define la hipótesis nula y alternativa junto con su 
# respectiva notación matemática:
# H0: La media de las edades es igual en las regiones de Tarapacá, Valparaiso,
#     La Araucanía y Metropolitana de Santiago. (μA = μB = μC = μD)
# HA: La media de las edades es diferente en las regiones de Tarapacá, 
#     Valparaiso, La Araucanía y Metropolitana de Santiago.
#     (μA != μB != μC != μD)

cat("Tamaño de las muestras para la región de Tarapacá:               ", 
    nrow(reg_tarapaca), "\n")
cat("Tamaño de las muestras para la región de Valpararaiso:           ", 
    nrow(reg_valparaiso), "\n")
cat("Tamaño de las muestras para la región de La Araucanía:           ", 
    nrow(reg_araucania), "\n")
cat("Tamaño de las muestras para la región Metropolitana de Santiago: ",
    nrow(reg_santiago), "\n")

# Como se puede observar, el tamaño de los datos difieren entre sí. Por lo 
# tanto, es preciso hacer uso de la ya enseñada prueba de Kruskal-Wallis la 
# cual, es adecuada como alternativa no paramétrica para inferir con más de dos
# muestras independientes (aludiendo a ANOVA de una vía para muestras 
# independientes)

# Adicionalmente, se empleará un nivel de significación que denote mayor cuidado
# en la prueba, esto es, un nivel de significación de 0.01

alfa <- 0.01

# Se crea una cantidad B de muestras nuevas
B <- 9639

# Se obtiene la diferencia de las medias de acuerdo con las observaciones
media_edad_aracania <- mean(reg_araucania[["edad"]])
media_edad_santiago <- mean(reg_santiago[["edad"]])
media_edad_valparaiso <- mean(reg_valparaiso[["edad"]])
media_edad_tarapaca <- mean(reg_tarapaca[["edad"]])
diferencia_observada <- media_edad_aracania - media_edad_santiago -
                        media_edad_tarapaca - media_edad_valparaiso
cat("Diferencia observada: ", diferencia_observada)

# Se obtiene el dataframe anterior de muestras en formato ancho
datos_anchos <- muestra_hogares %>% pivot_wider(names_from = "region",
                                                values_from = "edad")

###############################################################################
# Función que retorna una permutación (bootstrap)
bootstrap <- function(i, df_ancho) {
  n <- nrow(df_ancho)
  indices <- sample(1:n, replace = TRUE)
  df_ancho_boot <- df_ancho[indices, ]
  return(df_ancho_boot)
}

# Obtiene permutaciones
permutaciones <- lapply(1:B, bootstrap, datos_anchos)

F <- function(df_ancho) {
  df_largo <- df_ancho %>%
    pivot_longer(cols = c("Region de La Araucania", "Region de Valparaiso",
                          "Region de Tarapaca", "Region Metropolitana de Santiago"),
                 names_to = "Region",
                 values_to = "Edad")
  df_largo[["Region"]] <- factor(df_largo[["Region"]])
  df_largo[["id.vivienda"]] <- factor(df_largo[["id.vivienda"]])
  anova <- ezANOVA(df_largo, dv = Edad, within = Region, wid = id.vivienda,
                   return_aov = TRUE)
  return(anova[["ANOVA"]][["F"]])
}

distribucion_bootstrap <- sapply(permutaciones, F)

###############################################################################
#rm(muestra_hogares, reg_araucania, reg_santiago, reg_tarapaca, reg_valparaiso,
#   alfa, B)





