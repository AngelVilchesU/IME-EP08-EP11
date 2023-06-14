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

# Se extrae el número de hombres y mujeres solteras de acuerdo a la muestra
# obtenida
n_hombres_solteros <- nrow(hombres_solteros_rm)
n_mujeres_solteras <- nrow(mujeres_solteras_rm)

# Crear un dataframe con los datos
datos_solteros <- data.frame(Grupo = c("Hombres",
                                       "Mujeres"),
                             Cantidad = c(n_hombres_solteros,
                                        n_mujeres_solteras))

# Se grafican los datos
barplot(datos_solteros[["Cantidad"]],
        names.arg = datos_solteros[["Grupo"]],
        xlab = "Sexo",
        ylab = "Solteros(as)",
        main = "Hombres y Mujeres Solteras en la Región Metropolitana",
        col = c("purple",
                "green"),
        ylim = c(0,
                 max(datos_solteros[["Cantidad"]]) + 10))

# De acuerdo al gráfico elaborado se puede observar una diferencia notable
# entre la cantidad de hombres y mujeres solteros(as) en la Región Metropolitana
# con una concentración en mujeres por sobre los hombres.

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
muestra_hogares <- muestra_hogares %>% select(region, edad)
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

kruskal <- kruskal.test(edad ~ region,
                        data = muestra_hogares)
kruskal_est <- kruskal[["statistic"]]
print(kruskal)

# En este sentido, se logra observar que p (0.004509) < alfa (0.01) por lo que
# preliminarmente nos encontramos en la situación donde se observan diferencias
# entre los grupos en estudio (regiones)

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

# Función que retorna una permutación (bootstrap)
bootstrap <- function(i, df){
  n <- nrow(df)
  remuestreado <- sample(df[["edad"]],
                         n,
                         replace = TRUE) # Replace = TRUE indica reposición
  datos_nuevos <- data.frame(df[["region"]],
                             remuestreado)
  colnames(datos_nuevos) <- colnames(df)
  return(datos_nuevos)
}

# Se realiza el remuestreo correspondiente según bootstrap
remuestreos <- lapply(1:B,
                      bootstrap,
                      muestra_hogares)

# Función que retorna el estadístico según la prueba de Kruskal-Wallis
x2 <- function(df){
  kruskal <- kruskal.test(edad ~ region,
                          data = df)
  return(kruskal[["statistic"]])
}

# Se obtiene la distribución bootstrap
distribucion_bootstrap <- sapply(remuestreos,
                                 x2)

# Finalmente se calcula el p-valor
pvalor <- (sum(abs(distribucion_bootstrap) > abs(kruskal_est)) + 1) / (B + 1)
cat("\n")
print(pvalor)

# Según el p valor obtenido, este es significativamente menor que el nivel de 
# significación establecido anteriormente, p (≈ 0.00311) < alfa (0.01). Por lo 
# tanto, se rechaza la hipótesis nula en favor de la hipótesis alternativa. En
# este sentido, se concluye, con un 99% de confianza, que la media de las edades
# es diferente para al menos una de las regiones, sea la región de Tarapacá,
# Valparaiso, La Araucanía y/o Metropolitana de Santiago.

# A continuación se realiza un análisis post-hoc dado que, por un lado, se 
# solicita como ejercicio académico y, por otro lado, se observan diferencias
# entre el/los grupo/s.

# Recordar que trabajamos con 4 regiones, por lo tanto, abarcamos, 6 pares.
# En este sentido, se realiza la prueba de rangos de Wilcoxon de acuerdo con
# 2 muestras independientes, es decir, cada par (6)

par1 <- wilcox.test(muestra_hogares[muestra_hogares[["region"]] == "Region de Valparaiso",
                                      "edad"],
                      muestra_hogares[muestra_hogares[["region"]] == "Region de La Araucania",
                                      "edad"],
                      alternative = "two.sided",
                      conf.level = 1 - alfa)

par2 <- wilcox.test(muestra_hogares[muestra_hogares[["region"]] == "Region de Valparaiso",
                                    "edad"],
                    muestra_hogares[muestra_hogares[["region"]] == "Region Metropolitana de Santiago",
                                    "edad"],
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

par3 <- wilcox.test(muestra_hogares[muestra_hogares[["region"]] == "Region de Valparaiso",
                                    "edad"],
                    muestra_hogares[muestra_hogares[["region"]] == "Region de Tarapaca",
                                    "edad"],
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

par4 <- wilcox.test(muestra_hogares[muestra_hogares[["region"]] == "Region de La Araucania",
                                    "edad"],
                    muestra_hogares[muestra_hogares[["region"]] == "Region Metropolitana de Santiago",
                                    "edad"],
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

par5 <- wilcox.test(muestra_hogares[muestra_hogares[["region"]] == "Region de La Araucania",
                                    "edad"],
                    muestra_hogares[muestra_hogares[["region"]] == "Region de Tarapaca",
                                    "edad"],
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

par6 <- wilcox.test(muestra_hogares[muestra_hogares[["region"]] == "Region Metropolitana de Santiago",
                                    "edad"],
                    muestra_hogares[muestra_hogares[["region"]] == "Region de Tarapaca",
                                    "edad"],
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

# Se extraen los estadísticos respectivos
est_par1 <- par1[["statistic"]]
est_par2 <- par2[["statistic"]]
est_par3 <- par3[["statistic"]]
est_par4 <- par4[["statistic"]]
est_par5 <- par5[["statistic"]]
est_par6 <- par6[["statistic"]]

# Función que retorna el estadístico según la diferencia de los remuestreos
# con reposición (bootstrap)
obtener_estadistico <- function(df, r1, r2) {
  comp <- wilcox.test(df[df[["region"]] == r1,
                            "edad"],
                         df[df[["region"]] == r2,
                            "edad"],
                         alternative = "two.sided",
                         conf.level = 1 - alfa)
  return(comp[["statistic"]])
}

# Se obtiene las distribuciones según cada par definido anteriormente
dist1 <- sapply(remuestreos,
                obtener_estadistico,
                "Region de Valparaiso",
                "Region de La Araucania")
dist2 <- sapply(remuestreos,
                obtener_estadistico,
                "Region de Valparaiso",
                "Region Metropolitana de Santiago")
dist3 <- sapply(remuestreos,
                obtener_estadistico,
                "Region de Valparaiso",
                "Region de Tarapaca")
dist4 <- sapply(remuestreos,
                obtener_estadistico,
                "Region de La Araucania",
                "Region Metropolitana de Santiago")
dist5 <- sapply(remuestreos,
                obtener_estadistico,
                "Region de La Araucania",
                "Region de Tarapaca")
dist6 <- sapply(remuestreos,
                obtener_estadistico,
                "Region Metropolitana de Santiago",
                "Region de Tarapaca")

# Se calcula el p-valor para cada par correspondiente
denominador <- B + 1

numerador <- sum(abs(dist1) > abs(est_par1)) + 1
p_par1 <- numerador / denominador
cat("Post-hoc Valparaiso - La Araucanía:",p_par1 ,"\n")

numerador <- sum(abs(dist2) > abs(est_par2)) + 1
p_par2 <- numerador / denominador
cat("Post-hoc Valparaiso - Metropolitana:",p_par2 ,"\n")

numerador <- sum(abs(dist3) > abs(est_par3)) + 1
p_par3 <- numerador / denominador
cat("Post-hoc Valparaiso - Tarapacá:",p_par3 ,"\n")

numerador <- sum(abs(dist4) > abs(est_par4)) + 1
p_par4 <- numerador / denominador
cat("Post-hoc La Araucanía - Metropolitana:",p_par4 ,"\n")

numerador <- sum(abs(dist5) > abs(est_par5)) + 1
p_par5 <- numerador / denominador
cat("Post-hoc La Araucania - Tarapacá:",p_par5 ,"\n")

numerador <- sum(abs(dist6) > abs(est_par6)) + 1
p_par6 <- numerador / denominador
cat("Post-hoc Metropolitana - Tarapacá:",p_par6 ,"\n")

# Post-hoc Valparaiso - La Araucanía: 0.4188797 
# Post-hoc Valparaiso - Metropolitana: 0.2852697 
# Post-hoc Valparaiso - Tarapacá: 0.0005186722 
# Post-hoc La Araucanía - Metropolitana: 0.4821577 
# Post-hoc La Araucania - Tarapacá: 0.001452282 
# Post-hoc Metropolitana - Tarapacá: 0.001452282

# De acuerdo a los resultados obtenidos según las pruebas realizadas, se concluye
# que, con un 99% de confianza, la media de las edades es diferente para 
# la región de Tarapacá con respecto a las regiones de Valparaiso, La Araucanía
# y Metropolitana de Santiago (es decir, 3 pares).