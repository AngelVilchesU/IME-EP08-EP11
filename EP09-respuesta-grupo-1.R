# Obtención de datos
datos <- read.csv2(file.choose(),
                   stringsAsFactors = TRUE)

# Librerías
if(!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}
if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr)
}
if(!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse)
}
if(!require(ez)){
  install.packages("ez", dependencies = TRUE)
  require(ez)
}

################################### Grupo 1 ################################### 
# Determine si, para el grupo 8, existen diferencias significativas en el peso
# de los gatos para los distintos tipos de alimentos.

# Primeramente se observa, de acuerdo a los datos obtenidos, la existencia de
# un total de 7 variables (excluyendo el identificador único de gato como
# variable numérica discreta) correspondientes a Grupo (variables numérica
# discreta) que determina un orden de consumo según el alimento asociado,
# Inicial (variable numérica continua) aludiendo al peso del gato (en
# kilogramos) previo al comienzo del experimento, Pollo (variable numérica
# continua) aludiendo al peso del gato (en kilogramos) tras un mes alimentandose
# con pollo, Res (variable numérica continua) aludiendo al peso del gato (en
# kilogramos) tras un mes alimentandose con res, Conejo (variable numérica
# continua) aludiendo al peso del gato (en kilogramos) tras un mes alimentandose
# con conejo, Pescado (variable numérica continua) aludiendo al peso del gato 
# (en kilogramos) tras un mes alimentandose con pescado y finalmente, la
# variables Pato (variable numérica continua) aludiendo al peso del gato (en
# kilogramos) tras un mes alimentandose con pato.

# Para la presente prueba se realizará una prueba ANOVA de una vía para muestras
# correlaciondas dado que se observa y/o plantea una interrelación entre las
# variables refiriendo al peso (en kilogramos) las cuales varían según el
# alimento (sea pollo, res, conejo, pescado o pato), y grupo (grupo 8 en este
# caso), asignado a lo largo del tiempo (1 mes). En este sentido, destacar
# que se alude al escenario que implica diseño con medidas repetidas dado 
# el registro de medidas (peso en kilogramos) en diferentes condiciones
# (alimentos) bajo un tiempo compartido (1 mes).

# A continuación se plantea la hipótesis nula y alternativa junto con la
# respectiva notación matemática:
# H0: El peso promedio de los gatos pertenecientes al grupo 8 es igual para 
#     todos los tipos de alimento, es decir, sea pollo, res, conejo, pescado o 
#     pato. (μ1 = μ2 = μ3 = μ4 = μ5)
# HA: El peso promedio de los gatos pertenecientes al grupo 8 es distinto para 
#     al menos uno de los tipos de alimento, es decir, sea pollo, res, conejo,
#     pescado o pato. (μ1 != μ2 != μ3 != μ4 != μ5)

# Se filtran los datos de interés:
datos <- datos %>% filter(Grupo == 8)
datos <- datos %>% select(Id, Inicial, Pollo, Res, Conejo, Pescado, Pato)
datos <- droplevels(datos)

# Convertir los datos (dataframe) a formato largo:
datos <- datos %>% pivot_longer(c("Inicial",
                                  "Pollo",
                                  "Res",
                                  "Conejo",
                                  "Pescado",
                                  "Pato"),
                                names_to = "Alimento",
                                values_to = "Peso")
datos[["Id"]] <- factor(datos[["Id"]])
datos[["Alimento"]] <- factor(datos[["Alimento"]])

# Se procede con la verificación de condiciones para el uso de ANOVA para 
# muestras correlacionadas:
# En primer lugar, la variable dependiente, es decir, el peso
# de los gatos se mide utilizando una escala de intervalos iguales dada su
# proveniencia a partir de una escala de razón cuyo origen es en un cero
# verdadero. Por lo tanto, es razonable asumir que se cumple la propiedad de
# una escala de intervalos iguales.
# En segundo lugar, se confirma la aleatoriedad de las muestras obtenidas dado
# un origen de selección al azar e independiente de acuerdo a la asignación
# de gatos a un grupo establecido previamente.
# En tercer lugar, se realiza un gráfico Q-Q para confirmar el cumplimiento
# de la tercera condición:
graficoQQ <- ggqqplot(datos,
                      x = "Peso" ,
                      y = "Alimento",
                      color = "Alimento")
graficoQQ <- graficoQQ + facet_wrap(~ Alimento)
graficoQQ <- graficoQQ + rremove("x.ticks") + rremove("x.text")
graficoQQ <- graficoQQ + rremove("y.ticks") + rremove("y.text")
graficoQQ <- graficoQQ + rremove("axis.title")
print(graficoQQ)

# De acuerdo al gráfico Q-Q es posible observar que no se presentan valores
# atípicos en los modelos, por ello, se determina emplear un nivel de
# significación de 0.05
alfa <- 0.05
# Finalmente y en cuarto lugar, se implementa la prueba ANOVA con la función
# ezANOVA (del paquete ez) que permitiría efectuar la prueba en paralelo con
# la comprobación de la cuarta condición de acuerdo a la esfericidad de la 
# matriz de varianzas-covarianzas.
prueba_ANOVA <- ezANOVA(data = datos,
                        dv = Peso,
                        within = Alimento,
                        wid = Id,
                        return_aov = TRUE)
print(prueba_ANOVA)

# Según el valor p obtenido por la prueba (4.486503e-10), específicamente la
# prueba de esfericidad de Mauchly se puede apreciar que no se cumple con la
# condición de esfericidad. Por lo tanto, se debe realizar un proceso de 
# corrección. En este sentido, se puede observar que según el proceso de 
# corrección de esfericidad (Sphericity Corrections) el valor obtenido por 
# la estimación de Greenhouse-Geisser (GGe) corresponde a 0.6068348, valor
# que es menor a 0.75 lo cual implica, según el texto base de la presente 
# asignatura que es apropiado para la presente prueba. Por ello, el p-valor
# asociado (p[GG]) correspondiente a 1.680269e-26 será el p-valor de la 
# prueba ANOVA para muestras correlacionadas. Por lo cual, al ser p-valor <
# (1.680269e-26) nivel de significancia establecido (0.05), se rechaza la
# la hipótesis nula en favor de la hipótesis alternativa. De modo que,
# se concluye, con un 95% de confianza, que el peso promedio de los gatos
# pertenecientes al grupo 8 es distinto para al menos uno de los tipos de
# alimento, es decir, sea pollo, res, conejo, pescado o pato.

# Al presentarse diferencias entre los grupos analizados, es pertinente
# emplear el procedimiento Post-hoc con el propósito de realizar una comparación
# más precisa y detallada entre los grupos.
# En este caso, se opta por realizar la corrección de Holm ya que es más liberal
# en contraste con Bonferroni la cual es más conservadora, y por ende, no
# recomendada para muchos grupos.
holm <- pairwise.t.test(datos[["Peso"]],
                        datos[["Alimento"]],
                        p.adjust.method = "holm",
                        paired = TRUE)
print(holm)

# De acuerdo a los resultados obtenidos se puede apreciar que los pares de
# tipos de alimento Conejo-Pato, Conejo-Res, Pato-Pescado, Pato-Pollo, Pato-Res,
# Pescado-Res y Pollo-Res presentan una diferencia significativa al comparar el
# valor p ajustado otorgado por la corrección de Holm con el nivel de
# significación establecido (alfa = 0.05).