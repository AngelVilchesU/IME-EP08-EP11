# Obtención de datos
datos <- read.csv2(file.choose(),
                   stringsAsFactors = TRUE)
datos[["id"]] <- factor(datos[["id"]])

# Librerías
if(!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE)
  require(ggpubr)
}
if(!require(ez)){
  install.packages("ez", dependencies = TRUE)
  require(ez)
}

################################### Grupo 1 ################################### 
# En este momento, los agrónomos buscan determinar si existen diferencias en
# el peso a la semana 10 de crecimiento entre las manzanas Richard, Pink Lady,
# Golden y Fuji.

# Primeramente se observa, de acuerdo a los datos obtenidos, la existencia de
# dos variables categóricas: variedad (según tipo) y tiempo (en semanas). Por 
# otro lado, el peso (en gramos) figura como variable numérica, específicamente,
# continua al adoptar cualquier valor dentro de los reales y un intervalo.
# En este sentido, considerar el uso de ANOVA de una vía para muestras 
# independientes dado un contexto que involucra muestras de manzanas
# provenientes de un árbol distinto con un peso propio que no mantiene 
# dependencia entre las observaciones de una manzana y otra.

# A continuación se plantea la hipótesis nula y alternativa junto con la
# respectiva notación matemática:
# H0: El peso, a la décima semana de crecimiento, entre las manzanas Richard,
#     Pink Lady, Golden y Fuji es igual para toda la variedad o grupo de
#     manzanas. (μ1 = μ2 = μ3 = μ4)
# HA: El peso, a la décima semana de crecimiento, entre las manzanas Richard,
#     Pink Lady, Golden y Fuji es diferente para al menos una variedad o grupo
#     de manzanas. (μ1 != μ2 != μ3 != μ4)

# Se filtran los datos de interés:
datos <- datos %>% filter(tiempo == "semana_10")
datos <- datos %>% filter(variedad == "Richard" |
                          variedad == "Pink Lady" |
                          variedad == "Golden" |
                          variedad == "Fuji")
datos <- droplevels(datos)

# Se procede con la verificación de condiciones para el uso de ANOVA:
# En primer lugar, la variable dependiente, es decir, el peso de las manzanas
# se mide utilizando una escala de intervalos iguales dada su proveniencia
# a partir de una escala de razón cuyo origen es en un cero verdadero. Por lo
# tanto, es razonable asumir que se cumple la propiedad de una escala de 
# intervalos iguales.
# En segundo lugar, se confirma la aleatoriedad de las muestras obtenidas dado
# un origen de selección al azar e independiente.
# En tercer lugar, se realiza un gráfico Q-Q para confirmar el cumplimiento
# de la tercera condición:
graficoQQ <- ggqqplot(datos,
                      x = "peso" ,
                      y = "variedad" ,
                      color = "variedad")


graficoQQ <- graficoQQ + facet_wrap(~ variedad)
graficoQQ <- graficoQQ + rremove("x.ticks") + rremove("x.text")
graficoQQ <- graficoQQ + rremove("y.ticks") + rremove("y.text")
graficoQQ <- graficoQQ + rremove("axis.title")
print(graficoQQ)

# De acuerdo al gráfico Q-Q es posible observar algunos pocos valores atípicos
# en los extremos del modelo, por ello, se procede con cautela y se determina
# emplear un nivel de significación de 0.025
alfa <- 0.025
# Finalmente y en cuarto lugar, se implementa la prueba ANOVA con la función
# ezANOVA (del paquete ez) que permitiría efectuar la prueba en paralelo con
# la comprobación de la cuarta condición de acuerdo a la prueba de 
# homocedasticidad.

prueba_ANOVA <- ezANOVA(data = datos,
                        dv = peso,
                        between = variedad,
                        wid = id,
                        return_aov = TRUE)
print(prueba_ANOVA)
print(summary(prueba_ANOVA[["aov"]]))

# De acuerdo a los resultados obtenidos, se observa primeramente, que el p-valor
# es notablemente mayor al nivel de significación establecido. Por lo tanto, 
# se falla en rechazar la hipótesis nula de la presente prueba, es decir, se
# tiene un 97,5% de confianza en el cumplimiento de la cuarta condición
# correspondiente a homocedasticidad.

# Finalmente, se observa en la prueba ANOVA que el valor p obtenido (0.1914) es
# considerablemente mayor que el nivel de significación establecido (0.025).
# Por lo tanto, se falla en rechazar la hipótesis nula en favor de la 
# alternativa. En consecuencia, se concluye, con un 97,5% de confianza, que
# el peso, a la décima semana de crecimiento, entre las manzanas Richard,
# Pink Lady, Golden y Fuji es igual para toda la variedad o grupo de manzanas.

# En este caso, no es necesario realizar un análisis post-hoc dado que no 
# se observan diferencias entre los grupos analizados y por lo tanto no se 
# requiere de mayores detalles respecto del hallazgo de diferencias globales
# entre los grupos dado que no se registran en el análisis de varianza (ANOVA).