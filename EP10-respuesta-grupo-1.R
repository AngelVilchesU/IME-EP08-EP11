# Grupo número 1
# Integrante Ángel Bastián Vilches Urrutia
# Integrante Matías Andrés Colil Colil
# Integrante Matías Fernando Yáñez Muñoz

# Obtención de datos
datos <- read.csv2(file.choose(),
                   stringsAsFactors = TRUE)

# Librerías
if (!require(dplyr)) {
  install.packages("dplyr", dependencies = TRUE)
  require(dplyr)
}

################################### Grupo 1 ################################### 
################################ Pregunta 1 ################################### 

# ¿Existe diferencia en la puntuación dada a la dimensión de usabilidad entre
# los usuarios que jugaron con los diferentes tipos de personaje? De ser así,
# ¿entre qué tipos de personajes existen diferencias?

# Primeramente se observa, de acuerdo a los datos obtenidos, la existencia de
# un total de 7 variables (excluyendo el identificador único de usuario como
# variable numérica discreta) correspondientes a Estética (variables numérica
# discreta) que alude a la percepción obtenida por el usuario respecto de la 
# calidad del producto en una escala de 1 (muy mala) a 5 (muy buena), Usabilidad
# (variables numérica discreta) que alude a la percepción obtenida por el 
# usuario respecto de la usabilidad del producto en una escala de 1 (muy mala)
# a 5 (muy buena), Dificultad (variable numérica discreta) que alude a la 
# percepción obtenida por el usuario respecto de la dificultad de niveles del
# producto en una escala de 1 (muy fácil) a 5 (muy difícil), Personaje 
# (variable categórica nominal) que alude a una cartera de personajes 
# disponibles en el videojuego. Por último, las variables Nivel_10, Nivel_20 y
# Nivel_30 (variables numéricas continuas) que aluden al tiempo (medido en horas)
# que tarda el usuario en completar dichos niveles.

# De la interrogante planteada anteriormente, que expresa una situación donde se
# buscan diferencias entre la dimensión de usabilidad (dada su escala de 
# puntuación, es decir, desde 1 como muy mala a 5 como muy buena) y el personaje
# elegido por el usuario (sea humano, elfo, enano, orco, mago o mediano). Se
# desprende el uso de la prueba ANOVA para muestras independientes.

# En este sentido, se verifican las condiciones necesarias para su uso:
# Primeramente, referente a la primera condición que alude al uso de una escala
# por parte de la variable dependiente cuyas propiedades sean de una escala de
# intervalos iguales. Se puede notar que no se cumple esta condición dado que la
# escala de la variable Usabilidad no cumple con una escala de intervalos 
# iguales, por ejemplo, no se logra observar la diferencia de usabilidad entre
# 5 (muy buena) y 3 cuya diferencia responde a 2, con el par, 3 y 1 cuya 
# diferencia igualmente responde a 2.

# Por lo tanto, se debe seleccionar una medida alternativa para la presente
# prueba. En esta línea, se opta por la prueba de Kruskal-Wallis dado que 
# se presenta un contexto de muestras independientes puesto que los usuarios
# que otorgan su percepción de usabilidad respecto del personaje elegido
# son distintos, y por ende, independientes.

# Ahora bien, se verifica el cumplimiento de condiciones:
# En primer lugar, se cumple que la variable independiente (Personaje) posee al
# menos dos niveles (humano, elfo, enano, orco, mago o mediano).
# En segundo lugar, se cumple que la escala de la variable dependiente
# (Usabilidad) es, al menos, ordinal (posee relación de orden desde 1 como
# muy malo a 5 como muy bueno).
# En tercer y último lugar, se cumple la independencia entre observaciones dado
# un contexto de selección de usuarios reales al azar.

# Se seleccionan los datos de interés para responder a la pregunta:
datos_interes <- datos %>% select(Id, Usabilidad, Personaje)
datos_interes[["Id"]] <- factor(datos_interes[["Id"]])
datos_interes[["Usabilidad"]] <- factor(datos_interes[["Usabilidad"]])

# A continuación se plantea la hipótesis nula y alternativa:
# H0: No existe diferencia en la puntuación dada a la dimensión de usabilidad 
#     (desde 1 como muy malo a 5 como muy bueno) entre los usuarios que jugaron
#     con los diferentes tipos de personaje (sea humano, elfo, enano, orco, mago
#     o mediano).
# HA: Al menos uno de los personajes presenta una usabilidad diferente a al
#     menos otro personaje (sea humano, elfo, enano, orco, mago o mediano).

# Se establece el nivel de significación en 0.05 ya que no se sugiere tener
# especial cuidado en la prueba.
alfa <- 0.05

# Se realiza la prueba de Kruskal-Wallis
prueba_kruskal_wallis <- kruskal.test(datos_interes[["Usabilidad"]] ~
                                        datos_interes[["Personaje"]],
                                      data = datos_interes)
print(prueba_kruskal_wallis)

# De acuerdo con la prueba realizada, se obtiene un p-valor de 0.6138 el cual es
# considerablemente mayor que el nivel de significación establecido. Por lo 
# tanto, se falla en rechazar la hipótesis nula en favor de la hipótesis 
# alternativa. Finalmente, se concluye que, con un 95% de confianza, que no
# existe diferencia en la puntuación dada a la dimensión de usabilidad (desde 1 
# como muy malo a 5 como muy bueno) entre los usuarios que jugaron con los 
# diferentes tipos de personaje (sea humano, elfo, enano, orco, mago o mediano).

# Dado que no se presentan diferencias entre los grupos evaluados en la presente
# prueba, no es necesaria la implementación de pruebas post-hoc dado que no
# se requiere profundizar en diferencias no existentes.

################################### Grupo 1 ################################### 
################################ Pregunta 2 ################################### 

# ¿Existe diferencia en el tiempo que tardan los usuarios que jugaron con un
# orco como personaje en completar los niveles 10, 20 y 30? De ser así, ¿entre
# qué niveles existen diferencias?

# De la interrogante planteada anteriormente, que expresa una situación donde se
# buscan diferencias entre los tiempos que tardan los usuarios (tipo orco) en 
# completar dichos niveles (10, 20 y 30), se desprende el uso de la prueba ANOVA 
# para muestras correlacionadas o pareadas.

# En este sentido, se verifican las condiciones necesarias para su uso:
# Primeramente, en cuanto a la primera condición, se requiere el uso de una escala
# por parte de la variable dependiente con propiedades de una escala de intervalos iguales. 
# Sin embargo, dado que cada jugador tiene habilidades y experiencias diferentes en el juego, 
# no se cumple esta condición, incluso al considerar únicamente los jugadores tipo orco.

# Por lo tanto, se debe seleccionar una medida alternativa para la presente prueba. 
# En esta línea, se opta por la prueba de Friedman, ya que se presenta un contexto 
# de muestras correlacionadas, dado que los tiempos de cada nivel efectuados por 
# jugadores tipo orco hacen alusión a los tiempos de un mismo jugador en cada nivel,
# por ende, son correlacionados.

# Ahora bien, se verifican el cumplimiento de las condiciones:
# En primer lugar, se cumple que la variable independiente debe ser categórica 
# (personaje) y poseer al menos tres niveles (humano, elfo, enano, orco, mago o mediano).
# En segundo lugar, se cumple que la escala de la variable dependiente es, al menos,
# ordinal. Para esta condición, se puede asumir que se cumple debido a que los niveles 
# que tienen un tiempo asociado se representan mediante categorías ordenadas 
# (Nivel 10, Nivel 20, Nivel 30).
# En tercer y último lugar, se cumple la independencia entre observaciones, dado
# un contexto de selección de usuarios reales al azar.

# Se seleccionan los datos de interés para responder a la pregunta:
datos_interes2 <- datos
datos_interes2[["Id"]] <- factor(datos_interes[["Id"]])
datos_interes2[["Personaje"]] <- factor(datos_interes[["Personaje"]])
datos_interes2 <- datos %>% filter(Personaje == "Orco")
datos_interes2 <- datos_interes2 %>% select(Id, Nivel_10, Nivel_20, Nivel_30)

# Se creará una nueva tabla a partir de los datos seleccionados anteriormente
tiempos <- c(datos_interes2$Nivel_10, datos_interes2$Nivel_20, datos_interes2$Nivel_30)
niveles <- c(rep("Nivel 10",length(datos_interes2$Nivel_10)),
              rep("Nivel 20",length(datos_interes2$Nivel_20)),
              rep("Nivel 30",length(datos_interes2$Nivel_30)))
niveles <- factor(niveles)
jugadores <- c(datos_interes2$Id)
nuevos_datos <- data.frame(jugadores, tiempos, niveles)

# A continuación se plantea la hipótesis nula y alternativa:
# H0: No existe diferencia en el tiempo que tardan los usuarios que jugaron con
#     personaje tipo orco en completar los niveles 10, 20 y 30.
# HA: Al menos un usuario presenta diferencias en el tiempo que tarda de acuerdo
#     con quienes jugaron con personaje tipo orco en completar los niveles 10,
#     20 y 30.

# Se establece el nivel de significación en 0.05 ya que no se sugiere tener
# especial cuidado en la prueba.
alfa <- 0.05

# Se realiza la prueba de friedman
prueba_friedman <- friedman.test(tiempos ~ niveles | jugadores, data = nuevos_datos)

print(prueba_friedman)

# De acuerdo con la prueba realizada, se obtiene un p-valor de 3.304e-11 el cual es
# considerablemente menor que el nivel de significación establecido. Por lo 
# tanto, se rechaza la hipótesis nula en favor de la hipótesis alternativa. 
# Finalmente, se concluye que, con un 95% de confianza, que los niveles tienen una
# diferencia significativa en cuanto al tiempo para completarles.

# Dado que se presentaron diferencias entre los grupos evaluados en la presente
# prueba, es necesario la implementación de pruebas post-hoc.

# Se efectuara un procedimiento post-hoc de Holm dado que se encuentraron diferencias
# significativas.

if (prueba_friedman$p.value < alfa) {
  post_hoc <- pairwise.wilcox.test(nuevos_datos$tiempos,
                                   nuevos_datos$niveles,
                                   p.adjust.method = "holm",
                                   paired = TRUE)
  print(post_hoc)
}

# Si prestamos atencion a los resultados, los valores de p son los siguientes:
# La comparación entre Nivel 20 y Nivel 10 tiene un valor de p de 5.6e-09, lo cual
# indica una diferencia estadísticamente significativa.
# La comparación entre Nivel 30 y Nivel 10 también tiene un valor de p de 5.6e-09, 
# lo cual indica una diferencia estadísticamente significativa.
# La comparación entre Nivel 30 y Nivel 20 tiene un valor de p de 0.00012, lo cual
# también indica una diferencia estadísticamente significativa.
# Finalmente, se observa que todos los tiempos asociados a los distintos niveles
# 10, 20 y 30, difieren en cuanto a tiempo para completarles refiere para los
# usuarios que escojieron un orco como su personaje.
