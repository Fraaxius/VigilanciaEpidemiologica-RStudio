# GUIA DE EJERCICIOS DPLYR

###############
# EJERCICIO 1 #
###############

install.packages('dplyr')
install.packages('haven')
library('dplyr')
library('haven')

# cargar y/o instalar dplyr
# cargar  y/o instalar haven

# IMPORTAR DATOS
data <- readRDS(url("https://github.com/JoseRTM/Clases-R/raw/main/jovenes.rds"))
View(data)

###############
# EJERCICIO 2 #
###############

# seleccionar y renombrar las variables a utilizar
# EDAD = edad
# SEXO = sexo
# P76_1 = alcohol
# P76_3 = marihuana
# P76_4 = cocaina
# P76_5 = pbc
# P91 = aborto

datos <- data %>%
  select(
    Edad = EDAD,
    Sexo = SEXO,
    Alcohol = P76_1,
    Marihuana = P76_3,
    Cocaina = P76_4,
    PBC = P76_5,
    Aborto = P91
  )
head(datos)

###############
# EJERCICIO 3 #
###############

# Generar la variable tramo_etario:
# tramo 1: menor a 18
# tramo 2: entre 18 y 22
# tramo 3: entre 23 y 26
# tramo 4: mayor a 26

datos <- datos %>%
  mutate(tramo_etario = case_when(
    Edad < 18 ~ "Tramo 1",
    Edad >= 18 & Edad <= 22 ~ "Tramo 2",
    Edad >= 23 & Edad <= 26 ~ "Tramo 3",
    Edad > 26 ~ "Tramo 4",
  ))

###############
# EJERCICIO 4 #
###############

# limpiar las variables asignando NA, puede utilizar na_if


# Reemplazar datos 99

datos <- datos %>%
  mutate(across(where(is.numeric), ~ na_if(., 99)))

###############
# EJERCICIO 5 #
###############

# Asignar los labels a las categorías utilizando la funcion factor()

SiNo <- function(x) {
  x <- factor(x, levels = c(1, 2), labels = c("Sí", "No"))
}

Sexo <- function(x) {
  x <- factor(x, levels = c(1, 2), labels = c("Hombre", "Mujer"))
}

datos <- datos %>%
  mutate(across(-c(Edad, tramo_etario, Sexo), SiNo))

datos <- datos %>%
  mutate(Sexo = Sexo(Sexo))

###############
# EJERCICIO 6 #
###############

# Desarrollar los ejercicios 2, 3, 4 y 5 en una misma linea de código, es decir,
# se deben hacer todas esas tareas solo en una asignacion (<-)

SiNo <- function(x) {
  factor(x, levels = c(1, 2), labels = c("Sí", "No"))
}

Sexo <- function(x) {
  factor(x, levels = c(1, 2), labels = c("Hombre", "Mujer"))
}

datos <- data %>%
  select(
    Edad = EDAD,
    Sexo = SEXO,
    Alcohol = P76_1,
    Marihuana = P76_3,
    Cocaina = P76_4,
    PBC = P76_5,
    Aborto = P91
  ) %>%
  mutate(
    tramo_etario = case_when(
      Edad < 18 ~ "Tramo 1",
      Edad >= 18 & Edad <= 22 ~ "Tramo 2",
      Edad >= 23 & Edad <= 26 ~ "Tramo 3",
      Edad > 26 ~ "Tramo 4"
    )
  ) %>%
  mutate(across(where(is.numeric), ~ na_if(., 99))) %>%
  mutate(across(-c(Edad, tramo_etario, Sexo), SiNo)) %>%
  mutate(Sexo = Sexo(Sexo))

###############
# EJERCICIO 7 #
###############

# apoyandose en los datos, definir si las siguientes afirmaciones son verdaderas o falsas:

# 1) De las mujeres que han consumido pbc, la mayoría ha realizado un aborto.

mujeres_pbc <- datos %>%
  filter(PBC == "Sí", Sexo == "Mujer") %>% 
  count(Aborto)
mujeres_pbc

# Sí, verdadero... Hay 3 mujeres que si han realizado abortos, mientras que 2
# no han realizado abortos.

# 2) Las personas que han consumido alcohol son en promedio mas jovenes que las que no han consumido.

edad_promedio <- datos %>%
  group_by(Alcohol) %>%
  summarise(edad_promedio = mean(Edad, na.rm =T))
edad_promedio

# Verdadero, ya que si comparamos los resultados, las personas que consumen
# alcohol, es mayor que las personas que no consumen.

# 3) La mayoría de las personas que se encuentran en el tramo de menores de 18 han consumido marihuana
menores_de_edad <- datos %>%
  filter(tramo_etario == "Tramo 1")
menores_de_edad

menores_marihuana <- menores_de_edad %>%
  summarise(menores_marihuana = mean(Marihuana == "Sí", na.rm = TRUE))
menores_marihuana

# Falso, ya que los menores que si consumen marihuana representan al
# 13,1%.
  
# 4) La mayoría de los hombres entre 23 y 26 han consumido cocaína.
hombres_23_26 <- datos %>%
  filter(tramo_etario == "Tramo 3", Sexo == "Hombre")
hombres_23_26

proporcion_h2326_cocaina <- hombres_23_26 %>%
  summarise(proporcion_h2326_cocaina = mean(Cocaina == "Sí", na.rm =T))
proporcion_h2326_cocaina

# Falso, la mayoría de hombres de ese tramo que han consumido cocaina, representa
# al 3,34%.

# 5) La mayoría de las personas que ha consumido cocaína, también ha consumido alcohol.
consumidores_cocaina <- datos %>%
  filter(Cocaina == "Sí")
consumidores_cocaina

proporcion_cocaina_alcohol <- consumidores_cocaina %>%
  summarise(proporcion_alcohol = mean(Alcohol == "Sí"))
proporcion_cocaina_alcohol

# Verdadero, la mayoría de personas que consumieron alcohol también
# han consumido cocaína.(95,7%)

