# TALLER 1

# IMPORTAR LA BASE DE DATOS: JÓVENES (RDS)

install.packages("dplyr")
library(dplyr)

install.packages("readxl")
library(readxl)

install.packages("haven")
library(haven)

install.packages('ggplot2')
library(ggplot2)

data <- readRDS('C:/Users/fernp/Downloads/jovenes.rds')

# seleccionar y renombrar las variables a utilizar

# p82 = Número de parejas sexuales
# SEXO = sexo
# P76_1 = alcohol
# P76_3 = marihuana
# P76_4 = cocaina
# P76_5 = pbc

datos <- data %>%
  select(
    n_psexuales = P82,
    Sexo = SEXO,
    Alcohol = P76_1,
    Marihuana = P76_3,
    Cocaina = P76_4,
    PBC = P76_5
  )

SiNo <- function(x) {
  x <- factor(x, levels = c(1, 2), labels = c("Si", "No"))
}

Sexo <- function(x) {
  x <- factor(x, levels = c(1, 2), labels = c("Hombre", "Mujer"))
}

datos <- datos %>%
  mutate(across(-c(n_psexuales, Sexo), SiNo))

datos <- datos %>%
  mutate(Sexo = Sexo(Sexo))

# apoyandose en los datos, definir si las siguientes afirmaciones son verdaderas o falsas:
# TODAS LAS RESPUESTAS DEBEN INCLUIR UN GRÁFICO

# 1) Los hombres declaran tener más parejas sexuales que las mujeres

datos %>%
  filter(!is.na(n_psexuales)) %>%
  group_by(Sexo) %>% 
  summarise(suma_n_psexuales = mean(n_psexuales, na.rm = TRUE))
  

# Gráfico de barras del número total de parejas sexuales por sexo con valores en las barras
ggplot(datos %>% filter(!is.na(n_psexuales)), aes(x = Sexo, y = n_psexuales, fill = Sexo)) +
  geom_bar(stat = "summary", fun = "mean") +  # Usamos "mean" para mostrar el promedio
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 1)), 
            vjust = -0.5, size = 5) +  # Añadiendo los valores encima de las barras
  labs(title = "Promedio del número de parejas sexuales por sexo",
       x = "Sexo",
       y = "Promedio de parejas sexuales") +
  theme_minimal() +
  scale_fill_manual(values = c("#e4cff5", "#e0f5cf"))

### RESPUESTA:  Verdadero , los hombres declaran tener más parejas sexuales que las mujeres.



# 2) Las personas que consumen dos sustancias o más (policonsumo), tienden
# a tener más parejas en comparación a las personas que consumen una sola sustancia

datos <- datos %>%
  rowwise() %>%
  mutate(Consumo = ifelse(sum(c(Alcohol, Marihuana, Cocaina, PBC) == "Si") > 1, "Policonsumo", "Monoconsumo"))

datos %>%
  filter(!is.na(n_psexuales)) %>%
  count(Consumo)


# Calcular el promedio de parejas sexuales por tipo de consumo
promedios <- datos %>%
  filter(!is.na(n_psexuales), !is.na(Consumo)) %>%
  group_by(Consumo) %>%
  summarise(mean_psexuales = mean(n_psexuales))

# Gráfico de barras del número promedio de parejas sexuales por tipo de consumo con los valores encima
ggplot(datos %>% filter(!is.na(n_psexuales), !is.na(Consumo)), aes(x = Consumo, y = n_psexuales, fill = Consumo)) +
  geom_bar(stat = "summary", fun = "mean") +  # Usamos "mean" para mostrar el promedio
  geom_text(data = promedios, aes(x = Consumo, y = mean_psexuales, label = round(mean_psexuales, 1)), 
            vjust = -0.5, size = 5) +  # Ajustar posición y tamaño del texto
  labs(title = "Promedio de parejas sexuales entre policonsumidores y monoconsumidores",
       x = "Tipo de consumo",
       y = "Promedio de parejas sexuales") +
  theme_minimal() +
  scale_fill_manual(values = c("#9999ff", "#ff0000"))


### RESPUESTA: Verdadero, las personas con policonsumo tienden a tener más parejas sexuales que las personas
### que tienen un monoconsumo 

# 3) Hay más mujeres policonsumidoras que hombres policonsumidores

datos %>%
  filter(!is.na(Consumo)) %>%
  group_by(Sexo) %>%
  count(Consumo)

# Calcular la cantidad de personas por sexo y consumo
conteos <- datos %>%
  filter(!is.na(Consumo)) %>%
  group_by(Sexo, Consumo) %>%
  summarise(count = n())

# Gráfico de barras de la cantidad de policonsumidores por sexo con los valores encima
ggplot(datos %>% filter(!is.na(Consumo)), aes(x = Sexo, fill = Consumo)) +
  geom_bar(position = "dodge") +
  geom_text(data = conteos, aes(x = Sexo, y = count, label = count, group = Consumo),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +  # Ajustar la posición y tamaño del texto
  labs(title = "Distribución de Policonsumidores por Sexo",
       x = "Sexo",
       y = "Cantidad de personas") +
  theme_minimal() +
  scale_fill_manual(values = c("#b4ca99", "#ca99b4")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


### RESPUESTA: Falso, Hay más hombres policonsumidores que mujeres policonsumidoras. ####