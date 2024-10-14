### Taller de Inferencia Estadística
#
# Nombre: Francisco Sánchez M.
#
# Asignatura:
# - Sistemas de Vigilancia Epidemiológica
#

# Instalar y correr librerías
install.packages("dplyr")
library(dplyr)

install.packages("readxl")
library(readxl)

install.packages("haven")
library(haven)

install.packages('ggplot2')
library(ggplot2)

# Agregar base de datos
data <- readRDS('C:/Users/CETECOM/Downloads/jovenes.rds')

# Seleccionar columnas necesarias
datos <- data %>%
  select(
    Edad = EDAD,
    Sexo = SEXO,
    Alcohol = P76_1,
    inicio_sexual = P78,
    inicio_actsexual = P81,
    p_sexualesaño = P82,
  )
head(datos)

datos %>%
  filter(inicio_actsexual == 0)

datos <- datos %>%
  mutate(inicio_actsexual = na_if(inicio_actsexual, 99))

# Eliminar NA's
datos <- datos %>%
  filter(!is.na(inicio_actsexual) & !is.na(p_sexualesaño))

# 1. Crear variable tramo_etario
datos <- datos %>%
  mutate(tramo_etario = case_when(
    Edad >= 15 & Edad <= 19 ~ "Tramo 1",
    Edad >= 20 & Edad <= 24 ~ "Tramo 2",
    Edad >= 25 & Edad <= 29 ~ "Tramo 3",
  ))

# 1.1. Las personas del tramo 1 vs el tramo 3 tienen más parejas sexuales en promedio (P82).
medias_tramos <- datos %>%
  group_by(tramo_etario) %>%
  summarise(media_parejas = mean(p_sexualesaño, na.rm = TRUE))

ggplot(medias_tramos, aes(x = tramo_etario, y = media_parejas, fill = tramo_etario)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(media_parejas, 1)), 
            vjust = -0.5, size = 3) + 
  scale_fill_manual(values = c("Tramo 1" = "lightblue", "Tramo 2" = "lightgreen", "Tramo 3" = "purple")) +
  labs(title = "Media de parejas sexuales por tramo etario", 
       x = "Tramo etario de edad", 
       y = "Media de parejas sexuales") +
  theme_minimal()

# Comparación de Tramo 1 y 3
anova_result <- aov(p_sexualesaño ~ tramo_etario, data = datos)
summary(anova_result)

# 2. El tramo etario y el consumo de alcohol (P76_1) son independientes.

# Tabla de contingencia
tabla_contingencia <- table(datos$tramo_etario, datos$Alcohol)

# Chi-cuadrado
chisq.test(tabla_contingencia)

# La hipótesis cero se asocia a que el tramo etario
# y el consumo de alcohol son variables independientes.

# Por lo tanto, debido al resultado de P-VALUE obtenida,
# rechazamos de inmediato la hipótesis nula, ya que ambas
# variables son totalmente relacionadas entre sí.

# p-value < 2.2e-16 ~ 0.0000000000000022

# 3. Las variables edad de inicio de la actividad sexual (P81) y el número de parejas sexuales en el último año no están relacionadas.

# Correlación
correlacion <- cor(datos$inicio_actsexual, datos$p_sexualesaño, use = "complete.obs")

# Análisis de regresión
modelo <- lm(p_sexualesaño ~ inicio_actsexual, data = datos)
summary(modelo)

# Gráfico de dispersión
ggplot(datos, aes(x = inicio_actsexual, y = p_sexualesaño)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Edad de Inicio de Actividad Sexual y Número de Parejas Sexuales")
