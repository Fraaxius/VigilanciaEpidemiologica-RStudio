# Instalación de libs
install.packages("dplyr")
library(dplyr)

install.packages("readxl")
library(readxl)

install.packages("haven")
library(haven)

install.packages('ggplot2')
library(ggplot2)

# Carga de librerías
library('readxl')
library('dplyr')
library('haven')
library('ggplot2')

# Let's go
bbdd <- "https://github.com/Fraaxius/VigilanciaEpidemiologica-RStudio/raw/refs/heads/main/base_duoc.xlsx"
destfile <- "base_duoc.xlsx"

download.file(bbdd, destfile, mode = "wb")

data <- read_excel(destfile, sheet = 1)

data <- data %>% 
  mutate(Talla = as.numeric(Talla))

# Gráfico de dispersión con color por sexo y línea de regresión
ggplot(data, aes(x = Talla, y = Altura, color = Sexo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gráfico de dispersión: Altura corporal y Talla de Calzado", 
       x = "Talla Calzado", y = "Altura") +
  theme_minimal()


