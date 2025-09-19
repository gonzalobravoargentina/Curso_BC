
# Cargar la librería
library(googlesheets4)

# Autorizar el acceso a Google Drive
gs4_auth()

# Leer los datos de la hoja de Google Sheets
url <- "https://docs.google.com/spreadsheets/d/1DfcXNHJH9Ar6EjApe2tsqgijyYCrnjz_P2O-HirjuGY/edit?usp=sharing"
datos <- read_sheet(url)

library(dplyr)
library(ggplot2)
library(tidyr)

# Calcular estadísticas básicas
resumen <- datos %>%
  select(-Timestamp) %>%
  summarise_all(list(
    Promedio = mean,
    Mediana = median,
    Desviación_Estándar = sd
  ))

# Seleccionar solo las columnas numéricas (excluyendo Timestamp)
datos_num <- datos %>%
  select(-Timestamp) %>%  # Excluir la columna Timestamp
  mutate(across(everything(), as.numeric))  # Convertir todas las columnas a numéricas

# Eliminar columnas que contienen solo NA (sin puntajes)
datos_num <- datos_num %>%
  select(where(~ !all(is.na(.))))  # Selecciona solo las columnas sin todos los valores NA

# Transformar los datos a formato largo
datos_largos <- datos_num %>%
  pivot_longer(
    cols = everything(),   # Seleccionamos todas las columnas
    names_to = "Pregunta",  # Las columnas se transforman en una columna de 'Pregunta'
    values_to = "Puntaje"   # Los valores de las columnas pasan a la columna 'Puntaje'
  )

# Calcular el puntaje promedio por pregunta
promedio_puntajes <- datos_largos %>%
  group_by(Pregunta) %>%
  summarise(Puntaje_promedio = mean(Puntaje, na.rm = TRUE))  # Calculamos el promedio ignorando los NA

# Ordenar por puntaje promedio de menor a mayor
promedio_puntajes_ordenado <- promedio_puntajes %>%
  arrange(Puntaje_promedio)

# Mostrar las preguntas con los menores puntajes
head(promedio_puntajes_ordenado)

# Graficar los resultados para visualizar las preguntas con menores puntajes
ggplot(promedio_puntajes_ordenado, aes(x = reorder(Pregunta, Puntaje_promedio), y = Puntaje_promedio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Para que las preguntas aparezcan en el eje Y
  labs(
    title = "Puntajes Promedio por Pregunta",
    x = "Pregunta",
    y = "Puntaje Promedio"
  ) +
  theme_minimal()


# Calcular desviación estándar, mediana y cuartiles
resumen_estadistico <- datos_largos %>%
  group_by(Pregunta) %>%
  summarise(
    Puntaje_promedio = mean(Puntaje, na.rm = TRUE),
    Desviacion_Estandar = sd(Puntaje, na.rm = TRUE),
    Mediana = median(Puntaje, na.rm = TRUE),
    Q1 = quantile(Puntaje, 0.25, na.rm = TRUE),
    Q3 = quantile(Puntaje, 0.75, na.rm = TRUE)
  )

# Mostrar resultados
print(resumen_estadistico)