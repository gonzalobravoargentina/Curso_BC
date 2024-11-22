# Este script fue desarrollado como parte del curso de Buceo Científico 2024
# de la Universidad Nacional de la Patagonia San Juan Bosco (UNPSJB), durante
# el cual se realizaron ejercicios prácticos para el muestreo subacuático. 
# El script tiene como objetivo realizar dos ejercicios específicos:

# 1. Conteo de Piedras a lo largo de una transecta de 20 metros: 
# En este ejercicio se realizaron estimaciones utilizando dos métodos de muestreo: 
# cuadrantes visuales de 50 x 50 cm y fotocuadrantes de 25 x 25 cm. 
# La idea principal fue comparar las estimaciones de densidad de piedras obtenidas 
# a través de estos dos métodos. Para contar todas las piedras a lo largo de la transecta, 
# se utilizó también una transecta en video, en la que se registraron todas las piedras visualmente sobre el video.

# 2. Censo Visual de Peces con Peces Artificiales: 
# En este ejercicio, se colocaron peces artificiales con tres características diferentes: 
# cabeza negra, cabeza blanca y un círculo negro. Los buzos se encargaron de contabilizar 
# las tres especies a lo largo de las dos bandas de la transecta (de 2 metros de ancho cada una). 
# Se realizaron los conteos de estas especies a lo largo de la transecta para comparar la eficiencia 
# de la visualización de peces artificiales en diferentes condiciones.

# Este script permite realizar un análisis de las estimaciones de densidad, comparando 
# las diferentes metodologías empleadas para ambos ejercicios. Además, se incluye la visualización 
# de los resultados en gráficos y la posibilidad de acceder a datos adicionales almacenados en hojas  de Google para complementar el análisis.


# Leer los metadatos de los fotocuadrantes----
# Seleccionar el directorio de trabajo y elegir los archivos de imagen
photoquadrats <- list.files(pattern = ".jpg|.JPG|.png")  # Obtener la lista de archivos .jpg en el directorio de trabajo

# Cargar librerías necesarias
library(exifr)  
METADATA_photoquadrats <- read_exif(photoquadrats)  # Leer los metadatos de los fotocuadrantes 
METADATA_photoquadrats <- as.data.frame(METADATA_photoquadrats)  # Convertir los metadatos en un dataframe

# Crear un dataframe con solo las columnas de interés 
library(dplyr)
METADATA_photoquadrats_short <- dplyr::select(METADATA_photoquadrats, SourceFile, time = TimeCreated, 
                                              timeLOCAL = DateTimeOriginal, Latitude = GPSLatitude, 
                                              Longitude = GPSLongitude, Buzo = Artist, 
                                              Transecta = ImageDescription, Conteo = Title) 

# Transformar la columna 'timeLOCAL' a formato POSIXlt
METADATA_photoquadrats_short$timeLOCAL <- strptime(METADATA_photoquadrats_short$timeLOCAL, "%Y:%m:%d %H:%M:%S")

# Calcular la densidad en individuos por metro cuadrado (ind/m²)
area_m2 <- 0.25 * 0.25  # Área del cuadrante en metros cuadrados
METADATA_photoquadrats_short$densidad <- METADATA_photoquadrats_short$Conteo / area_m2  # Densidad de los fotocuadrantes

# Leer los metadatos de los fotocuadrantes----
# Seleccionar el directorio de trabajo y elegir los archivos de imagen
photoquadrats <- list.files(pattern = ".jpg|.JPG|.png")  # Obtener la lista de archivos .jpg en el directorio de trabajo

# Cargar librerías necesarias
library(exifr)  
METADATA_photoquadrats <- read_exif(photoquadrats)  # Leer los metadatos de los fotocuadrantes 
METADATA_photoquadrats <- as.data.frame(METADATA_photoquadrats)  # Convertir los metadatos en un dataframe

# Crear un dataframe con solo las columnas de interés 
library(dplyr)
METADATA_photoquadrats_short <- dplyr::select(METADATA_photoquadrats, SourceFile, time = TimeCreated, 
                                              timeLOCAL = DateTimeOriginal, Latitude = GPSLatitude, 
                                              Longitude = GPSLongitude, Buzo = Artist, 
                                              Transecta = ImageDescription, Conteo = Title) 

# Transformar la columna 'timeLOCAL' a formato POSIXlt
METADATA_photoquadrats_short$timeLOCAL <- strptime(METADATA_photoquadrats_short$timeLOCAL, "%Y:%m:%d %H:%M:%S")

# Calcular la densidad en individuos por metro cuadrado (ind/m²)
area_m2 <- 0.25 * 0.25  # Área del cuadrante en metros cuadrados
METADATA_photoquadrats_short$densidad <- METADATA_photoquadrats_short$Conteo / area_m2  # Densidad de los fotocuadrantes

# Calcular el promedio y la desviación estándar de la densidad por transecta
library(dplyr)
Densidad_fotocuadrantes <- METADATA_photoquadrats_short %>% 
  group_by(Transecta) %>% 
  summarise(
    mean_density = mean(densidad),  # Promedio de densidad
    sd_density = sd(densidad)  # Desviación estándar de densidad
  )

# Visualización de la distribución de la densidad por transecta
library(ggplot2)
ggplot(METADATA_photoquadrats_short, aes(x = Transecta, y = densidad)) +
  geom_boxplot() + 
  labs(title = "Distribución de densidad por transecta", 
       x = "Transecta", 
       y = "Densidad (ind/m²)")

# Mapa interactivo con capas y zoom
library(leaflet)

# Crear un mapa interactivo con una capa satelital y mayor zoom
METADATA_photoquadrats_short_GB <- subset(METADATA_photoquadrats_short, Buzo == "Gonzalo Bravo")  # Filtrar por buzo
leaflet(data = METADATA_photoquadrats_short_GB) %>% 
  setView(lng = mean(METADATA_photoquadrats_short_GB$Longitude), 
          lat = mean(METADATA_photoquadrats_short_GB$Latitude), 
          zoom = 16) %>%  # Ajustar nivel de zoom
  addTiles(group = "OpenStreetMap", options = tileOptions(maxZoom = 22)) %>%  # Agregar capa base de OpenStreetMap
  addProviderTiles(providers$Esri.WorldImagery, 
                   group = "Satélite", 
                   options = tileOptions(maxZoom = 22)) %>%  # Agregar capa satelital
  addCircleMarkers(
    lng = ~Longitude, 
    lat = ~Latitude, 
    radius = 5, 
    color = "blue", 
    fillOpacity = 0.7, 
    popup = ~paste0(
      "<b>Cuadrante:</b> ", SourceFile, "<br>", 
      "<b>Transecta:</b> ", Transecta, "<br>", 
      "<b>Densidad:</b> ", round(densidad, 2), " ind/m²"
    )
  ) %>% 
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satélite"), 
    options = layersControlOptions(collapsed = TRUE)
  )




# Acceso a Google Sheets con googlesheets4 para datos de peces y piedras
library(googlesheets4)
# gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

# Enlace a la hoja de Google Sheets
url <- "https://docs.google.com/spreadsheets/d/1gKbS7mw3xdtgI0NdKmV8iSP4QYsq_gXpVuE3yx1G65A/edit?usp=sharing"

# Leer todas las hojas disponibles
sheet_names <- sheet_names(url)  # Obtener los nombres de las hojas
dataframes <- list()  # Lista para almacenar los dataframes

# Leer las hojas y almacenarlas en la lista
for (sheet in sheet_names) {
  dataframes[[sheet]] <- read_sheet(url, sheet = sheet) 
}

# Acceso a los dataframes por nombre de hoja
# Ejemplo de acceso:
Piedras <- dataframes[["Piedras"]]
Peces <- dataframes[["Peces"]]

rm(dataframes)

#####################################################################

# Cálculo del coeficiente de variación (CV) para los peces
library(dplyr)
cv_results <- Peces %>% 
  group_by(Transecta, Especie) %>% 
  summarise(
    Mean = mean(Conteo), 
    SD = sd(Conteo), 
    CV = ifelse(Mean == 0, NA, (SD / Mean) * 100),  # Coeficiente de variación
    .groups = "drop"
  )

library(ggplot2)
cv_results <- subset(cv_results, Transecta != "TOTAL")  # Excluir la transecta "TOTAL"
# Crear el gráfico del coeficiente de variación
ggplot(cv_results, aes(x = Especie, y = CV, fill = Transecta)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#66C2A5", "#FC8D62")) +  # Colores personalizados
  labs(
    title = "Coeficiente de variación por transecta y especie", 
    x = "Especie", 
    y = "Coeficiente de variación (%)", 
    fill = "Transecta"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Incluir texto inclinado
    legend.title = element_text(size = 10), 
    plot.title = element_text(hjust = 0.5)  # Centrar el título
  )

# Sumar los conteos de BABOR y ESTRIBOR, y mantener TOTAL
peces_totales <- Peces %>% 
  group_by(Buzo, Especie) %>% 
  summarise(
    Conteo_Total = sum(Conteo[Transecta %in% c("BABOR", "ESTRIBOR")], na.rm = TRUE) + 
      sum(Conteo[Transecta == "TOTAL"], na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)
# Crear gráfico con líneas punteadas
ggplot(peces_totales, aes(x = Buzo, y = Conteo_Total, fill = Especie)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(
    data = peces_totales %>% filter(Buzo == "VIDEO"), 
    aes(yintercept = Conteo_Total, color = Especie), 
    linetype = "dotted", size = 0.8 
  ) + 
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")) + 
  scale_color_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")) + 
  labs(title = "Conteos totales por buzo y especie", y = "Conteo total", x = "Buzo") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.title = element_blank(), 
    legend.position = "right" 
  )

# Sumar los conteos de BABOR y ESTRIBOR, y mantener TOTAL
peces_totales <- Peces %>% 
  group_by(Buzo) %>% 
  summarise(
    Conteo_Total = sum(Conteo[Transecta %in% c("BABOR", "ESTRIBOR")], na.rm = TRUE) + 
      sum(Conteo[Transecta == "TOTAL"], na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)
# Crear el gráfico con líneas punteadas
ggplot(peces_totales, aes(x = Buzo, y = Conteo_Total)) + 
  geom_bar(stat = "identity", fill = "#66C2A5", position = "dodge") +  # Usar un color sólido para las barras
  geom_hline(
    data = peces_totales %>% filter(Buzo == "VIDEO"), 
    aes(yintercept = Conteo_Total), 
    linetype = "dotted", size = 0.8
  ) + 
  labs(title = "Conteos totales por buzo", y = "Conteo total", x = "Buzo") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.title = element_blank(), 
    legend.position = "none"
  )

