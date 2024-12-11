library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(DT)

# Cargar los datos
shapefile_path <- "/Users/jeronimo/Desktop/Jero/VS_Code/VI/atlas_de_riesgo_inundaciones/atlas_de_riesgo_inundaciones.shp"
gdf <- st_read(shapefile_path)

# Agrupar por alcaldía y calcular el nivel promedio de riesgo de inundación
gdf_summary <- gdf %>%
  group_by(alcaldi) %>%
  summarise(
    avg_intensity = mean(as.numeric(factor(intnsdd, levels = c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto"))), na.rm = TRUE),
    geometry = st_combine(geometry), # Combina geometrías de la misma alcaldía
    .groups = "drop"
  ) %>%
  st_cast("POLYGON") %>% # Asegura que las geometrías son polígonos
  st_as_sf()

# Convertir el GeoDataFrame a un dataframe
df <- as.data.frame(gdf)

# Eliminar la columna de geometría para la tabla y graficar
df_no_geom <- df[, !(names(df) %in% "geometry")]

# Definir la paleta de colores personalizada
pal <- colorNumeric(
  palette = colorRampPalette(c("#ADD8E6", "#00008B"))(9), # Azul claro a azul oscuro
  domain = gdf_summary$avg_intensity
)

# Define UI
ui <- fluidPage(
  titlePanel("Análisis de Riesgo de Inundaciones"),
  navbarPage(
    "Menú",
    tabPanel(
      "Mapa y Gráfico",
      fluidRow(
        column(6, leafletOutput("map")), # Mapa a la izquierda
        column(6, plotOutput("barPlot")) # Gráfico de barras a la derecha
      )
    ),
    tabPanel(
      "Tabla",
      DTOutput("table") # Solo la tabla en la segunda página
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Renderizar el mapa
  output$map <- renderLeaflet({
    leaflet(gdf_summary) %>%
      addProviderTiles("CartoDB.Positron") %>% # Añadir un mapa base
      addPolygons(
        fillColor = ~ pal(avg_intensity),
        color = ~ pal(avg_intensity), # Color del borde
        weight = 1, # Grosor del borde
        opacity = 1, # Opacidad del borde
        fillOpacity = 0.7, # Opacidad del relleno del polígono
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~ paste0(alcaldi, ": ", round(avg_intensity, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "color" = "black"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~avg_intensity,
        title = "Riesgo Promedio por Alcaldía",
        position = "bottomright"
      )
  })

  # Renderizar el gráfico de barras
  output$barPlot <- renderPlot({
    df_resumen <- as.data.frame(table(df$intnsdd))
    colnames(df_resumen) <- c("Intensidad", "Registros")

    ggplot(df_resumen, aes(x = Intensidad, y = Registros)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black") + # Color uniforme
      labs(
        title = "Distribución de Registros por Intensidad",
        x = "Intensidad",
        y = "Número de Registros"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Renderizar la tabla dinámica
  df <- df_no_geom[, c(
    "id", "alcaldi", "intnsdd", "area_m2", "perim_m",
    "intens_n", "intns_nm", "int2"
  )]
  output$table <- renderDT({
    datatable(
      df,
      options = list(
        pageLength = 10, # Número de filas visibles por página
        scrollX = TRUE # Habilita desplazamiento horizontal
      )
    )
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
