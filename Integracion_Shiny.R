library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(DT)

# Cargar los datos
shapefile_path <- "/Users/jeronimo/Desktop/Jero/VS_Code/VI/Floods/atlas_de_riesgo_inundaciones/atlas_de_riesgo_inundaciones.shp"
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
df_no_geom <- as.data.frame(gdf)

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
      "Mapa y Gráficas",
      tags$style(HTML("
        #grid-container {
          display: grid;
          grid-template-rows: 70% 30%;
          grid-template-columns: 50% 50%;
          height: 100vh;
          gap: 10px;
        }
        #map-container {
          grid-row: 1 / 2;
          grid-column: 1 / 3;
        }
        #dropdown-container {
          grid-row: 2 / 3;
          grid-column: 1 / 2;
          padding: 10px;
          background-color: white;
          border-radius: 5px;
          box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
        }
        #barplot-container {
          grid-row: 2 / 3;
          grid-column: 2 / 3;
          padding: 10px;
          background-color: white;
          border-radius: 5px;
          box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
        }
      ")),
      div(
        id = "grid-container",
        div(
          id = "map-container",
          leafletOutput("map", height = "100%")
        ),
        div(
          id = "dropdown-container",
          selectInput(
            "alcaldia_filter",
            "Seleccionar Alcaldías:",
            choices = c("Todas", unique(gdf$alcaldi)),
            selected = "Todas",
            multiple = TRUE
          )
        ),
        div(
          id = "barplot-container",
          plotOutput("barPlot", height = "100%")
        )
      )
    ),
    tabPanel(
      "Tabla",
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Filtrar datos según selección
  filtered_data <- reactive({
    if ("Todas" %in% input$alcaldia_filter || length(input$alcaldia_filter) == 0) {
      gdf_summary
    } else {
      gdf_summary %>% filter(alcaldi %in% input$alcaldia_filter)
    }
  })

  filtered_table <- reactive({
    if ("Todas" %in% input$alcaldia_filter || length(input$alcaldia_filter) == 0) {
      df_no_geom[, c("id", "alcaldi", "intnsdd", "area_m2", "perim_m", "intens_n", "intns_nm", "int2")]
    } else {
      df_no_geom %>%
        filter(alcaldi %in% input$alcaldia_filter) %>%
        select(id, alcaldi, intnsdd, area_m2, perim_m, intens_n, intens_nm, int2)
    }
  })

  # Renderizar el mapa
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~ pal(avg_intensity),
        color = ~ pal(avg_intensity),
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
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
    df_resumen <- filtered_table() %>%
      count(intnsdd, name = "Registros")

    ggplot(df_resumen, aes(x = intnsdd, y = Registros)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black") +
      labs(
        title = "Distribución de Registros por Intensidad",
        x = "Intensidad",
        y = "Número de Registros"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Renderizar la tabla dinámica
  output$table <- renderDT({
    datatable(
      filtered_table(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
