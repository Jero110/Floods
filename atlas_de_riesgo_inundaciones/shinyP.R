library(sf)

# Lee el archivo .shp
shapefile_path <- "atlas_de_riesgo_inundaciones.shp"
gdf <- st_read("/Users/jeronimo/Desktop/Jero/VS_Code/VI/atlas_de_riesgo_inundaciones/atlas_de_riesgo_inundaciones.shp")


# Muestra las primeras filas del GeoDataFrame
# print(head(gdf))

library(sf)
library(leaflet)

# Read the shapefile

# Create a simple color palette
risk_colors <- c(
    "Muy Alto" = "#FF0000", # Red
    "Alto" = "#FF4500", # Orange-Red
    "Medio" = "#FFA500", # Orange
    "Bajo" = "#FFD700", # Gold
    "Muy Bajo" = "#90EE90" # Light Green
)

# Create the Leaflet map
flood_risk_map <- leaflet(gdf) %>%
    addTiles() %>%
    setView(lng = -99.1332, lat = 19.4326, zoom = 10) %>%
    addPolygons(
        fillColor = risk_colors[gdf$intnsdd],
        color = "white",
        weight = 2,
        opacity = 1,
        dashArray = "3",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE),
        popup = paste0(
            "<strong>Delegación:</strong> ", gdf$alcaldi, "<br>",
            "<strong>Riesgo:</strong> ", gdf$intnsdd
        )
    )

# Display the map
flood_risk_map
