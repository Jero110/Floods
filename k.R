library(sf)
library(dplyr)
library(leaflet)
library(grDevices)

# Load the data
shapefile_path <- "atlas_de_riesgo_inundaciones/atlas_de_riesgo_inundaciones.shp"
gdf <- st_read(shapefile_path)

# Diagnostic steps
print("Unique geometry types:")
print(unique(st_geometry_type(gdf)))

# Explicitly filter and convert to POLYGON
gdf_clean <- gdf %>%
    filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    st_cast("MULTIPOLYGON")

# Map intensity levels to numeric values
intensity_levels <- c("Muy Bajo" = 1, "Bajo" = 2, "Medio" = 3, "Alto" = 4, "Muy Alto" = 5)

# Group by alcaldía and calculate average flood risk intensity
gdf_summary <- gdf_clean %>%
    mutate(intensity_numeric = intensity_levels[intnsdd]) %>%
    group_by(alcaldi) %>%
    summarise(
        avg_intensity = mean(intensity_numeric, na.rm = TRUE),
        geometry = st_combine(geometry)
    ) %>%
    st_as_sf()

# Define a custom color palette from light blue to dark blue
pal <- colorNumeric(
    palette = colorRampPalette(c("#ADD8E6", "#00008B"))(5),
    domain = gdf_summary$avg_intensity
)

# Create the interactive map
leaflet(gdf_summary) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        fillColor = ~ pal(avg_intensity),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
        ),
        popup = ~ paste0(
            "Alcaldía: ", alcaldi, "<br>",
            "Riesgo Promedio: ", round(avg_intensity, 2)
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
        title = "Riesgo Promedio",
        position = "bottomright"
    )
