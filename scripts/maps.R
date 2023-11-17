library(tidyverse)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(geojsonio)
library(stringi)

data_file <- "data/Brazil with Bahia supplemental data_2023-11-13.csv"
bahia <- read_csv(data_file) %>%
  filter(admin1 == "Bahia")

leaflet(data = bahia) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lat = ~latitude, lng = ~longitude, radius = ~fatalities,
                   color = "#B51820", opacity = .01)

brasil <- read_csv(data_file) %>%
  group_by(admin1) %>%
  summarise(fatalities = sum(fatalities, na.rm = TRUE))

brasil_geo <- geojson_read("data/geoBoundaries-BRA-ADM1_simplified.geojson",
                           what = "sp") 
  
brasil_geo@data$shapeNameSimple <- stri_trans_general(
  str = brasil_geo@data$shapeName, id = "Latin-ASCII")
brasil_geo@data$shapeNameSimple <- ifelse(
  brasil_geo@data$shapeNameSimple == "Federal District", "Distrito Federal",
  brasil_geo@data$shapeNameSimple)

brasil_geo$fatalities <- brasil$fatalities[match(brasil_geo$shapeNameSimple, brasil$admin1)]
  
labels <- sprintf(
  "<strong>%s</strong><br/>%g fatalities in 2023",
  brasil_geo$shapeName, brasil_geo$fatalities
) %>% lapply(htmltools::HTML)

pal <- colorNumeric(c("#FAFAFA", "#B31536"),
                    domain = c(0, max(brasil_geo$fatalities)))

leaflet(brasil_geo, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(minZoom = 0)) %>%
  addPolygons(
    fillColor = ~pal(fatalities),
    weight = 1,
    opacity = 1,
    color = "#3B3B3B",
    fillOpacity = .8,
    highlightOptions = highlightOptions(
      weight = 2,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) 
