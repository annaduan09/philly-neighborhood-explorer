library(shiny)
library(sf)
library(tigris)

#### STATIC ####


# Data intake
nb <- st_read(
  "/Users/annaduan/Desktop/GitHub/philly-neighborhood-explorer/data/neighborhood/phl_neighs_2024.geojson"
) %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG:4326") 


# Metadata
## Display aliases
## Variable unit
## Variable prefixes
## Variable suffixes


#### DYNAMIC ####
server <- function(input, output, session) {
  
  # Filtering logic
  ## Geography: map area
# Filtering logic
nb_filt = reactive({
  nb %>%
    filter(MAPNAME %in% input$neighborhoods)
})
  ## Constraints: income/household size config -> expected contribution
  ## Preferences: amenities types, demographics thresholds
  
  
  
  # Leaflet map
  output$leaflet <- renderLeaflet({
    print("Rendering leaflet map")
    leaflet(options = leafletOptions(minZoom = 7)) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light Theme") %>%
      setView(lng = -75.13406,
              lat = 40.00761,
              zoom = 11) %>%
      setMaxBounds(
        lng1 = -75.28027,
        lat1 = 39.867,
        lng2 = -74.95576,
        lat2 = 40.13799
      ) %>%
      addPolygons(
        data = nb_filt(),
        fillColor = "darkcyan",
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.3,
        dashArray = "3",
        highlightOptions = highlightOptions(
          weight = 1,
          color = "white",
          dashArray = "",
          fillOpacity = 0.3,
          bringToFront = TRUE
        )
      )
    
  })
  
  # Colormapping
  ## Palette
  ## Dynamic binning
  
}
