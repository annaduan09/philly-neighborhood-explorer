library(shiny)
library(sf)
library(shinyWidgets)
library(tigris)
library(leaflet)
library(sf)
library(tidyverse)

#### STATIC ####
# Data intake
nb <- st_read(
  "data/panel.geojson"
)

colleges <- st_read("data/point/colleges.geojson")
parks <- st_read("data/point/parks.geojson")
park_polys <- st_read("data/polygon/PPR_Properties.geojson")
cityhall <- st_read("data/point/cityhall.geojson")
hospital <- st_read("data/point/Hospitals.geojson")

# Neighborhoods
# Define neighborhoods grouped by region
north_neighborhoods <- c("Richmond", "Frankford", "Juniata Park", "Northwood", "Harrowgate", "Hunting Park", "Nicetown", "Tioga", "Feltonville", "Logan")
northwest_neighborhoods <- c("West Oak Lane", "East Oak Lane", "Chestnut Hill", "East Germantown", "Southwest Germantown", "Roxborough", "Manayunk", "Mount Airy")
northeast_neighborhoods <- c("Mayfair", "Tacony", "Holmesburg", "Fox Chase", "Bustleton", "Somerton", "Oxford Circle", "Rhawnhurst")
west_neighborhoods <- c("University City", "Wynnefield", "Overbrook", "Cobbs Creek", "Walnut Hill", "Spruce Hill")
center_city_neighborhoods <- c("Rittenhouse", "Logan Square", "Chinatown", "Society Hill", "Washington Square West")
south_neighborhoods <- c("South Philadelphia", "Point Breeze", "Girard Estates", "Passyunk Square", "Whitman")
southwest_neighborhoods <- c("Kingsessing", "Elmwood", "Eastwick")

#### DYNAMIC ####
server <- function(input, output, session) {
  
##### User alerts #####

  # Alert to welcome user
  shinyalert(html = TRUE, "Welcome!", 
             "This tool is designed to help you explore Philly neighborhoods you'd like to live in. 
             We're going to ask you a few questions about where you'd like to live and what you are looking for in a neighborhood. 
             Based on that, we'll suggest some areas for you to consider. Happy searching!",
             size = "s",
             confirmButtonText = "Let's get started",
             confirmButtonCol = "#AEDEF4",
             imageUrl = "www/welcome.png",
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE
             )
  
  # Alert to input neighborhoods
  shinyalert(html = TRUE, 
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE,
             "Neighborhood Filter",
             text = tagList(
               selectizeInput(
               "neighborhoods",
               "Choose a few neighborhoods you'd like to consider. The more neighborhoods you select, the more likely we are able to find matches for your preferences.
               You can also choose to select all neighborhoods in a region or in the city using the buttons below.",
               choices = list(
                 "North" = north_neighborhoods,
                 "Northwest" = northwest_neighborhoods,
                 "Northeast" = northeast_neighborhoods,
                 "West" = west_neighborhoods,
                 "Center City" = center_city_neighborhoods,
                 "South" = south_neighborhoods,
                 "Southwest" = southwest_neighborhoods),
               selected = c("West" = "Wynnefield", "Northwest" = c("Germantown", "Roxborough")),
               multiple = TRUE,
               options = NULL
             ),
             actionButton("neigh_all", "Select All")
                            ))
  
  # alert to input amenities
  shinyalert(html = TRUE, 
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE,
             "Neighborhood features",
             text = tagList(
               selectizeInput(
                 "amenities",
                 "I'm looking for:",
                 choices = list(
                   "Amenities" = list(
                     "Restaurants" = "restaurant",
                     "Grocery stores" = "grocery",
                     "Shopping" = "shopping",
                     "Parks" = "parks",
                     "Healthcare" = "healthcare"
                   ),
                   "Community" = list(
                     "Families with kids" = "kids",
                     "Longtime residents" = "same_house_pct2022",
                     "Voucher users" = "vouchers",
                     "Population" = "population2022",
                     "Safety" = "shootings_100k"
                   )),
                 selected = c("Community" = "kids", "Amenities" = "restaurant"),
                 multiple = TRUE,
                 options = NULL
               )
             ))
  
  # alert - expected cost calculator and landmark toggles
  shinyalert(html = TRUE, "Calculate expected voucher cost",
             "To calculate how much you will have to contribute to your voucher unit's monthly rent, 
             enter your household size (who will be living in your voucher unit) and your monthly income into
             the cost calculator tool to the left of the screen",
             type = "info")
  
  output$neighborhoods <- renderText({
  input$neighborhoods
  })
  
  output$amenities <- renderText({
    input$amenities
  })
  
  observeEvent(input$update_neighs, {
    shinyalert(html = TRUE, "Neighborhood Filter",
               text = tagList(
                 selectizeInput(
                   "neighborhoods",
                   "Choose a few neighborhoods you'd like to consider. The more neighborhoods you select, the more likely we are able to find matches for your preferences.
               You can also choose to select all neighborhoods in a region or in the city using the buttons below.",
                   choices = list(
                     "North" = north_neighborhoods,
                     "Northwest" = northwest_neighborhoods,
                     "Northeast" = northeast_neighborhoods,
                     "West" = west_neighborhoods,
                     "Center City" = center_city_neighborhoods,
                     "South" = south_neighborhoods,
                     "Southwest" = southwest_neighborhoods),
                   selected = c("West" = "Wynnefield", "Northwest" = c("Germantown", "Roxborough")),
                   multiple = TRUE,
                   options = NULL
                 ),
                 actionButton("neigh_all", "Select All")
               ))
  })
  
  observeEvent(input$update_prefs, {
    shinyalert(html = TRUE, "Neighborhood features",
               text = tagList(
                 selectizeInput(
                   "amenities",
                   "I'm looking for:",
                   choices = list(
                     "Amenities" = list(
                       "Restaurants" = "restaurant",
                       "Grocery stores" = "grocery",
                       "Shopping" = "shopping",
                       "Parks" = "parks",
                       "Healthcare" = "healthcare"
                     ),
                     "Community" = list(
                       "Families with kids" = "kids",
                       "Longtime residents" = "same_house_pct2022",
                       "Voucher users" = "vouchers",
                       "Population" = "population2022",
                       "Safety" = "shootings_100k"
                     )),
                   selected = c("Community" = "kids", "Amenities" = "restaurant"),
                   multiple = TRUE,
                   options = NULL
                 )
               ))
  })
  # Filtering logic
  ## Geography: map area
  nb_filt = reactive({
    nb_sel = nb %>%
      filter(neighborhood %in% input$neighborhoods)
  
    amenities = input$amenities
    denom = length(amenities)
    nb_sel$neighborhood_score = 0
    
    for (amenity in amenities) {
      nb_sel$neighborhood_score = nb_sel$neighborhood_score + nb_sel[[amenity]]
    }
    
    nb_sel$neighborhood_score = nb_sel$neighborhood_score / denom
    
    return(nb_sel)
  })
  
  
  
  ## Constraints: income/household size config -> expected contribution
  output$monthly_payment = renderText({
    income = input$income
    household_size = input$household_size
    
    if (income <= 167) {
      return("$50")
    }
    else if (household_size <= 2) {
      return(paste(
        "$",
        round(0.28 * income / 12),
        " to ",
        "$",
        round(0.4 * income / 12),
        sep = ""
      ))
    }
    else if (household_size <= 5) {
      return(paste(
        "$",
        round(0.27 * income / 12),
        " to ",
        "$",
        round(0.4 * income / 12),
        sep = ""
      ))
    }
    else {
      return(paste(
        "$",
        round(0.26 * income / 12),
        " to ",
        "$",
        round(0.4 * income / 12),
        sep = ""
      ))
    }
  })
  
  
#####  Leaflet map #####
  palette <- "PuBu"
    # c("#F0F8FF", "#7EB7C0", "#0B7580")
  
  # reactive palette
  mapPalette <- reactive({
    leaflet::colorQuantile(
      na.color = "white",
      palette = palette,
      domain = NULL,
      n = 5,
      reverse = FALSE
    )
  })
  
  output$leaflet <- renderLeaflet({
    print("Rendering leaflet map")
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light Theme") %>%
      setView(lng = -75.13406,
              lat = 40.00761,
              zoom = 12) %>%
      setMaxBounds(
        lng1 = -75.28027,
        lat1 = 39.867,
        lng2 = -74.95576,
        lat2 = 40.13799
      ) %>%
      addPolygons(
        data = nb,
        fillColor = "black",
        color = "white",
        weight = 1,
        opacity = 0.1,
        fillOpacity = 0.1
      ) %>%
      # City Hall
      addMarkers(
        data = cityhall$geometry,
        popup = "City Hall",
        icon = makeIcon(iconUrl = "www/cityhall.png", iconWidth = 30, iconHeight = 30),
        group = "City Hall",
        options = markerOptions(opacity = 0.4)
      ) %>%
      # Parks
      addMarkers(
        data = parks$geometry,
        popup = parks$name,
        icon = makeIcon(iconUrl = "www/park.png", iconWidth = 20, iconHeight = 20),
        group = "Parks",
        options = markerOptions(opacity = 0.4)
      ) %>%
      # Colleges
      addMarkers(
        data = colleges$geometry,
        popup = colleges$NAME,
        icon = makeIcon(iconUrl = "www/college.png", iconWidth = 30, iconHeight = 30),
        group = "Colleges",
        options = markerOptions(opacity = 0.4)
      ) %>%
      # Hospitals
      addMarkers(
        data = hospital$geometry,
        popup = hospital$HOSPITAL_NAME,
        icon = makeIcon(iconUrl = "www/hospital.png", iconWidth = 30),
        group = "Hospitals",
        options = markerOptions(opacity = 0.4)
      ) %>%
      addPolygons(
        data = nb_filt(),
        fillColor = ~mapPalette()(nb_filt()$neighborhood_score),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        dashArray = "3",
        popup = ~paste(
          "<b>Neighborhood:</b> ",
          neighborhood,
          "<br>",
          "<b>Score:</b> ",
          neighborhood_score
        ),
        highlightOptions = highlightOptions(
          weight = 1,
          color = "white",
          dashArray = "",
          fillOpacity = 0.3
        )
      ) %>%
      addPolygons(
        data = park_polys,
        fillColor = "olivedrab",
        color = "white",
        weight = 1,
        opacity = 0.1,
        fillOpacity = 0.3,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "white",
          fillOpacity = 0.5,
          bringToFront = TRUE
        ),
        popup = park_polys$PUBLIC_NAME
      ) %>%
      # menu to toggle icon markers on and off
      addLayersControl(
        overlayGroups = c("City Hall", "Parks", "Colleges", "Hospitals"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
}
