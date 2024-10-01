# Load required libraries
library(shiny)
library(shinyjs)
library(conflicted)  
library(shinythemes)  
library(leaflet)      
library(bslib)        
library(sf)
library(tidyverse)

conflicts_prefer(shinyjs::show)

#### UI ####
ui <- fluidPage(
  useShinyjs(),  

  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap",
      rel = "stylesheet"
    ),
    includeCSS("www/styles.css")
  ),
  
##### Welcome panel #####
  div(
    id = "welcome_panel",
    div(
      style = "text-align: center;",
      img(
        src = "welcome.png",  
        class = "img-fluid",  
        height = "80vh",
        width = "auto",
        alt = "Welcome Image",
        style = "padding: 0vh; margin: 0vh;"
      )
    ),
    
    div(
      style = "text-align: center; margin-top: -60px;", 
      h1("Got a Housing Voucher?"),
      br(),
      h4("Philadelphia is a big city made up of smaller areas and neighborhoods. It can be hard to know where to look for a home with a housing voucher. This tool is designed to help you narrow down your housing search and determine your rent limit in different neighborhoods."),
      br(),
      actionButton("start_button", "Let's Go",  
                   class = "btn-custom")
    ),
    br(),
    div(
      style = "text-align: center;",
      p("This will take ~2 minutes")
    ),
    br()
  ),
  
##### Question panel #####
hidden(
  div(
    id = "main_content",
    # Remove sidebarLayout; use fluidRow for flexible layouts
    fluidRow(
      column(
        width = 12,
        uiOutput("question_ui") 
      )
    )
  )
),
  div(
    id = "footer",
    p("© 2024 Philly Neighborhood Explorer")
  )
)

#### DATA ####
nb <- st_read("panel.geojson") %>%
  st_transform(nb, crs = 4326)


# Load the neighborhood boundaries
neigh_bounds <- st_read("phl_neighs_2024.geojson")  %>%
  st_transform(neigh_bounds, crs = 4326)
neigh_bounds$neighborhood <- as.character(neigh_bounds$MAPNAME)

# Neighborhood lists by region
north_neighborhoods <- c("Juniata Park", "Northwood", "Upper Kensington", 
                         "Hunting Park", "Nicetown", "Tioga", "Ludlow", "Fairhill",
                         "Feltonville", "Logan", "Spring Garden", "Fairmount",
                         "North Central", "Franklinville", "West Kensington", "Hartranft",
                         "Brewerytown", "Northern Liberties", "Strawberry Mansion",
                         "Allegheny West", "Olney", "Stanton", "Glenwood", "McGuire", "Yorktown",
                         "Melrose Park Gardens", "Ogontz", "Fern Rock", "East Poplar",
                         "Francisville", "Sharswood", "Old Kensington", "West Poplar")

northwest_neighborhoods <- c("West Oak Lane", "East Oak Lane", "Chestnut Hill",
                             "East Germantown", "Southwest Germantown", "Roxborough",
                             "Manayunk", "West Mount Airy", "East Mount Airy", "Andorra",
                             "Cedarbrook", "East Falls", "Wissahickon", "Germany Hill",
                             "East Park", "Dearnley Park", "Upper Roxborough", "West Central Germantown",
                             "Germantown - Westside", "Germantown - Penn Knox", "Wister",
                             "Germantown - Morton", "Wissahickon Park")

northeast_neighborhoods <- c("Mayfair", "Tacony", "Holmesburg", "Fox Chase",
                             "Bustleton", "Somerton", "Oxford Circle", "Rhawnhurst",
                             "Bridesburg", "Fishtown - Lower Kensington",
                             "East Kensington", "Crescentville", "Riverfront", "Lawndale", "Modena",
                             "Millbrook", "Wissinoming", "Franklin Mills", "Parkwood Manor", "Byberry",
                             "Burholme", "Lexington Park", "Pennypack", "Academy Gardens", "Morrell Park",
                             "Pennypack Woods", "Aston-Woodbridge", "Torresdale", "Northeast Phila Airport",
                             "Normandy Village", "Harrowgate", "Richmond", "Frankford")

west_neighborhoods <- c("University City", "Wynnefield", "Overbrook", "Carroll Park", 
                        "Cobbs Creek", "Walnut Hill", "Spruce Hill", "Southwest Schuylkill",
                        "Wynnefield Heights", "East Parkside", "West Parkside", "Belmont", "Mantua",
                        "Haverford North", "Woodland Terrace", "Cedar Park", "Powelton", "West Powelton",
                        "Dunlap", "Haddington", "Mill Creek", "Garden Court")

center_city_neighborhoods <- c("Rittenhouse", "Logan Square", "Chinatown", "Callowhill",
                               "Society Hill", "Washington Square West", "Old City", "Graduate Hospital",
                               "Center City East", "Fitler Square")

south_neighborhoods <- c("Point Breeze", "Girard Estates", "Passyunk Square",
                         "Whitman", "Lower Moyamensing", "Packer Park", "Stadium District",
                         "Airport", "Navy Yard", "Bartram Village", "Industrial", "Dickinson Narrows",
                         "Pennsport", "Newbold", "East Passyunk", "Queen Village", "Hawthorne",
                         "Bella Vista", "West Passyunk", "Greenwich")

southwest_neighborhoods <- c("Kingsessing", "Elmwood", "Eastwick", "Penrose", "Paschall", "Grays Ferry",
                             "Clearview")

# List of regions
region_list <- list(
  "North" = north_neighborhoods,
  "Northwest" = northwest_neighborhoods,
  "Northeast" = northeast_neighborhoods,
  "West" = west_neighborhoods,
  "Center City" = center_city_neighborhoods,
  "South" = south_neighborhoods,
  "Southwest" = southwest_neighborhoods
)

# Neighborhood features
features <- c(
  "shootings_100k" = "Safety",
  "shopping" = "Shopping",
  "grocery" = "Grocery stores",
  "parks" = "Parks",
  "healthcare" = "Healthcare",
  "same_house_pct2022" = "Longtime residents",
  "vouchers" = "Voucher users"
)

# List of questions
question_info_list <- list(
  "shootings_100k" = list(
    question = "Safety",
    info = "A safe neighborhood has less crime and can help you feel more secure."
  ),
  "shopping" = list(
    question = "Commercial corridors",
    info = "Being close to shops means you can easily buy what you need, and the neighborhood might feel more lively."
  ),
  "grocery" = list(
    question = "Grocery stores",
    info = "Living near grocery stores makes it easier to buy fresh food and other essentials."
  ),
  "parks" = list(
    question = "Parks and green space",
    info = "Living near parks gives you a place to relax, exercise, and enjoy fresh air."
  ),
  "healthcare" = list(
    question = "Hospitals and clinics",
    info = "Living near healthcare providers makes it easier to see a doctor when you need to, especially if you need regular medical care."
  ),
  "same_house_pct2022" = list(
    question = "Longtime residents",
    info = "Neighborhoods where people have lived a long time often have a strong sense of community where neighbors know each other well."
  ),
  "vouchers" = list(
    question = "Voucher holder households",
    info = "In neighborhoods with more voucher holders, it might be easier to find landlords who accept vouchers."
  )
)

#### FUNCTIONS #### 
# Function to render progress bar
renderProgressBar <- function(percent) {
  tags$div(class = "progress",
           tags$div(class = "progress-bar",
                    role = "progressbar",
                    style = paste0("width: ", percent, "%;"),
                    `aria-valuenow` = percent,
                    `aria-valuemin` = "0",
                    `aria-valuemax` = "100"))}

#### SERVER ####
server <- function(input, output, session) {
  
  current_question <- reactiveVal(1)
  total_steps <- 5
  preferred_neighborhoods <- reactiveVal(c())
  household_size <- reactiveVal(NULL)
  annual_income <- reactiveVal(NULL)
  matched_5_neighs <- reactiveVal(NULL)
  selected_areas <- reactiveVal(c())
  
  ##### Start App #####
  observeEvent(input$start_button, {
    hide("welcome_panel")
    show("main_content")
    current_question(1)  
  })

  renderPreferredList <- function() {
    neighborhoods <- preferred_neighborhoods()
    if (length(neighborhoods) == 0) {
      return(p("No neighborhoods selected yet."))
    } else {
      return(
        div(class = "preferred-list",
            lapply(neighborhoods, function(nbh) {
              div(class = "preferred-item",
                  span(nbh),
                  actionButton(paste0("remove_", gsub(" ", "_", nbh)), "Remove", icon = icon("trash"), 
                               style = "padding: 3px 7px; font-size: 12px;")
              )
            })
        )
      )
    }
  }
  
  renderRecommendedList <- function(neighborhoods) {
    if (length(neighborhoods) == 0) {
      return(p("No neighborhoods match your preferences."))
    } else {
      return(
        div(class = "recommended-list",
            lapply(neighborhoods, function(nbh) {
              div(class = "recommended-item", nbh)
            })
        )
      )
    }
  }
  
  # Render UI for the questions and prep pages
  output$question_ui <- renderUI({
    
    current_q <- current_question()
    progress_percent <- ((current_q - 1) / (total_steps)) * 100
    
    if (current_q == 1) {
      tagList(
        renderProgressBar(progress_percent),
        div(class = "card",
            div(
              style = "text-align: center;",
              h2("Welcome to Philly Neighborhood Explorer!"),
              br(),
              p("We're here to help you find Philadelphia neighborhoods where you can use your housing voucher."),
              p("Let's get started by understanding what features are important to you.")
            ),
            br(),
            div(
              style = "text-align: center;",
              actionButton("next_info_1", "Let's Go",  
                           class = "btn-custom", style = "font-size: 16px; padding: 10px 20px;")
            )
        )
      )
    } else if (current_q == 2) {
      
      # Feature Importance Questions - All together on one page
      tagList(
        renderProgressBar(progress_percent),
        # Render each feature question in a separate div
        div(class = "card",
            h2("On a scale of 0 to 3, how important is it to have the following where you live?"),
            br(),
            lapply(names(features), function(feature_key) {
              current_feature_display <- features[[feature_key]]
              current_question_text <- question_info_list[[feature_key]]$question
              current_info_text <- question_info_list[[feature_key]]$info
              feature_input_id <- paste0("importance_", gsub(" ", "_", feature_key))
              image_src <- paste0(feature_key, ".png")
              
              div(
                style = "margin-bottom: 20px;",
                div(
                  style = "display: flex; align-items: left; justify-content: left;",
                  img(
                    src = image_src,
                    class = "img-fluid",
                    style = "width: 30px; height: 30px; margin-right: 15px; margin-top: 15px",
                    alt = paste0(current_feature_display, " Image")
                  ),
                  h3(current_question_text)
                ),
                p(current_info_text),
                sliderInput(
                  inputId = feature_input_id,
                  label = NULL,
                  min = 0,
                  max = 3,
                  value = 1
                )
              )
            }),
            br(),
              actionButton("back_feature", "Back", class = "btn-custom"),
              actionButton("next_feature", "Next", class = "btn-custom")
        ))
    } else if (current_q == 3) {
      # Preparation Page 2
      tagList(
        renderProgressBar(progress_percent),
        div(class = "card",
            div(
              style = "text-align: center;",
              h2("Almost There."),
              br(),
              p("Next, you'll select the neighborhoods you're interested in living. You can choose them by name or interact with the map."),
              p("This will help us tailor the best recommendations for you.")
            ),
            br(),
            div(
              style = "text-align: center;",
              actionButton("next_info_2", "Let's Go",  
                           class = "btn-custom", style = "font-size: 16px; padding: 10px 20px;")
            )
        )
      )
    } else if (current_q == 4) {
      # Household size
      tagList(
        renderProgressBar(progress_percent),
        div(class = "card",
            div(
              style = "text-align: center;",
              h2("Household Size"),
              br(),
              p("How many people will be living in this unit?"),
              p("This helps determine how much rent your voucher will cover.")
            ),
            br(),
            numericInput(
              inputId = "household_size",
              label = NULL,
              value = 1,
              min = 1,
              max = 9,
              step = 1
            ),
            br(),
            div(
              style = "text-align: center;",
              actionButton("back_hhsize", "Back", class = "btn-custom", style = "margin-right: 10px;"),
              actionButton("next_hhsize", "Next", class = "btn-custom")
            )
        )
      )
    } else if (current_q == 5) {
      # Annual income
      tagList(
        renderProgressBar(progress_percent),
        div(class = "card",
            div(
              style = "text-align: center;",
              h2("Estimated Annual Income"),
              br(),
              p("What is your estimated annual income?"),
              p("This helps us calculate your expected monthly contribution to your voucher unit.")
            ),
            br(),
            numericInput(
              inputId = "annual_income",
              label = NULL,
              value = 30000,
              min = 0,
              max = 150000,
              step = 5000
            ),
            br(),
            div(
              style = "text-align: center;",
              actionButton("back_income", "Back", class = "btn-custom", style = "margin-right: 10px;"),
              actionButton("next_income", "Next", class = "btn-custom")
            )
        )
      )
    } else if (current_q == 6) {
      # Results Page
      tagList(
        renderProgressBar(progress_percent),
        div(class = "card",
            div(
              style = "text-align: center;",
              h2("Your Neighborhood Matches"),
              br(),
              p("Based on your preferences, here are some neighborhoods you might like:")
            ),
            br(),
            # Leaflet map output
            leafletOutput("results_map", height = "600px"),
            div(
              class = "floating-card",
              h4("Your matches"),
              br(),
              div(
                h5("Neighborhoods for You"),
                tags$ol(
                  lapply(1:5, function(i) {
                    if (i <= nrow(neighs_matched())) {
                      tags$li(strong(neighs_matched()$neighborhood[i]))
                    } else {
                      tags$li("N/A")
                    }
                  })
                )
              ),
              br(),
              div(
                h5("Your Expected Monthly Payment"),
                p(textOutput("monthly_payment"))
              ),
              br(),
              actionButton("start_over", "Start Over", class = "btn-custom", style = "width: 100%;")
        )
      ))
    }
  })
  
  output$monthly_payment <- renderText({
    income <- annual_income()
    household_size_val <- household_size()
    
    # Check if inputs are available
    if (is.null(income) || is.null(household_size_val)) {
      return("N/A")
    }
    
    # Calculate contribution based on the provided logic
    if (income <= 167) {
      return("$50")
    }
    else if (household_size_val <= 2) {
      return(paste0("$", round(0.28 * income / 12), " to $", round(0.4 * income / 12)))
    }
    else if (household_size_val <= 5) {
      return(paste0("$", round(0.27 * income / 12), " to $", round(0.4 * income / 12)))
    }
    else {
      return(paste0("$", round(0.26 * income / 12), " to $", round(0.4 * income / 12)))
    }
  })
  
  #### Navigation Logic ####
  
  # INFO 1
  observeEvent(input$next_info_1, {
    current_question(2) 
  })
  
  observeEvent(input$back_info_1, {
    hide("main_content")
    show("welcome_panel")
  })
  
  # INFO 2
  observeEvent(input$next_info_2, {
    current_question(4)
  })
  observeEvent(input$back_info_2, {
    current_question(1)  
  })
  
  # FEATURE QS
  observeEvent(input$next_feature, {
    current_q <- current_question()
    feature_index <- current_q -1
    feature_keys <- names(features)
    current_feature_key <- feature_keys[feature_index]

    if (is.null(input[[paste0("importance_", gsub(" ", "_", current_feature_key))]])) {
      showModal(modalDialog(
        title = "Please select an answer.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else 
      current_question(3)
    
  })
  
  observeEvent(input$back_feature, {
    current_question(1)
  })
  
  # Navigation logic after Neighborhood Selection
  observeEvent(input$next_neigh_sel, {
    current_question(4)
  })
  
  observeEvent(input$back_neigh_sel, {
    current_question(2)
  })
  
  # HH Size
  observeEvent(input$next_hhsize, {
    if (is.null(input$household_size) || input$household_size < 1) {
      showModal(modalDialog(
        title = "Input Required",
        "Please enter a valid number of people living in the unit.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      # Store the input
      household_size(input$household_size)
      current_question(5)
    }
  })
  
  observeEvent(input$back_hhsize, {
    current_question(3)
  })
  
  # Annual income
  observeEvent(input$next_income, {
    if (is.null(input$annual_income) || input$annual_income < 0) {
      showModal(modalDialog(
        title = "Input Required",
        "Please enter a valid estimated annual income.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      # Store the input
      annual_income(input$annual_income)
      # Proceed to Results Page
      current_question(6)
    }
  })
  
  # Back Button Logic for Second New Question
  observeEvent(input$back_income, {
    current_question(4)  
  })
  
  # Restart
  observeEvent(input$start_over, {
    current_question(1)
    show("welcome_panel")
    hide("main_content")
    preferred_neighborhoods(c())
    household_size(NULL)
    annual_income(NULL)
  })
  
  
  #### Neighborhood Selection ####
  observeEvent(input$select_map, {
    showModal(modalDialog(
      title = "Select Preferred Areas via Map",
      size = "l",  # Large modal
      easyClose = TRUE,
      footer = tagList(
        actionButton("clear_map_selection", "Clear Selections", class = "btn-custom"),
        modalButton("Close"),
        actionButton("save_map_selection", "Save Selection", class = "btn-custom")
      ),
      leafletOutput("philly_map_selection", height = "600px")
    ))
  })
  
  observeEvent(input$select_list, {
    showModal(modalDialog(
      title = "Select Preferred Areas via List",
      size = "l",  # Large modal
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("save_list_selection", "Save Selection", class = "btn-custom")
      ),
      fluidPage(
        h4("Select regions and neighborhoods to include:"),
        br(),
        uiOutput("region_neighborhood_selection_ui")
      )
    ))
  })
  
  # Clear All Selections
  observeEvent(input$clear_all, {
    preferred_neighborhoods(c())
    showNotification("All selected neighborhoods have been cleared.", type = "message")
  })

  
  # Render the Leaflet map in the map selection modal
  output$philly_map_selection <- renderLeaflet({
    req(neigh_bounds)
    selected_neighborhoods <- selected_areas()
    leaflet(data = neigh_bounds,
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
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
        layerId = ~neighborhood,
        fillColor = ~ifelse(neighborhood %in% c(preferred_neighborhoods(), selected_neighborhoods), "salmon", "darkcyan"),
        color = "white",
        weight = 1,
        fillOpacity = 0.5,
        label = ~neighborhood,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Observe clicks on the map polygons in 'neigh_bounds' for selection
  observeEvent(input$philly_map_selection_shape_click, {
    clicked_area <- input$philly_map_selection_shape_click$id
    current_selection <- selected_areas()
    if (clicked_area %in% current_selection) {
      # If area is already selected, remove it
      current_selection <- setdiff(current_selection, clicked_area)
    } else {
      # Otherwise, add it
      current_selection <- c(current_selection, clicked_area)
    }
    selected_areas(current_selection)
    
    # Update the map to reflect the new selection
    leafletProxy("philly_map_selection") %>%
      clearShapes() %>%
      addPolygons(
        data = neigh_bounds,
        layerId = ~neighborhood,
        fillColor = ~ifelse(neighborhood %in% c(preferred_neighborhoods(), selected_areas()), "salmon", "darkcyan"),
        color = "white",
        weight = 1,
        fillOpacity = 0.5,
        label = ~neighborhood,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Observe the "Clear Selections" button in the map modal
  observeEvent(input$clear_map_selection, {
    selected_areas(c())
    leafletProxy("philly_map_selection") %>%
      clearShapes() %>%
      addPolygons(
        data = neigh_bounds,
        layerId = ~neighborhood,
        fillColor = ~ifelse(neighborhood %in% preferred_neighborhoods(), "salmon", "darkcyan"),
        color = "white",
        weight = 1,
        fillOpacity = 0.5,
        label = ~neighborhood,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Save the selected areas when the user clicks "Save Selection" in the map modal
  observeEvent(input$save_map_selection, {
    # Add the selected areas to the preferred_neighborhoods
    new_preferences <- selected_areas()
    updated_preferences <- unique(c(preferred_neighborhoods(), new_preferences))
    preferred_neighborhoods(updated_preferences)
    selected_areas(c())  # Reset the temporary selection
    removeModal()
  })
  
  # Render regions and neighborhoods with checkboxes for list selection
  output$region_neighborhood_selection_ui <- renderUI({
    lapply(names(region_list), function(region_name) {
      region_id <- paste0("region_", gsub(" ", "_", region_name))
      neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
      tagList(
        checkboxInput(region_id, strong(region_name)),
        div(
          class = "neighborhood-checkbox",
          checkboxGroupInput(
            inputId = neighborhood_input_id,
            label = NULL,
            choices = region_list[[region_name]],
            selected = intersect(region_list[[region_name]], preferred_neighborhoods())
          )
        ),
        hr()
      )
    })
  })
  
  # Observe changes in region checkboxes to select/deselect neighborhoods
  lapply(names(region_list), function(region_name) {
    region_id <- paste0("region_", gsub(" ", "_", region_name))
    neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
    observeEvent(input[[region_id]], {
      selected <- if (isTRUE(input[[region_id]])) region_list[[region_name]] else character(0)
      current_selected <- input[[neighborhood_input_id]]
      if (isTRUE(input[[region_id]])) {
        # Add all neighborhoods in the region to the selection
        updateCheckboxGroupInput(session, neighborhood_input_id,
                                 selected = unique(c(current_selected, region_list[[region_name]])))
      } else {
        # Remove all neighborhoods in the region from the selection
        updateCheckboxGroupInput(session, neighborhood_input_id,
                                 selected = setdiff(current_selected, region_list[[region_name]]))
      }
    }, ignoreInit = TRUE)
  })
  
  # Save the selected areas when the user clicks "Save Selection" in the list modal
  observeEvent(input$save_list_selection, {
    neighborhoods <- unlist(lapply(names(region_list), function(region_name) {
      neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
      input[[neighborhood_input_id]]
    }))
    
    updated_preferences <- unique(c(preferred_neighborhoods(), neighborhoods))
    preferred_neighborhoods(updated_preferences)
    
    removeModal()
  })
  
  # Handle removal of individual neighborhoods
  observe({
    lapply(preferred_neighborhoods(), function(nbh) {
      input_id <- paste0("remove_", gsub(" ", "_", nbh))
      # Use isolate to prevent unnecessary reactivity
      observeEvent(input[[input_id]], {
        updated_preferences <- setdiff(preferred_neighborhoods(), nbh)
        preferred_neighborhoods(updated_preferences)
      }, ignoreNULL = TRUE)
    })
  })
  
  #### Results Page ####
  neighs_matched <- reactive({
    # Collect user preferences and map to weights
    preference_weights <- sapply(names(features), function(feature_key) {
      response <- as.numeric(input[[paste0("importance_", gsub(" ", "_", feature_key))]])
      return(response)
    }, simplify = TRUE)
    
    names(preference_weights) <- names(features)
    
    preferred_neighborhoods_current <- preferred_neighborhoods()
    
    if (length(preferred_neighborhoods_current) > 0) {
      recommended_data <- nb[nb$neighborhood %in% preferred_neighborhoods_current, ]
    } else {
      recommended_data <- nb
    }
    
    # Ensure all necessary feature columns are present and numeric
    missing_features <- setdiff(names(features), colnames(recommended_data))
    if (length(missing_features) > 0) {
      stop(paste("The following feature columns are missing in recommended_data:", paste(missing_features, collapse = ", ")))
    }
    recommended_data_no_geom <- st_drop_geometry(recommended_data)
    feature_data <- recommended_data_no_geom[, names(features), drop = FALSE]
    for (feature in names(features)) {
      feature_data[[feature]] <- as.numeric(as.character(feature_data[[feature]]))
      feature_data[[feature]][is.na(feature_data[[feature]])] <- 0
    }
    
    # Calculate weighted scores
    weighted_features <- sweep(feature_data, 2, preference_weights[colnames(feature_data)], `*`)
    recommended_data$score <- rowMeans(weighted_features)
    
    # Find top 5 neighborhood matches
    recommended_data <- arrange(recommended_data, desc(score))
    top_neighborhoods <- head(recommended_data, 5)
    top_neighborhoods_centroids <- st_centroid(top_neighborhoods)
    top_neighborhoods_coords <- st_coordinates(top_neighborhoods_centroids)
    top_neighborhoods$lon <- top_neighborhoods_coords[, 1]
    top_neighborhoods$lat <- top_neighborhoods_coords[, 2]
    
    return(top_neighborhoods)
  })
  
  ##### Result Map ##### 
  output$results_map <- renderLeaflet({
    req(current_question() == 6)
    
    # Initialize the Leaflet map
    map <- leaflet(data = nb, options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = -75.13406, lat = 40.00761, zoom = 11) %>%
      setMaxBounds(
        lng1 = -75.28027,
        lat1 = 39.867,
        lng2 = -74.95576,
        lat2 = 40.13799
      ) %>%
      addPolygons(
        fillColor = "black",
        color = "transparent",
        weight = 0.1,
        fillOpacity = 0.1,
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.2,
          bringToFront = TRUE
        )
      ) %>%
      addPolygons(
        data = neighs_matched(),
        fillColor = "darkcyan",
        color = "white",
        dashArray = "3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~neighborhood,
        popup = ~paste0("<strong>", neighborhood, "</strong>
                        <br/>
                        Max rent: $", cost_2br),
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLabelOnlyMarkers(
        data = neighs_matched(),
        lng = ~lon,
        lat = ~lat,
        label = ~neighborhood,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "darkslategray",
            "font-size" = "12px",
            "background-color" = "rgba(255,255,255,0.7)",
            "padding" = "2px 4px",
            "border-radius" = "3px"
          )
        )
      )
    map
  })
  
##### Results sidebar card ##### 
output$recommended_neighborhoods_card <- renderUI({
  req(current_question() == (6 + length(features))) 
  matched_neigh <- neighs_matched()
  
  if (nrow(matched_neigh) == 0) {
    return(
      tags$div(
        tags$h4("No matching neighborhoods found based on your preferences.")
      )
    )
  }
  
  # Render the list of matched neighborhoods
  tags$div(
    tags$h4("Recommended Neighborhoods:"),
    tags$ol(
      lapply(1:nrow(matched_neigh), function(i) {
        tags$li(
          tags$strong(matched_neigh$neighborhood[i])
        )
      })
    )
  )
})
}

# Run the app
shinyApp(ui, server)
