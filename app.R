# Load required libraries
library(shiny)
library(shinyjs)
library(conflicted)  
library(shinythemes)  
library(leaflet)      
library(bslib)        
library(sf)           

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
      h4("Philadelphia is a big city made up of smaller areas and neighborhoods. 
         It can be hard to know where to look for a home with a housing voucher. 
         Answering a few questions might help you narrow down your housing search."),
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
      uiOutput("question_ui")
    )
  ),
  div(
    id = "footer",
    p("© 2024 Philly Neighborhood Explorer")
  )
)

#### DATA ####
nb <- st_read("panel.geojson")
# nb <- st_transform(nb, crs = 4326)

# Load the neighborhood boundaries
neigh_bounds <- st_read("phl_neighs_2024.geojson")  
neigh_bounds$neighborhood <- as.character(neigh_bounds$MAPNAME)

# neigh_bounds <- st_transform(neigh_bounds, crs = 4326)

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
  "restaurant" = "Restaurants",
  "shopping" = "Shopping",
  "parks" = "Parks",
  "healthcare" = "Healthcare",
  "same_house_pct2022" = "Longtime residents",
  "vouchers" = "Voucher users"
)

# List of questions
question_info_list <- list(
  "shootings_100k" = list(
    question = "Is living in a safe neighborhood important to you?",
    info = "A safe neighborhood has less crime and can help you feel more secure."
  ),
  "restaurant" = list(
    question = "Do you want to live near places to eat out?",
    info = "Living close to restaurants means you can easily go out to eat and have more choices."
  ),
  "shopping" = list(
    question = "Do you want to live near shops and stores?",
    info = "Being close to shops means you can easily buy what you need, and the neighborhood might feel more lively."
  ),
  "parks" = list(
    question = "Is it important for you to live near parks or green spaces?",
    info = "Living near parks gives you a place to relax, exercise, and enjoy fresh air."
  ),
  "healthcare" = list(
    question = "Do you want to live close to hospitals or clinics?",
    info = "Living near doctors or clinics makes it easier to see a doctor when you need to, especially if you need regular medical care."
  ),
  "same_house_pct2022" = list(
    question = "Is it important for you to live where people have lived for a long time?",
    info = "Neighborhoods with longtime residents often have a strong sense of community where neighbors know each other well."
  ),
  "vouchers" = list(
    question = "Do you want to live in a place where other people use housing vouchers?",
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
  total_steps <- length(features) + 6
  preferred_neighborhoods <- reactiveVal(c())
  household_size <- reactiveVal(NULL)
  annual_income <- reactiveVal(NULL)
  matched_5_neighs <- reactiveVal(NULL)
  
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
    } else if (current_q >= 2 && current_q <= (1 + length(features))) {
      
      # Feature Importance Questions
      feature_index <- current_q -1
      feature_keys <- names(features)
      current_feature_key <- feature_keys[feature_index]
      current_feature_display <- features[[current_feature_key]]
      current_question_text <- question_info_list[[current_feature_key]]$question
      current_info_text <- question_info_list[[current_feature_key]]$info
      feature_input_id <- paste0("importance_", gsub(" ", "_", current_feature_key))
      image_src <- paste0(current_feature_key, ".png")  
      
      tagList(
        renderProgressBar(progress_percent),
        # Question Card
        div(class = "card",
            div(
              style = "display: flex; align-items: center; justify-content: center;",
              img(
                src = image_src,
                class = "img-fluid",
                style = "width: 50px; height: auto; margin-right: 15px;",
                alt = paste0(current_feature_display, " Image")
              ),
              h2(current_question_text)
            ),
            br(),
            p(current_info_text),
            br(),
            radioButtons(
              inputId = feature_input_id,
              label = NULL,
              choices = c("No", "Yes", "Very Much"),
              inline = TRUE
            ),
            br(),
            div(
              style = "text-align: center;",
              actionButton("back_feature", "Back", class = "btn-custom", style = "margin-right: 10px;"),
              actionButton("next_feature", "Next", class = "btn-custom")
            )
        )
      )
    } else if (current_q == (2 + length(features))) {
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
    } else if (current_q == (3 + length(features))) {
      # Neighborhood Selection
      tagList(
        renderProgressBar(progress_percent),
        div(class = "card",
            div(
              style = "text-align: center;",
              h2("Select Your Preferred Neighborhoods"),
              br(),
              p("Choose the neighborhoods you'd prefer to live in. Select via the map or from the list below.")
            ),
            br(),
            fluidRow(
              column(6,
                     div(class = "selection-buttons",
                         actionButton("select_map", "Select via Map", class = "btn-selection", 
                                      style = "width: 100%; margin-bottom: 10px; padding: 15px; font-size: 14px;"),
                         actionButton("select_list", "Select via List", class = "btn-selection", 
                                      style = "width: 100%; padding: 15px; font-size: 14px;")
                     )
              ),
              column(6,
                     # Display preferred neighborhoods
                     h4("Currently Selected Neighborhoods:"),
                     renderPreferredList(),
                     br(),
                     div(
                       actionButton("clear_all", "Clear All", class = "btn-custom", 
                                    style = "padding: 10px 20px; font-size: 14px;")
                     )
              )
            ),
            br(),
            div(
              style = "text-align: center;",
              actionButton("back_neigh_sel", "Back", class = "btn-custom", style = "margin-right: 10px;"),
              actionButton("next_neigh_sel", "Next", class = "btn-custom")
            )
        )
      )
    } else if (current_q == (4 + length(features))) {
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
    } else if (current_q == (5 + length(features))) {
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
    } else if (current_q == (6 + length(features))) {
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
            p("Your expected monthly payment:", textOutput("monthly_payment"))
        )
      )
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
    current_question(3 + length(features))
  })
  observeEvent(input$back_info_2, {
    current_question(3 + length(features))  
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
    } else {
      if (current_q < (1 + length(features))) {
        current_question(current_q + 1)
      } else {
        current_question(current_q +1)
      }
    }
  })
  
  observeEvent(input$back_feature, {
    current_q <- current_question()
    if (current_q > 2) {
      current_question(current_q - 1)
    } else if (current_q == 2) {
      current_question(1)
    }
  })
  
  # Navigation logic after Neighborhood Selection
  observeEvent(input$next_neigh_sel, {
    current_question(4 + length(features))
  })
  
  observeEvent(input$back_neigh_sel, {
    current_q <- current_question()
    if (current_q > 3 + length(features)) {
      current_question(current_q -1)
    }
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
      current_question(5 + length(features))
    }
  })
  
  observeEvent(input$back_hhsize, {
    current_question(4 + length(features))
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
      current_question(6 + length(features))
    }
  })
  
  # Back Button Logic for Second New Question
  observeEvent(input$back_income, {
    current_question(5 + length(features))  
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
  
  # Reactive value to store selected neighborhoods from the map
  selected_areas <- reactiveVal(c())
  
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
  output
  output$results_map <- renderLeaflet({
    req(current_question() == (6 + length(features)))
    
    # Collect user preferences and map to weights
    preference_weights <- sapply(names(features), function(feature_key) {
      response <- input[[paste0("importance_", gsub(" ", "_", feature_key))]]
      if (response == "No") {
        return(0)
      } else if (response == "Yes") {
        return(1)
      } else if (response == "Very Much") {
        return(2)
      } else {
        return(0)
      }
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
    print(head(top_neighborhoods))
    top_neighborhoods_centroids <- st_centroid(top_neighborhoods)
    top_neighborhoods_coords <- st_coordinates(top_neighborhoods_centroids)
    top_neighborhoods$lon <- top_neighborhoods_coords[, 1]
    top_neighborhoods$lat <- top_neighborhoods_coords[, 2]
    
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
        data = top_neighborhoods,
        fillColor = "darkcyan",
        color = "white",
        dashArray = "3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~neighborhood,
        popup = ~paste0("<strong>", neighborhood, "</strong><br/>Score: ", round(score, 2)),
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLabelOnlyMarkers(
        data = top_neighborhoods,
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
  
  # Render the sidebar card listing the top 5 neighborhoods
  output$recommended_neighborhoods_card <- renderUI({
    req(current_question() == (6 + length(features)))
    
    # Collect user preferences and map to weights
    preference_weights <- sapply(names(features), function(feature_key) {
      response <- input[[paste0("importance_", gsub(" ", "_", feature_key))]]
      if (response == "No") {
        return(0)
      } else if (response == "Yes") {
        return(1)
      } else if (response == "Very Much") {
        return(2)
      } else {
        return(0)  # Default to 0 if no response
      }
    }, simplify = TRUE)
    
    names(preference_weights) <- names(features)
    
    # Get all preferred neighborhoods
    preferred_neighborhoods_current <- preferred_neighborhoods()
    
    if (length(preferred_neighborhoods_current) > 0) {
      # If preferred neighborhoods are selected, focus on them
      recommended_data <- nb[nb$neighborhood %in% preferred_neighborhoods_current, ]
    } else {
      # Else, use all neighborhoods
      recommended_data <- nb
    }
    
    # Ensure all necessary feature columns are present and numeric
    missing_features <- setdiff(names(features), colnames(recommended_data))
    if (length(missing_features) > 0) {
      stop(paste("The following feature columns are missing in recommended_data:", paste(missing_features, collapse = ", ")))
    }
    
    # Drop geometry to avoid issues with non-numeric data
    recommended_data_no_geom <- st_drop_geometry(recommended_data)
    
    # Extract feature data
    feature_data <- recommended_data_no_geom[, names(features), drop = FALSE]
    
    # Convert feature columns to numeric and replace NAs with zeros
    for (feature in names(features)) {
      feature_data[[feature]] <- as.numeric(as.character(feature_data[[feature]]))
      feature_data[[feature]][is.na(feature_data[[feature]])] <- 0
    }
    
    # Multiply each feature column by the corresponding weight
    weighted_features <- sweep(feature_data, 2, preference_weights[colnames(feature_data)], `*`)
    
    # Compute the weighted sum (rowMeans)
    recommended_data$score <- rowMeans(weighted_features)
    
    # Arrange the data by descending score
    recommended_data <- arrange(recommended_data, desc(score))
    
    # Select top 5 neighborhoods
    top_neighborhoods <- head(recommended_data, 5)
    
    # Render the list as a styled HTML element
    tags$div(
      tags$ol(
        lapply(1:nrow(top_neighborhoods), function(i) {
          tags$li(
            tags$strong(top_neighborhoods$tract_neigh[i]),
            ": Score ",
            round(top_neighborhoods$score[i], 2)
          )
        })
      )
    )
  })
}

# Run the app
shinyApp(ui, server)
