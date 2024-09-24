# Load required libraries
library(shiny)
library(shinyjs)
library(conflicted)  # For conflict resolution
library(shinythemes)  # For themes
library(leaflet)      # For interactive maps
library(bslib)        # For advanced theming and styling
library(sf)           # For spatial data handling

conflicts_prefer(shinyjs::show)

#### UI ####
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Include Google Fonts for a modern and playful look
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      /* Apply the Poppins font throughout the app */
      body, h1, h2, h3, h4, h5, h6, p, label, .btn {
        font-family: 'Poppins', sans-serif;
      }
      
      /* Customize the header */
      h1, h2, h3 {
        color: #2C3E50;
        font-weight: 600;
      }
      
      /* Style the action buttons */
      .btn-custom {
        background-color: #3498DB;
        color: #FFFFFF;
        border: none;
        font-size: 16px;
        padding: 12px 24px;
        margin-top: 20px;
        border-radius: 25px;
        transition: background-color 0.3s ease;
      }
      .btn-custom:hover {
        background-color: #2980B9;
      }
      
      /* Style the selection buttons */
      .btn-selection {
        background-color: #18BC9C;
        color: #FFFFFF;
        border: none;
        font-size: 14px;
        padding: 10px 20px;
        margin: 10px;
        border-radius: 20px;
        transition: background-color 0.3s ease;
      }
      .btn-selection:hover {
        background-color: #148F77;
      }
      
      /* Style the radio buttons */
      .shiny-input-radiogroup label {
        font-weight: 500;
        font-size: 16px;
        color: #34495E;
      }
      
      /* Style the main content area */
      #main_content {
        margin-top: 30px;
      }
      
      /* Style the welcome panel */
      #welcome_panel {
        text-align: center;
        margin-top: 50px;
      }
      
      /* Add a footer */
      #footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #2C3E50;
        color: white;
        text-align: center;
        padding: 10px;
        font-size: 14px;
      }
      
      /* Style for progress bar */
      .progress {
        margin-bottom: 20px;
        height: 20px;
        border-radius: 10px;
        overflow: hidden;
      }
      .progress-bar {
        background-color: #18BC9C;
      }
      
      /* Style cards */
      .card {
        background-color: #FFFFFF;
        padding: 30px;
        border-radius: 15px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        margin-bottom: 30px;
      }
      
      /* Indent neighborhoods */
      .neighborhood-checkbox {
        margin-left: 20px;
      }
      
      /* Style for excluded neighborhoods list */
      .excluded-list {
        background-color: #F8F9FA;
        padding: 15px;
        border-radius: 10px;
        max-height: 200px;
        overflow-y: auto;
        margin-top: 15px;
      }
      .excluded-item {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 8px 0;
        border-bottom: 1px solid #E9ECEF;
      }
      .excluded-item:last-child {
        border-bottom: none;
      }
      .excluded-item button {
        background-color: #E74C3C;
        color: white;
        border: none;
        padding: 5px 10px;
        border-radius: 3px;
        cursor: pointer;
        font-size: 12px;
      }
      .excluded-item button:hover {
        background-color: #C0392B;
      }
      
      /* Style for selection buttons container */
      .selection-buttons {
        text-align: center;
        margin-top: 20px;
      }
      
      /* Style for recommended neighborhoods list */
      .recommended-list {
        background-color: #F8F9FA;
        padding: 15px;
        border-radius: 10px;
        max-height: 200px;
        overflow-y: auto;
        margin-top: 15px;
      }
      .recommended-item {
        padding: 8px 0;
        border-bottom: 1px solid #E9ECEF;
        color: #34495E;
      }
      .recommended-item:last-child {
        border-bottom: none;
      }
      
      /* Enhance Leaflet map controls */
      .leaflet-control {
        font-family: 'Poppins', sans-serif;
      }
      
      /* Adjust image responsiveness */
      #welcome_panel img {
        max-width: 100%;
        height: auto;
        border-radius: 15px;
        max-height: 300px; /* Limit the maximum height */
      }
      
      /* Modal customizations */
      .modal-header {
        background-color: #3498DB;
        color: white;
        border-bottom: none;
        border-top-left-radius: 15px;
        border-top-right-radius: 15px;
      }
      .modal-footer {
        border-top: none;
      }
      .modal-content {
        border-radius: 15px;
      }
      
      /* Adjust checkbox group labels */
      .checkbox-group label {
        font-size: 14px;
        color: #34495E;
      }
      
      /* Adjust action buttons in modals */
      .modal .btn-custom, .modal .btn-selection {
        margin-top: 10px;
      }

      /* Background Image Styling */
      body::before {
        content: '';
        background-image: url('https://drexel.edu/~/media/Drexel/Core-Site-Group/Core/Images/life-at-drexel/philadelphia/philly-3200x1600/philly-3200x1600_160x53.jpg');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        opacity: 0.1; /* 10% opacity */
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        z-index: -1; /* Ensure the image stays behind all content */
      }
    "))
  ),
  
  # Welcome Panel
  div(
    id = "welcome_panel",
    br(), br(), br(), br(),
    h1("You got a voucher, now what?"),
    br(),
    h4("Find Philadelphia neighborhoods where you can use your housing choice voucher."),
    h4("We'll ask you a few questions to understand what you're looking for."),
    br(),
    # Center the button
    div(
      style = "text-align: center;",
      actionButton("start_button", "Get Started",
                   class = "btn-custom")
    ),
    br(),
    p("This will take ~2 minutes"),
    br(),
    img(
      src = "https://github.com/annaduan09/philly-neighborhood-explorer/blob/main/www/welcome.png?raw=true", 
      class = "img-fluid",  # Bootstrap class for responsive images
      style = "height: 80%; width: auto;",
      alt = "Welcome Image"
    )
  ),
  
  # Main Content Panel (initially hidden)
  hidden(
    div(
      id = "main_content",
      uiOutput("question_ui")
    )
  ),
  
  # Footer
  div(
    id = "footer",
    p("© 2024 Philly Neighborhood Explorer")
  )
)


#### DATA ####
# Load panel
nb <- st_read("data/panel.geojson")  # Original dataset with multiple geometries per neighborhood

# Ensure the 'neighborhood' column is character type
nb$neighborhood <- as.character(nb$neighborhood)

# Transform CRS of 'nb' to WGS84 (EPSG:4326)
nb <- st_transform(nb, crs = 4326)

# Load the neighborhood boundaries
neigh_bounds <- st_read("data/neighborhood/phl_neighs_2024.geojson")  # New dataset with single geometry per neighborhood

# Assign 'neighborhood' column using 'MAPNAME'
neigh_bounds$neighborhood <- as.character(neigh_bounds$MAPNAME)

# Transform CRS of 'neigh_bounds' to WGS84 (EPSG:4326)
neigh_bounds <- st_transform(neigh_bounds, crs = 4326)

# Neighborhood lists by region
north_neighborhoods <- c("Richmond", "Frankford", "Juniata Park", "Northwood",
                         "Harrowgate", "Hunting Park", "Nicetown", "Tioga",
                         "Feltonville", "Logan")

northwest_neighborhoods <- c("West Oak Lane", "East Oak Lane", "Chestnut Hill",
                             "East Germantown", "Southwest Germantown", "Roxborough",
                             "Manayunk", "West Mount Airy", "East Mount Airy")

northeast_neighborhoods <- c("Mayfair", "Tacony", "Holmesburg", "Fox Chase",
                             "Bustleton", "Somerton", "Oxford Circle", "Rhawnhurst")

west_neighborhoods <- c("University City", "Wynnefield", "Overbrook",
                        "Cobbs Creek", "Walnut Hill", "Spruce Hill")

center_city_neighborhoods <- c("Rittenhouse", "Logan Square", "Chinatown",
                               "Society Hill", "Washington Square West")

south_neighborhoods <- c("Point Breeze", "Girard Estates", "Passyunk Square",
                         "Whitman", "Lower Moyamensing")

southwest_neighborhoods <- c("Kingsessing", "Elmwood", "Eastwick")


# List of all neighborhoods and regions
region_list <- list(
  "North" = north_neighborhoods,
  "Northwest" = northwest_neighborhoods,
  "Northeast" = northeast_neighborhoods,
  "West" = west_neighborhoods,
  "Center City" = center_city_neighborhoods,
  "South" = south_neighborhoods,
  "Southwest" = southwest_neighborhoods
)

# Neighborhood features to rank/filter neighborhoods on
features <- c(
  "Restaurants" = "restaurant",
  "Grocery stores" = "grocery",
  "Shopping" = "shopping",
  "Parks" = "parks",
  "Healthcare" = "healthcare",
  "Families with kids" = "kids",
  "Longtime residents" = "same_house_pct2022",
  "Voucher users" = "vouchers",
  "Population" = "population2022",
  "Safety" = "shootings_100k"
)


features <- setNames(names(features), features)

#### SERVER ####
server <- function(input, output, session) {
  # Hide the welcome panel and show the main content when the button is clicked
  observeEvent(input$start_button, {
    hide("welcome_panel")
    show("main_content")
  })
  
  # Reactive value to keep track of the current question
  current_question <- reactiveVal(1)
  
  # Reactive value to store all excluded neighborhoods
  excluded_neighborhoods <- reactiveVal(c())
  
  # Function to render progress bar
  renderProgressBar <- function(step) {
    total_steps <- 4  # Updated total steps/questions
    percent <- (step / total_steps) * 100
    tags$div(class = "progress",
             tags$div(class = "progress-bar",
                      role = "progressbar",
                      style = paste0("width: ", percent, "%;"),
                      `aria-valuenow` = percent,
                      `aria-valuemin` = "0",
                      `aria-valuemax` = "100"
             )
    )
  }
  
  # Function to render excluded neighborhoods list
  renderExcludedList <- function() {
    neighborhoods <- excluded_neighborhoods()
    if (length(neighborhoods) == 0) {
      return(p("No neighborhoods excluded yet."))
    } else {
      return(
        div(class = "excluded-list",
            lapply(neighborhoods, function(nbh) {
              div(class = "excluded-item",
                  span(nbh),
                  actionButton(paste0("remove_", gsub(" ", "_", nbh)), "Remove", icon = icon("trash"), 
                               style = "padding: 3px 7px; font-size: 12px;")
              )
            })
        )
      )
    }
  }
  
  # Function to render recommended neighborhoods list
  renderRecommendedList <- function(neighborhoods) {
    if (length(neighborhoods) == 0) {
      return(p("No neighborhoods match your preferences based on the exclusions."))
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
  
  # Render UI for the questions
  output$question_ui <- renderUI({
    if (current_question() == 1) {
      # Question 1 UI
      fluidPage(
        renderProgressBar(1),
        div(class = "card",
            h2("Are there any areas you wouldn't want to live in?"),
            br(),
            radioButtons("exclude_areas", label = NULL, choices = c("Yes", "No"), inline = TRUE),
            br(),
            actionButton("next_button_q1", "Next", class = "btn-custom")
        ),
        div(class = "question_info",
            img(src = "www/info.png", 
                class = "img-fluid",  # Bootstrap class for responsive images
                style = "height: 3%; width: auto;",
                alt = "Welcome Image"),
            h4("Why we ask this question:"),
            p("Knowing which areas you'd like to avoid can help us recommend neighborhoods that are a better fit for you. 
              We'll use this information to exclude these areas from our recommendations.")
        ),
      )
    } else if (current_question() == 2) {
      # Question 2 UI (Exclusion Selection)
      fluidPage(
        renderProgressBar(2),
        div(class = "card",
            h2("Select Areas to Exclude"),
            br(),
            fluidRow(
              column(6,
                     # Selection Method Buttons
                     div(class = "selection-buttons",
                         actionButton("select_map", "Select via Map", class = "btn-selection"),
                         actionButton("select_list", "Select via List", class = "btn-selection")
                     )
              ),
              column(6,
                     # Display excluded neighborhoods
                     h4("Currently Excluded Neighborhoods:"),
                     renderExcludedList(),
                     br(),
                     div(
                       actionButton("add_more", "Add More Exclusions", class = "btn-custom"),
                       actionButton("clear_all", "Clear All Exclusions", class = "btn-custom", style = "margin-left: 10px;")
                     )
              )
            ),
            br(),
            # Next Button to proceed
            actionButton("next_button_q2", "Next", class = "btn-custom")
        )
      )
    } else if (current_question() == 3) {
      # Question 3 UI
      fluidPage(
        renderProgressBar(3),
        div(class = "card",
            h2("What's important to you in a neighborhood?"),
            p("For each feature below, please select how important it is to you."),
            br(),
            uiOutput("importance_ui"),
            actionButton("see_results", "See Results", class = "btn-custom")
        )
      )
    } else if (current_question() == 4) {
      # Results Page UI
      fluidPage(
        renderProgressBar(4),
        div(class = "card",
            h2("Recommended Neighborhoods"),
            p("Based on your preferences, here are some neighborhoods you might like:"),
            br(),
            # Leaflet map output
            leafletOutput("results_map", height = "700px"),
            br(),
            # List of recommended neighborhoods
            h4("Recommended Neighborhoods:"),
            uiOutput("recommended_neighborhoods"),
            br(),
            actionButton("start_over", "Start Over", class = "btn-custom")
        )
      )
    }
  })
  
  # Navigation logic after Question 1
  observeEvent(input$next_button_q1, {
    if (input$exclude_areas == "Yes") {
      current_question(2)  # Proceed to Question 2
    } else if (input$exclude_areas == "No") {
      current_question(3)  # Skip to Question 3
    } else {
      showModal(modalDialog(
        title = "Selection Required",
        "Please select 'Yes' or 'No' before proceeding.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Navigation logic after Question 2
  observeEvent(input$next_button_q2, {
    current_question(3)
  })
  
  # Navigation logic after Question 3
  observeEvent(input$see_results, {
    # Before proceeding, check if all preferences are selected
    preferences <- sapply(features, function(feature) {
      input[[paste0("importance_", gsub(" ", "_", feature))]]
    }, simplify = FALSE)
    
    # Check for missing selections
    missing_selections <- sapply(preferences, is.null)
    if (any(missing_selections)) {
      showModal(modalDialog(
        title = "Incomplete Selection",
        "Please select an importance level for each feature before proceeding.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      # Preferences are complete; proceed to results
      print(preferences)  # Replace with actual processing logic
      current_question(4)
    }
  })
  
  # Restart the app when "Start Over" is clicked
  observeEvent(input$start_over, {
    current_question(1)
    show("welcome_panel")
    hide("main_content")
    # Reset selections
    excluded_neighborhoods(c())
  })
  
  # Selection Method Buttons on Exclusion Page
  observeEvent(input$select_map, {
    showModal(modalDialog(
      title = "Select Areas to Exclude via Map",
      size = "l",  # Large modal
      easyClose = TRUE,
      footer = tagList(
        actionButton("clear_map_selection", "Clear Selections", class = "btn-custom"),
        modalButton("Close"),
        actionButton("save_map_selection", "Save Selection", class = "btn-custom")
      ),
      leafletOutput("philly_map_selection", height = "500px")
    ))
  })
  
  observeEvent(input$select_list, {
    showModal(modalDialog(
      title = "Select Areas to Exclude via List",
      size = "l",  # Large modal
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("save_list_selection", "Save Selection", class = "btn-custom")
      ),
      # Modal content: selection inputs
      fluidPage(
        h4("Select regions and neighborhoods to exclude:"),
        br(),
        uiOutput("region_neighborhood_selection_ui")
      )
    ))
  })
  
  # Clear All Exclusions
  observeEvent(input$clear_all, {
    excluded_neighborhoods(c())
    showNotification("All excluded neighborhoods have been cleared.", type = "message")
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
        fillColor = ~ifelse(neighborhood %in% excluded_neighborhoods(), "salmon", "darkcyan"),
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
        fillColor = ~ifelse(neighborhood %in% c(excluded_neighborhoods(), selected_areas()), "salmon", "darkcyan"),
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
        fillColor = ~ifelse(neighborhood %in% excluded_neighborhoods(), "salmon", "darkcyan"),
        color = "white",
        weight = 1,
        fillOpacity = 0.5,
        label = ~neighborhood,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Save the selected areas when the user clicks "Save Selection" in the map modal
  observeEvent(input$save_map_selection, {
    # Add the selected areas to the excluded_neighborhoods
    new_exclusions <- selected_areas()
    updated_exclusions <- unique(c(excluded_neighborhoods(), new_exclusions))
    excluded_neighborhoods(updated_exclusions)
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
            selected = intersect(region_list[[region_name]], excluded_neighborhoods())
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
    # Collect selected neighborhoods from each region
    neighborhoods <- unlist(lapply(names(region_list), function(region_name) {
      neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
      input[[neighborhood_input_id]]
    }))
    
    updated_exclusions <- unique(c(excluded_neighborhoods(), neighborhoods))
    excluded_neighborhoods(updated_exclusions)
    
    # For debugging purposes
    print("Excluded Neighborhoods from List:")
    print(excluded_neighborhoods())
    
    removeModal()
  })
  
  # Render the importance selection UI
  output$importance_ui <- renderUI({
    lapply(features, function(feature) {
      tagList(
        h4(feature),
        radioButtons(
          inputId = paste0("importance_", gsub(" ", "_", feature)),
          label = NULL,
          choices = c("Most Important", "Important", "Not Important"),
          inline = TRUE
        ),
        hr()
      )
    })
  })
  
  
  # Handle removal of individual excluded neighborhoods
  observe({
    lapply(excluded_neighborhoods(), function(nbh) {
      # Create unique input IDs by replacing spaces with underscores
      input_id <- paste0("remove_", gsub(" ", "_", nbh))
      # Use isolate to prevent unnecessary reactivity
      observeEvent(input[[input_id]], {
        updated_exclusions <- setdiff(excluded_neighborhoods(), nbh)
        excluded_neighborhoods(updated_exclusions)
      }, ignoreNULL = TRUE)
    })
  })
  
  # Render the results map on the results page
  output$results_map <- renderLeaflet({
    req(current_question() == 4)
    
    # Get all excluded neighborhoods
    excluded_neighborhoods_current <- excluded_neighborhoods()
    
    # Exclude all geometries in 'nb' that have neighborhood names in excluded_neighborhoods
    recommended_data <- nb[!nb$neighborhood %in% excluded_neighborhoods_current, ]
    
    # Create the map
    leaflet(data = recommended_data,
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
        fillColor = "darkcyan",
        color = "white",
        dashArray = "3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~neighborhood,
        popup = ~paste0("<strong>", neighborhood, "</strong>"),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
  })
  
  # Generate and render the list of recommended neighborhoods
  output$recommended_neighborhoods <- renderUI({
    req(current_question() == 4)
    
    # Get all excluded neighborhoods
    excluded_neighborhoods_current <- excluded_neighborhoods()
    
    # Exclude all geometries in 'nb' that have neighborhood names in excluded_neighborhoods
    recommended_data <- nb[!nb$neighborhood %in% excluded_neighborhoods_current, ]
    
    # Extract neighborhood names
    recommended_neighborhoods <- unique(recommended_data$neighborhood)
    
    renderRecommendedList(recommended_neighborhoods)
  })
}

# Run the app
shinyApp(ui, server)
