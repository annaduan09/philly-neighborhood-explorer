# Load required libraries
library(shiny)
library(shinyjs)
library(conflicted)  # For conflict resolution
library(shinythemes)  # For themes
library(leaflet)      # For interactive maps
library(bslib)        # For advanced theming and styling
library(sf)           # For spatial data handling
library(RColorBrewer) # For color palettes

conflicts_prefer(shinyjs::show)

#### UI ####
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Include external resources in the header
  tags$head(
    # Link to Google Fonts
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap",
      rel = "stylesheet"
    ),
    # Include your custom CSS
    includeCSS("www/styles.css")
  ),
  
  # Welcome Panel
  div(
    id = "welcome_panel",
    img(
      src = "welcome.png",  # Ensure 'welcome.png' is inside 'www' directory
      class = "img-fluid",  # Bootstrap class for responsive images
      height = "80vh",
      width = "auto",
      alt = "Welcome Image",
      padding = "0vh",
      margin = "0vh"
    ),
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
    br()
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
# Use column names as keys and display names as values
features <- c(
  "restaurant" = "Restaurants",
  "grocery" = "Grocery stores",
  "shopping" = "Shopping",
  "parks" = "Parks",
  "healthcare" = "Healthcare",
  "kids" = "Families with kids",
  "same_house_pct2022" = "Longtime residents",
  "vouchers" = "Voucher users",
  "population2022" = "Population",
  "shootings_100k" = "Safety"
)

# List of questions and info texts
question_info_list <- list(
  "restaurant" = list(
    question = "Do you want to live near places to eat out?",
    info = "Living close to restaurants means you can easily go out to eat and have more choices."
  ),
  "grocery" = list(
    question = "Is it important for you to live near grocery stores?",
    info = "Living near grocery stores makes it easier to buy food and things you need every day."
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
  "kids" = list(
    question = "Do you want to live in a neighborhood with other families who have children?",
    info = "If you have kids, living near other families means your children can make friends nearby. It can also make the neighborhood feel safer."
  ),
  "same_house_pct2022" = list(
    question = "Is it important for you to live where people have lived for a long time?",
    info = "Neighborhoods with longtime residents often have a strong sense of community where neighbors know each other well."
  ),
  "vouchers" = list(
    question = "Do you want to live in a place where other people use housing vouchers?",
    info = "In neighborhoods with more voucher holders, it might be easier to find landlords who accept vouchers."
  ),
  "population2022" = list(
    question = "Do you prefer busy neighborhoods or quieter ones?",
    info = "Busy areas have more people and activities. Quieter areas have fewer people and might be more peaceful."
  ),
  "shootings_100k" = list(
    question = "Is living in a safe neighborhood important to you?",
    info = "A safe neighborhood has less crime and can help you feel more secure."
  )
)

#### SERVER ####
server <- function(input, output, session) {
  # Hide the welcome panel and show the main content when the button is clicked
  observeEvent(input$start_button, {
    hide("welcome_panel")
    show("main_content")
    current_question(1)  # Reset to first question
  })
  
  # Reactive value to keep track of the current question
  current_question <- reactiveVal(1)
  
  # Total number of steps (questions)
  total_steps <- 2 + length(features)  # 2 initial questions + number of features
  
  # Reactive value to store all excluded neighborhoods
  excluded_neighborhoods <- reactiveVal(c())
  
  # Function to render progress bar
  renderProgressBar <- function(percent) {
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
    current_q <- current_question()
    
    # Total number of steps (including features)
    total_steps <- 2 + length(features)  # 2 initial questions + number of features
    
    # Calculate progress percentage
    progress_percent <- (current_q / (total_steps + 1)) * 100  # +1 for results page
    
    if (current_q == 1) {
      # Question 1 UI
      tagList(
        # Progress Bar
        renderProgressBar(progress_percent),
        # Question Card
        div(class = "card",
            h2("Are there any areas you wouldn't want to live in?"),
            br(),
            p("Knowing which areas you'd like to avoid can help us recommend neighborhoods that are a better fit for you."),
            br(),
            radioButtons("exclude_areas", label = NULL, choices = c("Yes", "No"), inline = TRUE),
            br(),
            # Action Buttons
            div(
              actionButton("back_button", "Back", class = "btn-custom"),
              actionButton("next_button_q1", "Next", class = "btn-custom", style = "margin-left: 10px;")
            )
        )
      )
    } else if (current_q == 2) {
      # Question 2 UI (Exclusion Selection)
      tagList(
        # Progress Bar
        renderProgressBar(progress_percent),
        # Question Card
        div(class = "card",
            h2("Select Areas to Exclude"),
            br(),
            p("Selecting areas to exclude ensures that the recommended neighborhoods align with your preferences and requirements."),
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
                       actionButton("clear_all", "Clear All Exclusions", class = "btn-custom")
                     )
              )
            ),
            br(),
            # Action Buttons
            div(
              actionButton("back_button", "Back", class = "btn-custom"),
              actionButton("next_button_q2", "Next", class = "btn-custom", style = "margin-left: 10px;")
            )
        )
      )
    } else if (current_q >= 3 && current_q <= 2 + length(features)) {
      # Feature Importance Questions
      feature_index <- current_q - 2
      feature_keys <- names(features)
      current_feature_key <- feature_keys[feature_index]
      current_feature_display <- features[[current_feature_key]]
      current_question_text <- question_info_list[[current_feature_key]]$question
      current_info_text <- question_info_list[[current_feature_key]]$info
      feature_input_id <- paste0("importance_", gsub(" ", "_", current_feature_key))
      image_src <- paste0(current_feature_key, ".png")  # Construct image filename
      
      tagList(
        # Progress Bar
        renderProgressBar(progress_percent),
        # Question Card
        div(class = "card",
            h2(current_question_text),
            br(),
            p(current_info_text, class = "question-info-text"),
            br(),
            img(
              src = image_src,
              class = "img-fluid",
              style = "width: 5%; height: auto;",  # Set width to 50%
              alt = paste0(current_feature_display, " Image")
            ),
            br(),
            radioButtons(
              inputId = feature_input_id,
              label = NULL,
              choices = c("No", "Yes", "Very Much"),
              inline = TRUE
            ),
            br(),
            # Action Buttons
            div(
              actionButton("back_button", "Back", class = "btn-custom"),
              actionButton("next_button_feature", "Next", class = "btn-custom", style = "margin-left: 10px;")
            )
        )
      )
    } else if (current_q == total_steps + 1) {
      # Results Page UI
      tagList(
        # Progress Bar
        renderProgressBar(progress_percent),
        # Results Card
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
      current_question(3)  # Skip to the first feature question
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
  
  # Navigation logic for Feature Importance Questions
  observeEvent(input$next_button_feature, {
    current_q <- current_question()
    feature_index <- current_q - 2
    feature_keys <- names(features)
    current_feature_key <- feature_keys[feature_index]
    feature_input_id <- paste0("importance_", gsub(" ", "_", current_feature_key))
    
    # Check if the user has selected an option
    if (is.null(input[[feature_input_id]])) {
      showModal(modalDialog(
        title = "Selection Required",
        "Please select an option before proceeding.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      if (current_q < total_steps) {
        # Proceed to next feature question
        current_question(current_q + 1)
      } else {
        # All features have been asked; proceed to results
        current_question(total_steps + 1)
      }
    }
  })
  
  # Back Button Logic
  observeEvent(input$back_button, {
    current_q <- current_question()
    
    if (current_q > 1) {
      # Decrement current_question to go back
      current_question(current_q - 1)
    } else if (current_q == 1) {
      # If on the first question, go back to the welcome panel
      hide("main_content")
      show("welcome_panel")
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
  
  # [Rest of the server code remains the same, including the neighborhood exclusion map functionality]
  
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
        fillColor = ~ifelse(neighborhood %in% c(excluded_neighborhoods(), selected_neighborhoods), "salmon", "darkcyan"),
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
    req(current_question() == total_steps + 1)
    
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
    
    # For debugging purposes
    print("User Preference Weights:")
    print(preference_weights)
    
    # Get all excluded neighborhoods
    excluded_neighborhoods_current <- excluded_neighborhoods()
    
    # Exclude all geometries in 'nb' that have neighborhood names in excluded_neighborhoods
    recommended_data <- nb[!nb$neighborhood %in% excluded_neighborhoods_current, ]
    
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
    
    # Compute the weighted sum
    recommended_data$score <- rowSums(weighted_features)
    
    # Create color palette
    pal <- colorNumeric(palette = "YlGnBu", domain = recommended_data$score)
    
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
        fillColor = ~pal(score),
        color = "white",
        dashArray = "3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~paste0(neighborhood, ": Score ", round(score, 2)),
        popup = ~paste0("<strong>", neighborhood, "</strong><br/>Score: ", round(score, 2)),
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~score,
                title = "Neighborhood Match",
                opacity = 1)
  })
  
  # Generate and render the list of recommended neighborhoods
  output$recommended_neighborhoods <- renderUI({
    req(current_question() == total_steps + 1)
    
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
    
    # For debugging purposes
    print("User Preference Weights:")
    print(preference_weights)
    
    # Get all excluded neighborhoods
    excluded_neighborhoods_current <- excluded_neighborhoods()
    
    # Exclude all geometries in 'nb' that have neighborhood names in excluded_neighborhoods
    recommended_data <- nb[!nb$neighborhood %in% excluded_neighborhoods_current, ]
    
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
    
    # Compute the weighted sum
    recommended_data$score <- rowSums(weighted_features)
    
    # Aggregate scores by neighborhood (since nb may have multiple geometries per neighborhood)
    neighborhood_scores <- aggregate(score ~ neighborhood, data = recommended_data, FUN = mean)
    
    # Sort neighborhoods by score in descending order
    neighborhood_scores <- neighborhood_scores[order(-neighborhood_scores$score), ]
    
    # For debugging purposes
    print("Neighborhood Scores:")
    print(neighborhood_scores)
    
    # Render the list of recommended neighborhoods
    renderRecommendedList(neighborhood_scores$neighborhood)
  })
}

# Run the app
shinyApp(ui, server)
