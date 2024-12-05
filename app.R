# Load required libraries
library(shiny)
library(rsconnect)
library(shinyjs)
library(conflicted)  
library(shinythemes)  
library(leaflet)      
library(bslib)        
library(sf)
library(tidyverse)
library(DT)
library(leaflet.extras)
library(shinyWidgets)

conflicts_prefer(dplyr::filter)
conflicts_prefer(shinyjs::show)

#### UI ####
ui <- fluidPage(
  useShinyjs(),  
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    includeCSS("www/styles.css")
  ),
  
  div(
    id = "app-header",
    h1("🏡 Philadelphia Neighborhood Explorer")
  ),
  
  div(
    id = "app-container",
    div(
      id = "welcome_panel",
      class = "welcome-panel",
        h1("Do you have a Housing Choice Voucher?"),
        h4("Philadelphia is a big city made up of smaller areas and neighborhoods. 
        It can be hard to know where to look for a home with a housing voucher. 
           This tool is designed to help you narrow down your housing search and determine your rent limit in different neighborhoods."),
        br(),
        actionButton("start_button", "Start",  
                     class = "btn-custom"),
      br(),
        p("This will take about 5 minutes."),
      br(),
    ),
    hidden(
      div(
        id = "main_content",
        class = "main-content",
        uiOutput("question_ui") 
      )
    )
  )
)

#### DATA ####
# Data panel
nb <- st_read("panel_2024.geojson") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG:4326") %>%
  mutate(across(starts_with("cost"), as.numeric)) %>%
  filter(tract != 42101006500) # empty tract causes bug on final result page


# ZIP boundaries
zip_bounds <- st_read("phl_zips_2024.geojson") %>%
  st_as_sf() %>%
  st_transform(crs = "EPSG:4326") %>%
  st_make_valid() %>%
  select(zip_code = CODE)

# ZIP lists by region

north_zips <- c(19121, 19122, 19133, 19132, 19140, 19141, 19120, 19126)

northwest_zips <- c(19131, 19129, 19127, 19128, 19118, 19119, 19144, 19138, 19150)

northeast_zips <- c(19125, 19134, 19137, 19124)

far_northeast_zips <- c(19135, 19149, 19111, 19152, 19136, 19114, 19115, 19116, 19154)

west_zips <- c(19151, 19139, 19104)

center_city_zips <- c(19130, 19123, 19103, 19102, 19107, 19106)

south_zips <- c(19146, 19147, 19145, 19148, 19112)

southwest_zips <- c(19143, 19142, 19153)

# List of regions
region_list <- list(
  "North" = north_zips,
  "Northwest" = northwest_zips,
  "Northeast" = northeast_zips,
  "Far Northeast" = far_northeast_zips,
  "West" = west_zips,
  "Center City" = center_city_zips,
  "South" = south_zips,
  "Southwest" = southwest_zips
)

# Neighborhood features
features <- c(
  "safety" = "Safety",
  "transit_density" = "Transit",
  "downtown_prox" = "Close to Center City",
  "shopping" = "Shops and Businesses",
  "grocery" = "Grocery stores",
  "parks" = "Parks",
  "healthcare" = "Healthcare",
  "same_house_pct2022" = "Longtime residents"
)

# List of questions
question_info_list <- list(
  "safety" = list(
    question = "🤝 Safety",
    info = "A safe neighborhood has less crime and can help you feel more secure."
  ),
  "transit_density" = list(
    question = "🚎 Transit",
    info = "Living near transit can make it easier to get around the city without a car."
  ),
  "downtown_prox" = list(
    question = "🌆 Close to Center City",
    info = "Neighborhoods close to Center City may have more amenities and opportunities."
  ),
  "shopping" = list(
    question = "🛍 Shops and Businesses",
    info = "Living close to shops and businesses can be convenient for shopping and activities."
  ),
  "grocery" = list(
    question = "🥕 Grocery stores",
    info = "Being near grocery stores makes it easier to buy fresh food and other essentials."
  ),
  "parks" = list(
    question = "🌳 Parks and green space",
    info = "Living near parks gives you a place to relax, exercise, and enjoy fresh air."
  ),
  "healthcare" = list(
    question = "🩺 Healthcare facilities",
    info = "Living near healthcare providers makes it easier to see a doctor when you need to."
  ),
  "same_house_pct2022" = list(
    question = "🏘️ Longtime residents",
    info = "Residents in these neighborhoods have been living here for longer."
  )
)

#### FUNCTIONS #### 
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
  observe({
    cat("Current question is:", current_question(), "\n")
  })
  
  total_steps <- 6
  preferred_zips <- reactiveVal(numeric())
  household_size <- reactiveVal(NULL)
  annual_income <- reactiveVal(NULL)
  
  income_limits <- c(
    "1" = 64250, "2" = 73400, "3" = 82600, "4" = 91750,
    "5" = 99100, "6" = 106450, "7" = 113800, "8" = 121150
  )
  
  
  matched_5_neighs <- reactiveVal(NULL)
  
  # Prevent infinite loops
  update_in_progress <- reactiveValues(region = FALSE, zip = FALSE)
  
  ##### Start App #####
  observeEvent(input$start_button, {
    hide("welcome_panel")
    show("main_content")
    current_question(1)  
  })
  
  renderPreferredList <- function() {
    zips <- preferred_zips()
    if (length(zips) == 0) {
      return(p("No zips selected yet."))
    } else {
      return(
        div(class = "preferred-list",
            lapply(zips, function(nbh) {
              div(class = "preferred-item",
                  span(nbh),
                  actionButton(paste0("remove_", gsub(" ", "_", nbh)), "Remove", icon = icon("trash"))
              )
            })
        )
      )
    }
  }
  
  renderRecommendedList <- function(neighborhoods) {
    if (length(neighborhoods) == 0) {
      return(p("No areas match your preferences."))
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
        h1("Getting started"),
        br(),
        h4("We'll start by learning what neighborhood features are important to you. 
            The following page will list some neighborhood features that are important to many people. For each feature, you'll rate how important it is to you. 
               After you've rated all the features, we'll show you neighborhoods that match your preferences."),
        br(),
              actionButton("next_info_1", "I'm ready",  
                           class = "btn-custom")
      )
    } else if (current_q == 2) {
      
      tagList(
        renderProgressBar(progress_percent),
        
        h1("How important is it to have the following where you live?"),
        br(),
        
        tagList(
          lapply(names(features), function(feature_key) {
            current_feature_display <- features[[feature_key]]
            current_question_text <- question_info_list[[feature_key]]$question
            current_info_text <- question_info_list[[feature_key]]$info
            feature_input_id <- paste0("importance_", gsub(" ", "_", feature_key))
            
            tagList(
                h3(current_question_text),
              p(id = "question-info", current_info_text),
              sliderTextInput(
                inputId = feature_input_id,
                label = NULL,
                choices = c("Less", "Somewhat", "More", "Most"),
                grid = FALSE,
                selected = "Somewhat"
              ),
              br()
            )
          })
        ),
        
        br(),
        actionButton("back_feature", "Back", class = "btn-custom"),
        actionButton("next_feature", "Next", class = "btn-custom")
      )
    } else if (current_q == 3) {
      # Preparation Page 2
      tagList(
        renderProgressBar(progress_percent),
              h1("Neighborhood Preferences"),
              br(),
              h4("Next, we'll ask you to select places you'd prefer to live in Philadelphia. 
              You have two options: you can select the places you'd like to consider by either selecting them on the menu to the left or clicking them on the map to the right. 
              You can also click 'Select All' to include the whole city, or click 'Clear All' to start over. 
                 This will help us show you the best recommendations."),
            div(
              actionButton("next_info_2", "I'm ready",
                           class = "btn-custom")
            )
      )
    } else if (current_q == 4) {
      tagList(
        renderProgressBar(progress_percent),
              h1("Preferred Neighborhoods"),
              h4("You can use the menu below or click on the map."),
              actionButton("select_all", "Select All", class = "btn-custom"),
              actionButton("clear_all", "Clear All", class = "btn-custom"),
            br(),
        fluidRow(
          column(width = 4,
                 uiOutput("region_zip_selection_ui")  
          ),
          column(width = 8,
                 leafletOutput("philly_map_selection", height = "600px")     
          )
        ),
            br(),
            div(
              actionButton("back_neigh_sel", "Back", class = "btn-custom"),
              actionButton("next_neigh_sel", "Next", class = "btn-custom")
            )
      )
    } else if (current_q == 5) {
      tagList(
        renderProgressBar(progress_percent),
              h1("Household Size"),
              br(),
              h4("How many people will be living in this unit?"),
              h4("This helps determine how much rent your voucher will cover."),
            br(),
            numericInput(
              inputId = "household_size",
              label = NULL,
              value = 3,
              min = 1,
              max = 8,
              step = 1
            ),
            br(),
              actionButton("back_hhsize", "Back", class = "btn-custom"),
              actionButton("next_hhsize", "Next", class = "btn-custom")
      )
    } else if (current_q == 6) {
      tagList(
        renderProgressBar(progress_percent),
              h1("Annual Income"),
              br(),
              h4("What is your estimated annual income?"),
              h4("This helps us calculate your expected monthly contribution to your voucher unit."),
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
              actionButton("back_income", "Back", class = "btn-custom"),
              actionButton("next_income", "Next", class = "btn-custom")
      )
    } else if (current_q == 7) {
      tagList(
        renderProgressBar(progress_percent),
            h1("Neighborhoods for you"),
            br(),
            h4("Based on your preferences, here are some neighborhoods you may like:"),
            uiOutput("neighborhood_details_table"),
            br(),
            h4("Also consider these neighborhoods which match for your neighborhood preferences, but fall outside of your search area."),
            uiOutput("neighborhood_recs_table"),
            br(),
            h4("See your neighborhood matches on the map below. Click on a neighborhood to see more details."),
            br(),
            leafletOutput("results_map", height = "600px"),
              actionButton("start_over", "Start Over", class = "btn-custom")
      )
    }
  })
  
  monthly_payment <- reactive({
    
    income <- annual_income()
    household_size_val <- household_size()
    
    if (is.null(income) || is.null(household_size_val) || is.na(income) || is.na(household_size_val)) {
      return(0)  
    }

    if (income <= 167) {
      return(50)
    }
    else if (household_size_val <= 2) {
      return(round(0.28 * income / 12))
    }
    else if (household_size_val <= 5) {
      return(round(0.27 * income / 12))
    }
    else {
      return(round(0.26 * income / 12))
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
    current_question(2)  
  })
  
  # FEATURE QS
  label_to_value <- c("Less" = 0, "Somewhat" = 1, "More" = 2, "Most" = 3)
  
  observeEvent(input$next_feature, {
    current_q <- current_question()
    feature_index <- current_q -1
    feature_keys <- names(features)
    current_feature_key <- feature_keys[feature_index]
    
    
    selected_label <- input[[paste0("importance_", gsub(" ", "_", current_feature_key))]]
    
    selected_value <- label_to_value[selected_label]
    
    # Now `selected_value` contains the numeric value (0, 1, 2, or 3)
    cat("Selected value for", current_feature_key, "is", selected_value, "\n")
    
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
    current_question(5)
  })
  
  observeEvent(input$back_neigh_sel, {
    current_question(3)
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
      household_size(input$household_size)
      current_question(6)
    }
  })
  
  observeEvent(input$back_hhsize, {
    current_question(4)
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
      household_size_val <- household_size()
      annual_income_val <- input$annual_income
      max_income <- income_limits[as.character(household_size_val)]
      
      # Check if income exceeds the limit for the household size
      if (!is.null(max_income) && annual_income_val > max_income) {
        showModal(modalDialog(
          title = "Income Limit Exceeded",
          paste0(
            "Your annual income exceeds the maximum allowed income of $", 
            formatC(max_income, format = "f", digits = 0, big.mark = ","), 
            " for a household size of ", household_size_val, "."
          ),
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        annual_income(input$annual_income)
        current_question(7)  # Proceed to the next step if income is valid
      }
    }
  })
  
  # Back Button Logic for Second New Question
  observeEvent(input$back_income, {
    current_question(5)  
  })
  
  # Restart
  observeEvent(input$start_over, {
    current_question(1)
    show("welcome_panel")
    hide("main_content")
    preferred_zips(c())
    household_size(NULL)
    annual_income(NULL)
  })
  
  #### Neighborhood Selection ####
  
  # Select All
  observeEvent(input$select_all, {
    all_zips <- unlist(region_list)  
    preferred_zips(all_zips) 
    
    # Update each region's checkbox group input
    lapply(names(region_list), function(region_name) {
      zip_input_id <- paste0("zips_", gsub(" ", "_", region_name))
      updateCheckboxGroupInput(session, zip_input_id,
                               selected = region_list[[region_name]])
    })
    
    # Update the "Select All" checkboxes for each region
    lapply(names(region_list), function(region_name) {
      region_id <- paste0("region_", gsub(" ", "_", region_name))
      updateCheckboxInput(session, region_id, value = TRUE)
    })
    showNotification("All ZIP codes have been selected.", type = "message")
  })
  
  # Clear All Selections
  observeEvent(input$clear_all, {
    preferred_zips(character())  # Clear all preferred neighborhoods
    
    # Clear checkboxes for each region
    lapply(names(region_list), function(region_name) {
      zip_input_id <- paste0("zips_", gsub(" ", "_", region_name))
      updateCheckboxGroupInput(session, zip_input_id, selected = character(0))  # Clear all checkboxes
    })
    
    # Reset the "Select All" checkboxes for each region
    lapply(names(region_list), function(region_name) {
      region_id <- paste0("region_", gsub(" ", "_", region_name))
      updateCheckboxInput(session, region_id, value = FALSE)
    })
    
    showNotification("All selected ZIP codes have been cleared.", type = "message")
  })
  
  # Neighborhood selection leaflet map
  output$philly_map_selection <- renderLeaflet({
    req(zip_bounds)
    leaflet(data = zip_bounds,
            options = leafletOptions(minZoom = 11, maxZoom = 11)) %>%
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
        group = 'zips',
        layerId = ~zip_code,
        fillColor = ~ifelse(zip_code %in% preferred_zips(), "cyan3", "darkcyan"),
        color = "white",
        weight = 1,
        fillOpacity = 0.5,
        label = ~zip_code,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })
  
  observeEvent(input$philly_map_selection_shape_click, {
    click <- input$philly_map_selection_shape_click
    clicked_zip <- click$id
    
    current_preferences <- preferred_zips()
    
    if (clicked_zip %in% current_preferences) {
      
      new_preferences <- setdiff(current_preferences, clicked_zip)
    } else {
      new_preferences <- c(current_preferences, clicked_zip)
    }
    preferred_zips(new_preferences)
  })
  
  # Update the map when preferences change
  observeEvent(preferred_zips(), {
    leafletProxy("philly_map_selection") %>%
      clearGroup('zips') %>%
      addPolygons(
        data = zip_bounds,
        group = 'zips',
        layerId = ~zip_code,
        fillColor = ~ifelse(zip_code %in% preferred_zips(), "cyan3", "darkcyan"),
        color = "white",
        weight = 1,
        fillOpacity = 0.5,
        label = ~zip_code,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })


# Render regions and zips with enhanced dropdown appearance
  output$region_zip_selection_ui <- renderUI({
    lapply(names(region_list), function(region_name) {
      region_id <- paste0("region_", gsub(" ", "_", region_name))
      zip_input_id <- paste0("zips_", gsub(" ", "_", region_name))
      selected_zips <- intersect(region_list[[region_name]], preferred_zips())
      all_selected <- length(selected_zips) == length(region_list[[region_name]])
      
      div(
        class = "region-container",
        checkboxInput(region_id, label = region_name, value = all_selected),
        checkboxGroupInput(
          inputId = zip_input_id,
          label = NULL,
          choices = region_list[[region_name]],
          selected = selected_zips,
          inline = TRUE
        )
      )
    })
  })

  
  # Observe changes in region checkboxes to select/deselect neighborhoods
  lapply(names(region_list), function(region_name) {
    region_id <- paste0("region_", gsub(" ", "_", region_name))
    zip_input_id <- paste0("zips_", gsub(" ", "_", region_name))
    
    observeEvent(input[[region_id]], {
      if (update_in_progress$zip) return()
      update_in_progress$region <- TRUE
      
      # If "Select all" is checked, select all neighborhoods in the region; otherwise, deselect all
      selected <- if (isTRUE(input[[region_id]])) region_list[[region_name]] else character(0)
      
      # Update the neighborhood checkboxes
      updateCheckboxGroupInput(session, zip_input_id, selected = selected)
      update_in_progress$region <- FALSE
    }, ignoreInit = TRUE)
  })

  # Update region checkboxes when neighborhood selections change
  lapply(names(region_list), function(region_name) {
    zip_input_id <- paste0("zips_", gsub(" ", "_", region_name))
    region_id <- paste0("region_", gsub(" ", "_", region_name))
    
    observeEvent(input[[zip_input_id]], {
      if (update_in_progress$region) return()
      update_in_progress$zip <- TRUE
      
      # Get selected neighborhoods
      selected_zips <- input[[zip_input_id]]
      
      # Check if all neighborhoods in the region are selected
      all_selected <- length(selected_zips) == length(region_list[[region_name]])
      
      # Update the "Select all" checkbox based on whether all neighborhoods are selected
      updateCheckboxInput(session, region_id, value = all_selected)
      
      # Update preferred neighborhoods
      zips <- unlist(lapply(names(region_list), function(r_name) {
        n_input_id <- paste0("zips_", gsub(" ", "_", r_name))
        input[[n_input_id]]
      }))
      
      preferred_zips(unique(zips))
      update_in_progress$zip <- FALSE
    }, ignoreInit = TRUE)
  })
  
  
  # Synchronize list selections with preferences
  observeEvent(preferred_zips(), {
    lapply(names(region_list), function(region_name) {
      zip_input_id <- paste0("zips_", gsub(" ", "_", region_name))
      selected_zips <- intersect(region_list[[region_name]], preferred_zips())
      all_selected <- length(selected_zips) == length(region_list[[region_name]])
      updateCheckboxGroupInput(session, zip_input_id,
                               selected = selected_zips)
      updateCheckboxInput(session, paste0("region_", gsub(" ", "_", region_name)), value = all_selected)
    })
  })
  
  #### Results Page ####
  ##### Matched neighborhoods ##### 
  neighs_matched <- reactive({
  
    # Collect user preferences and map to weights
    preference_weights <- sapply(names(features), function(feature_key) {
      response <- as.numeric(input[[paste0("importance_", gsub(" ", "_", feature_key))]])
      return(response)
    }, simplify = TRUE)
    
    names(preference_weights) <- names(features)
    
    preferred_zips_current <- preferred_zips()
    
    if (length(preferred_zips_current) > 0) {
      recommended_data <- nb[nb$zip_code %in% preferred_zips_current, ]
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
  
  
  ##### Recommended neighborhoods #####
  neighs_rec <- reactive({
    
    # Collect user preferences and map to weights
    preference_weights <- sapply(names(features), function(feature_key) {
      response <- as.numeric(input[[paste0("importance_", gsub(" ", "_", feature_key))]])
      return(response)
    }, simplify = TRUE)
    
    names(preference_weights) <- names(features)
    
    matched_zips <- neighs_matched()$zip_code
    
    # Exclude only the matched ZIP codes from recommendations
    recommended_data <- nb[!nb$zip_code %in% matched_zips, ]
    
    # If no neighborhoods are left, include all neighborhoods (even matched ones)
    if (nrow(recommended_data) == 0) {
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
    
    # Exclude the matched neighborhoods from the recommendations, if they were added back
    if (nrow(recommended_data) > 0 && any(recommended_data$zip_code %in% matched_zips)) {
      recommended_data <- recommended_data[!recommended_data$zip_code %in% matched_zips, ]
    }
    
    # Find top 5 neighborhood matches
    recommended_data <- recommended_data %>% arrange(desc(score))
    top_neighborhoods <- head(recommended_data, 5)
    
    top_neighborhoods_centroids <- st_centroid(top_neighborhoods)
    top_neighborhoods_coords <- st_coordinates(top_neighborhoods_centroids)
    top_neighborhoods$lon <- top_neighborhoods_coords[, 1]
    top_neighborhoods$lat <- top_neighborhoods_coords[, 2]
    
    return(top_neighborhoods)
  })
  
  
  ##### Result Map ##### 
  output$results_map <- renderLeaflet({
    
    rec_neigh <- neighs_rec()
    matched_neigh <- neighs_matched()
    
    hh_size <- household_size()
    
    max_rent_match = case_when(
      hh_size == 1 ~ matched_neigh$cost_1br,
      hh_size == 2 ~ matched_neigh$cost_2br,
      hh_size == 3 ~ matched_neigh$cost_3br,
      hh_size == 4 ~ matched_neigh$cost_4br,
      hh_size == 5 ~ matched_neigh$cost_5br,
      hh_size == 6 ~ matched_neigh$cost_6br,
      hh_size == 7 ~ matched_neigh$cost_7br,
      hh_size == 8 ~ matched_neigh$cost_8br
    )
    
    max_rent_rec = case_when(
      hh_size == 1 ~ rec_neigh$cost_1br,
      hh_size == 2 ~ rec_neigh$cost_2br,
      hh_size == 3 ~ rec_neigh$cost_3br,
      hh_size == 4 ~ rec_neigh$cost_4br,
      hh_size == 5 ~ rec_neigh$cost_5br,
      hh_size == 6 ~ rec_neigh$cost_6br,
      hh_size == 7 ~ rec_neigh$cost_7br,
      hh_size == 8 ~ rec_neigh$cost_8br
    )
    
    monthly_payment_value <- monthly_payment() 
    
    
    pal <- colorFactor(
      palette = c("darkcyan", "darkslategray"), 
      domain = c("Matched", "Recommended")
    )
    
    req(current_question() == 7)
    monthly_payment <- monthly_payment()
  
    cat("Monthly payment: ", monthly_payment, "\n")
    
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
        label = ~tract_neigh,
        popup = ~paste0("<h2>", tract_neigh, "</h2>
                <br/>
                <strong>ZIP Code:</strong> ", zip_code,
                        "<br/>
                <strong>Max rent:</strong> $", max_rent_match,
                        "<br/>
                <strong>You pay:</strong> $", monthly_payment_value,
                        "<br/>
                <strong>HUD pays:</strong> $", as.numeric(max_rent_match) - as.numeric(monthly_payment_value),
                        "<br/>
                <strong>Elementary School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", es_catchment, "</a>",
                        "<br/>
                <strong>Middle School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", ms_catchment, "</a>",
                        "<br/>
                <strong>High School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", hs_catchment, "</a>",
                        "<br/>
                <strong>SEPTA Lines:</strong> <a href='https://plan.septa.org/#/' target='_blank'>", transit_line_names, "</a>"),
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        group = "matched"
      ) %>%
      addPolygons(
        data = neighs_rec(),
        fillColor = "darkslategray",
        color = "white",
        dashArray = "3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~tract_neigh,
        popup = ~paste0("<h2>", tract_neigh, "</h2>
                <br/>
                <strong>ZIP Code:</strong> ", zip_code,
                        "<br/>
                <strong>Max rent:</strong> $", max_rent_rec,
                        "<br/>
                <strong>You pay:</strong> $", monthly_payment_value,
                        "<br/>
                <strong>HUD pays:</strong> $", as.numeric(max_rent_rec) - as.numeric(monthly_payment_value),
                        "<br/>
                <strong>Elementary School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", es_catchment, "</a>",
                        "<br/>
                <strong>Middle School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", ms_catchment, "</a>",
                        "<br/>
                <strong>High School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", hs_catchment, "</a>",
                        "<br/>
                <strong>SEPTA Lines:</strong> <a href='https://plan.septa.org/#/' target='_blank'>", transit_line_names, "</a>"),
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        group = "recommended"
      ) %>%
      addLabelOnlyMarkers(
        data = neighs_matched(),
        lng = ~lon,
        lat = ~lat,
        label = ~tract_neigh,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE 
        )
      ) %>%
      addLabelOnlyMarkers(
        data = neighs_rec(),
        lng = ~lon,
        lat = ~lat,
        label = ~tract_neigh,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE  
        )
      ) %>%
      addLegend(
        position = "bottomleft", 
        pal = pal, 
        values = c("Matched", "Recommended")
      )
    map
  })
  
  #### Data table for Neighborhood Details ####
  ##### Matched ##### 
  output$neighborhood_details_table <- renderUI({
    matched_neigh <- neighs_matched()
    hh_size <- household_size()
    
    # determine max rent in neighborhood to be based on household size
    max_rent = case_when(
      hh_size == 1 ~ matched_neigh$cost_1br,
      hh_size == 2 ~ matched_neigh$cost_2br,
      hh_size == 3 ~ matched_neigh$cost_3br,
      hh_size == 4 ~ matched_neigh$cost_4br,
      hh_size == 5 ~ matched_neigh$cost_5br,
      hh_size == 6 ~ matched_neigh$cost_6br,
      hh_size == 7 ~ matched_neigh$cost_7br,
      hh_size == 8 ~ matched_neigh$cost_8br
    )
    
    monthly_payment_value <- monthly_payment() 
    
    # Prepare table data
    details_table <- data.frame(
      Neighborhood = matched_neigh$tract_neigh,
      ZIP_Code = matched_neigh$zip_code,
      Max_Rent = paste0("$", formatC(max_rent, big.mark = ",")),
      HUD_Pays = paste0("$", formatC(max_rent - monthly_payment_value, big.mark = ",")),
      Your_Contribution = paste0("$", formatC(as.numeric(monthly_payment_value), big.mark = ","))
    )
    
    # Create HTML table
    table_html <- paste(
      "<table style='width:50%; border-collapse: collapse;'>",
      "<thead><tr style='background-color: darkcyan; color: white;'>",
      "<th>Neighborhood</th><th>ZIP Code</th><th>Max Rent</th><th>HUD pays</th><th>You Pay</th>",
      "</tr></thead>",
      "<tbody>",
      paste(
        apply(details_table, 1, function(row) {
          paste0("<tr style='border: transparent; padding: 8px;'>",
                 "<td>", row[1], "</td>",
                 "<td>", row[2], "</td>",
                 "<td>", row[3], "</td>",
                 "<td>", row[4], "</td>",
                 "<td>", row[5], "</td>",
                 "</tr>")
        }),
        collapse = ""
      ),
      "</tbody></table>"
    )
    
    HTML(
      paste0(
        "<div style='max-height: 200px; overflow-y: auto;'>", table_html, "</div>"
      )
    )
  })
  
  ##### Recommended ##### 
  output$neighborhood_recs_table <- renderUI({
    rec_neigh <- neighs_rec()
    hh_size <- household_size()
    
    if (nrow(rec_neigh) == 0) {
      return(h4("No additional recommendations available."))
    }
    
    # determine max rent in neighborhood to be based on household size
    max_rent = case_when(
      hh_size == 1 ~ rec_neigh$cost_1br,
      hh_size == 2 ~ rec_neigh$cost_2br,
      hh_size == 3 ~ rec_neigh$cost_3br,
      hh_size == 4 ~ rec_neigh$cost_4br,
      hh_size == 5 ~ rec_neigh$cost_5br,
      hh_size == 6 ~ rec_neigh$cost_6br,
      hh_size == 7 ~ rec_neigh$cost_7br,
      hh_size == 8 ~ rec_neigh$cost_8br
    )
    
    monthly_payment_value <- monthly_payment() 
    
    # Prepare table data
    details_table <- data.frame(
      Neighborhood = rec_neigh$tract_neigh,
      ZIP_Code = rec_neigh$zip_code,
      Max_Rent = paste0("$", formatC(max_rent, big.mark = ",")),
      HUD_Pays = paste0("$", formatC(max_rent - monthly_payment_value, big.mark = ",")),
      Your_Contribution = paste0("$", formatC(as.numeric(monthly_payment_value), big.mark = ","))
    )
    
    # Create HTML table
    table_html <- paste(
      "<table style='width:50%; border-collapse: collapse;'>",
      "<thead><tr style='background-color: darkcyan; color: white;'>",
      "<th>Neighborhood</th><th>ZIP Code</th><th>Max Rent</th><th>HUD pays</th><th>You Pay</th>",
      "</tr></thead>",
      "<tbody>",
      paste(
        apply(details_table, 1, function(row) {
          paste0("<tr style='border: transparent; padding: 8px;'>",
                 "<td>", row[1], "</td>",
                 "<td>", row[2], "</td>",
                 "<td>", row[3], "</td>",
                 "<td>", row[4], "</td>",
                 "<td>", row[5], "</td>",
                 "</tr>")
        }),
        collapse = ""
      ),
      "</tbody></table>"
    )
    HTML(
      paste0(
        "<div style='max-height: 200px; overflow-y: auto;'>", table_html, "</div>"
      )
    )
  })
  
  
  
}

# Run the app
shinyApp(ui, server)
