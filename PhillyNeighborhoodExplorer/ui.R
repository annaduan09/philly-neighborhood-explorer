# load required packages
library(shiny)
library(shinyWidgets)
library(shinythemes)


# INPUTS
# 1. Geography: choose neighborhoods or skip
# 2. Constraints: income/work status, household size
# 3. Preferences: amenities types, demographics

#

ui <- navbarPage(
  theme = shinytheme("flatly"),
  collapsible = TRUE,
  title = strong("Pennsylvania Housing Explorer"),
  windowTitle = "PA Housing Data Explorer",
  
    tabPanel(
      "Neighborhood mapper",
      leafletOutput("leaflet", height = "100vh"),
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        top = 110,
        left = 55,
        width = 250,
        fixed = TRUE,
        draggable = TRUE,
        height = "auto",
        h3("Philly Mapper"),
        p(
          "Explore Philadelphia neighborhoods."
        ),
        selectizeInput("neighborhoods", "Neighborhoods:", choices = nb$MAPNAME, selected = "Grays Ferry", multiple = TRUE,
                       options = list(placeholder = 'select a state name')),
        p(textOutput("indicator_desc_text")),
        out =
          img(src = 'logos.png', height = 120)
      )
    ),
    
    tabPanel("Region plots", sidebarLayout(
      sidebarPanel(
        pickerInput(
          "outcome_select",
          "Outcome:",
          choices = c(
            "Deaths per million",
            "Cases per million",
            "Cases (total)",
            "Deaths (total)"
          ),
          selected = c("Deaths per million"),
          multiple = FALSE
        ),
        
      ), mainPanel(tabsetPanel())
    ))
)
