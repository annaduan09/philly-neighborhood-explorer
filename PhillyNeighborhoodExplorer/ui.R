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
  theme = shinytheme("cosmo"),
  collapsible = TRUE,
  title = strong("Philly Neighborhood Explorer"),
  windowTitle = "Find your new neighborhood",
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
        selectizeInput("neighborhoods", "Neighborhoods:", choices = nb$neighborhood, 
                       selected = c("Wynnefield", "East Mount Airy", "Roxborough", 
                                    "Manayunk", "East Germantown", "Wissahickon", 
                                    "Society Hill"), multiple = TRUE,
                       options = NULL))),
    
    tabPanel("About this tool", sidebarLayout(
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
