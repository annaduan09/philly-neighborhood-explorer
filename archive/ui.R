# load required packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(sf)
library(tidyverse)
library(shinyalert)

# INPUTS
# 1. Geography: choose neighborhoods or skip
# 2. Constraints: income/work status, household size
# 3. Preferences: amenities types, demographics


ui <- navbarPage(
  theme = shinytheme("cosmo"),
  collapsible = TRUE,
  title = strong("Philly Neighborhood Explorer"),
  windowTitle = "Find your new neighborhood",
  tabPanel(
    tags$head(includeCSS("styles.css")),
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
      h3("My Search"),
      br(),
      strong("Neighborhoods:"),
      p(textOutput("neighborhoods")),
      actionLink("update_neighs", "Update"),
      br(),
      strong("Amenities:"),
      p(textOutput("amenities")),
      actionLink("update_prefs", "Update")
    ),
    
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      top = 650,
      left = 55,
      width = 250,
      fixed = TRUE,
      draggable = TRUE,
      height = "auto",
      h3("Cost Calculator"),
      # User enters income
      sliderInput(
        "income",
        "My annual household income:",
        value = 10000,
        min = 0,
        max = 39150
      ),
      # User selects household size
      sliderInput(
        "household_size",
        "People living with me:",
        value = 1,
        min = 1,
        max = 8
      ),
      
      br(),
      # Print expected contribution based on output$monthly_payment
      strong("Estimated monthly cost:", textOutput("monthly_payment"))
    ))
)