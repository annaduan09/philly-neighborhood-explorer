# load required packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(sf)
library(tidyverse)

# Define neighborhoods grouped by region
north_neighborhoods <- c("Upper Kensington", "Richmond", "Frankford", "Juniata Park", "Northwood", "Harrowgate", "Hunting Park", "Nicetown", "Tioga", "Feltonville", "Logan")
northwest_neighborhoods <- c("West Oak Lane", "East Oak Lane", "Chestnut Hill", "Germantown", "Roxborough", "Manayunk", "Mount Airy")
northeast_neighborhoods <- c("Mayfair", "Tacony", "Holmesburg", "Fox Chase", "Bustleton", "Somerton", "Oxford Circle", "Rhawnhurst")
west_neighborhoods <- c("University City", "Wynnefield", "Overbrook", "Cobbs Creek", "Walnut Hill", "Spruce Hill")
center_city_neighborhoods <- c("Rittenhouse", "Logan Square", "Chinatown", "Society Hill", "Washington Square West")
south_neighborhoods <- c("South Philadelphia", "Point Breeze", "Girard Estates", "Passyunk Square", "Whitman")
southwest_neighborhoods <- c("Kingsessing", "Elmwood", "Eastwick")


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
      h3("Neighborhood Filters"),
      br(),
      p(
        "Select neighborhoods to explore. The more neighborhoods you select,
          the more likely you are to find matches for your constraints and preferences."
      ),
      selectizeInput(
        "neighborhoods",
        "Neighborhoods I want to consider:",
        choices = list(
          "North" = north_neighborhoods,
          "Northwest" = northwest_neighborhoods,
          "Northeast" = northeast_neighborhoods,
          "West" = west_neighborhoods,
          "Center City" = center_city_neighborhoods,
          "South" = south_neighborhoods,
          "Southwest" = southwest_neighborhoods),
        selected = c("West" = "Wynnefield"),
        multiple = TRUE,
        options = NULL
      ),
      br(),
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
            "Families" = "kids",
            "Longtime residents" = "same_house_pct2022",
            "Majority Black" = "black_pct2022",
            "Majority Hispanic" = "hispanic_pct2022",
            "Voucher-friendly" = "vouchers",
            "Population" = "population2022"
          )),
          selected = c("Community" = "kids", "Amenities" = "restaurant"),
          multiple = TRUE,
          options = NULL
      )
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
    )),
    
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