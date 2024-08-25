# load required packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(sf)
library(tidyverse)

neighborhoods <- c("West Oak Lane", "Northwood", "Southwest Germantown", "Mayfair", "Chinatown", 
                   "Logan Square", "Chestnut Hill", "West Mount Airy", "East Germantown", "Richmond", 
                   "Harrowgate", "Frankford", "Tacony", "Society Hill", "Packer Park", "Stadium District", 
                   "Andorra", "Chestnut Hill", "Chestnut Hill", "East Mount Airy", "East Mount Airy", 
                   "Chestnut Hill", "Cedarbrook", "Upper Kensington", "Richmond", "Richmond", 
                   "Upper Kensington", "Bustleton", "Airport", "Spring Garden", "Fairmount", 
                   "University City", "North Central", "Bridesburg", "Bridesburg", "Juniata Park", 
                   "Juniata Park", "Upper Kensington", "Hunting Park", "Hunting Park", "Franklinville", 
                   "Franklinville", "Tioga", "Hunting Park", "Nicetown", "Tioga", "East Falls", 
                   "East Falls", "Wissahickon", "Roxborough", "Roxborough", "Roxborough", "Manayunk", 
                   "Germany Hill", "West Kensington", "West Kensington", "Fishtown - Lower Kensington", 
                   "East Kensington", "West Kensington", "West Kensington", "Hartranft", "Hartranft", 
                   "Fishtown - Lower Kensington", "Crescentville", "Harrowgate", "Lower Moyamensing", 
                   "Riverfront", "Navy Yard", "Old City", "University City", "Rittenhouse", "East Park", 
                   "East Falls", "Brewerytown", "Logan Square", "Northern Liberties", "Logan Square", 
                   "Rittenhouse", "Rittenhouse", "Hunting Park", "Strawberry Mansion", "Allegheny West", 
                   "Allegheny West", "Allegheny West", "Olney", "Feltonville", "Lawndale", "Stanton", 
                   "University City", "Dearnley Park", "Upper Roxborough", "Upper Roxborough", 
                   "Upper Roxborough", "Chestnut Hill", "West Mount Airy", "West Mount Airy", 
                   "West Central Germantown", "Germantown - Westside", "Germantown - Penn Knox", 
                   "Brewerytown", "North Central", "North Central", "Southwest Germantown", 
                   "Southwest Germantown", "Wister", "Glenwood", "Hartranft", "Hartranft", "McGuire", 
                   "East Germantown", "East Germantown", "East Germantown", "Germantown - Morton", 
                   "East Mount Airy", "Graduate Hospital", "Yorktown", "Bartram Village", "Industrial", 
                   "Northern Liberties", "Richmond", "Callowhill", "Ludlow", "Oxford Circle", 
                   "Upper Roxborough", "University City", "Rhawnhurst", "Whitman", "Strawberry Mansion", 
                   "Southwest Schuylkill", "Lawndale", "Wissahickon Park", "Roxborough", "Frankford", 
                   "Spring Garden", "Cedarbrook", "West Oak Lane", "West Oak Lane", "West Oak Lane", 
                   "West Oak Lane", "East Oak Lane", "East Oak Lane", "Melrose Park Gardens", "Olney", 
                   "Modena", "Modena", "Ogontz", "Ogontz", "Logan", "Fern Rock", "Logan", "Olney", 
                   "Olney", "Feltonville", "Olney", "Frankford", "Frankford", "Millbrook", "Overbrook", 
                   "Wynnefield Heights", "Kingsessing", "Dickinson Narrows", "Wissinoming", "Frankford", 
                   "Northwood", "Northwood", "Lawndale", "Franklin Mills", "Parkwood Manor", 
                   "Parkwood Manor", "Byberry", "Burholme", "Oxford Circle", "Oxford Circle", 
                   "Oxford Circle", "Oxford Circle", "Mayfair", "Mayfair", "Mayfair", "Tacony", 
                   "Wissinoming", "Tacony", "Tacony", "Holmesburg", "Holmesburg", "Holmesburg", 
                   "Lexington Park", "Rhawnhurst", "Fox Chase", "Fox Chase", "Fox Chase", "Fox Chase", 
                   "Bustleton", "Pennypack", "Pennypack", "Walnut Hill", "Overbrook", "Overbrook", 
                   "Richmond", "East Parkside", "West Parkside", "Overbrook", "Overbrook", "Overbrook", 
                   "Wynnefield", "Wynnefield", "Overbrook", "Wynnefield", "East Poplar", "Francisville", 
                   "Point Breeze", "Oxford Circle", "Spring Garden", "Richmond", "Brewerytown", 
                   "Sharswood", "Yorktown", "Somerton", "Somerton", "Rhawnhurst", "Rhawnhurst", "Tioga", 
                   "Oxford Circle", "Oxford Circle", "Upper Kensington", "Upper Kensington", "Mayfair", 
                   "Holmesburg", "Mayfair", "Washington Square West", "Rittenhouse", "Allegheny West", 
                   "Overbrook", "Walnut Hill", "Hartranft", "Belmont", "Mantua", "Haverford North", 
                   "Eastwick", "Clearview", "Eastwick", "Center City East", "Academy Gardens", "Penrose", 
                   "Paschall", "Paschall", "East Oak Lane", "Old Kensington", "Riverfront", 
                   "Wynnefield Heights", "Wynnefield Heights", "Fairmount", "Bustleton", 
                   "Washington Square West", "Fitler Square", "Pennsport", "Logan", "Morrell Park", 
                   "Point Breeze", "Fishtown - Lower Kensington", "Washington Square West", "Olney", 
                   "Somerton", "Logan Square", "Bustleton", "Washington Square West", "Upper Kensington", 
                   "West Central Germantown", "Mantua", "East Germantown", "Rhawnhurst", 
                   "Woodland Terrace", "North Central", "Newbold", "East Passyunk", "Dickinson Narrows", 
                   "Stadium District", "Pennypack", "Pennypack Woods", "Aston-Woodbridge", "Torresdale", 
                   "Torresdale", "Torresdale", "Northeast Phila Airport", "Bustleton", "Somerton", 
                   "Somerton", "Somerton", "Normandy Village", "Cedarbrook", "Olney", "Paschall", 
                   "Elmwood", "Kingsessing", "Queen Village", "Queen Village", "Kingsessing", 
                   "Southwest Schuylkill", "Hawthorne", "Graduate Hospital", "Point Breeze", 
                   "Point Breeze", "Bella Vista", "Queen Village", "Passyunk Square", "Point Breeze", 
                   "Point Breeze", "Grays Ferry", "Cedar Park", "Cedar Park", "Cobbs Creek", 
                   "Cobbs Creek", "Grays Ferry", "West Passyunk", "West Passyunk", "Girard Estates", 
                   "Newbold", "Girard Estates", "Lower Moyamensing", "North Central", "East Mount Airy", 
                   "Morrell Park", "Wissinoming", "Logan", "Cobbs Creek", "Cobbs Creek", "Cobbs Creek", 
                   "Powelton", "Lower Moyamensing", "Lower Moyamensing", "Carroll Park", "Passyunk Square", 
                   "Elmwood", "Wissinoming", "Washington Square West", "Hawthorne", "West Powelton", 
                   "Dunlap", "Haddington", "Haddington", "Haddington", "Overbrook", "Carroll Park", 
                   "Haddington", "Haddington", "Mill Creek", "Whitman", "Feltonville", "Overbrook", 
                   "Strawberry Mansion", "Graduate Hospital", "West Poplar", "Bustleton", 
                   "Strawberry Mansion", "Strawberry Mansion", "Oxford Circle", "Stanton", "Ogontz", 
                   "Logan", "Spruce Hill", "Cobbs Creek", "Kingsessing", "Spruce Hill", "Society Hill", 
                   "Lawndale", "Mayfair", "Somerton", "Rittenhouse", "Rhawnhurst", "Cedarbrook", 
                   "Hunting Park", "Hartranft", "Fishtown - Lower Kensington", "Crescentville", 
                   "Lower Moyamensing", "University City", "East Falls", "Harrowgate", "Northern Liberties", 
                   "Old City", "West Park", "Industrial", "Logan Square", "Logan Square", "Rittenhouse", 
                   "Tioga", "West Mount Airy", "Germantown - Morton", "Rittenhouse", "Brewerytown", 
                   "University City", "Graduate Hospital", "Rittenhouse", "Riverfront", "Hunting Park", 
                   "Grays Ferry", "Industrial", "Grays Ferry", "Industrial", "West Oak Lane", "Olney", 
                   "Juniata Park", "Elmwood", "Cobbs Creek", "Mill Creek", "East Oak Lane", 
                   "Cobbs Creek")


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
        choices = neighborhoods,
        selected = c(
          "Wynnefield",
          "East Mount Airy",
          "Roxborough",
          "Manayunk",
          "East Germantown",
          "Wissahickon",
          "Society Hill"
        ),
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