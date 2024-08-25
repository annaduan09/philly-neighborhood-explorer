

# load required packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)


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
        selectInput(
          "variable",
          strong("Select an indicator"),
          choices = list(
            "Homeowners" = list(
              "Homeownership rate (2022)" = "owner_occ_hh_pct2022",
              "Median home value (2022)" = "med_home_value2022",
              "Mortgage burdened households (2022)" = "mortgage_burdened_pct2022"
            ),
            "Renter households" = list(
              "Rentership rate (2022)" = "renter_occ_hh_pct2022",
              "Rent burdened households (2022)" = "rent_burdened_pct2022",
              "Median gross rent (2022)" = "med_gross_rent2022"
            ),
            "Housing stock" = list(
              "Vacant rental units (2022)" = "renter_vacant_pct2022",
              "Median age of home (2022)" = "med_age_home2022",
              # "Affordable rent units available" = "afford_avail_units",
              "Affordable housing shortage (2020)" = "housing_balance"
            ),
            "Other topics" = list("Households with internet access (2022)" = "internet_hh_pct2022")
          ),
          selected = "owner_occ_hh_pct2022"
        ),
        strong("About this indicator"),
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
