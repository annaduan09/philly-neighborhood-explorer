

library(shiny)

# UI
ui <- fluidPage(
  conditionalPanel(
    condition = "input.next_btn > 0",
    checkboxGroupInput("preferences", "What's important to you in a neighborhood?",
                       choices = list(
                         "Safety" = "safety",
                         "Good Schools" = "schools",
                         "Public Transportation" = "transport",
                         "Parks and Recreation" = "parks",
                         "Shopping and Dining" = "shopping"
                       )),
    actionButton("results_btn", "See Results")
  )
)

server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
