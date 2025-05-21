library(shiny)

ui <- fluidPage(
  titlePanel("Select Number of Innovations"),
  
  sidebarLayout(
    sidebarPanel(
      # First set of radio buttons
      radioButtons("choice1", "Number of innovations to be modelled:",
                   choices = list("1" = "one_inn", "2" = "two_inn")),
      
      # Second radio button (conditionally shown)
      uiOutput("second_radio_ui"),
      
      # Conditional "Next" button
      uiOutput("next_button_ui")
    ),
    
    mainPanel(
      h4("Your Selection Summary:"),
      textOutput("summaryText")
    )
  )
)

server <- function(input, output, session) {
  # Dynamically show second radio buttons if Option 2 is selected
  output$second_radio_ui <- renderUI({
    if (input$choice1 == "two_inn") {
      radioButtons("choice2", "System:",
                   choices = list("Intercrop / Rotation" = "intercrop", "Compare" = "compare"))
    }
  })
  
  # Conditionally show the Next button
  output$next_button_ui <- renderUI({
    if (input$choice1 == "one_inn" || (input$choice1 == "two_inn" && !is.null(input$choice2))) {
      actionButton("next_btn", "Next")
    }
  })
  
  # Text summary in the main panel
  output$summaryText <- renderText({
    if (input$choice1 == "one_inn") {
      "You selected 1 innovation."
    } else if (input$choice1 == "two_inn" && !is.null(input$choice2)) {
      paste("You selected 2 innovations and", 
            ifelse(input$choice2 == "intercrop", "Intercrop / Rotation", "Compare"),"system")
    } else {
      "Please make a selection."
    }
  })
  
  # Optional: Print input values when "Next" is clicked
  observeEvent(input$next_btn, {
    print(paste("Number of Innovations:", input$choice1))
    if (input$choice1 == "two_inn") {
      print(paste("System:", input$choice2))
    }
  })
}

shinyApp(ui, server)
