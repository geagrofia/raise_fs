library(shiny)
library(shinyTree)

ui <- fluidPage(
  titlePanel("Editable ShinyTree with Save Functionality"),
  sidebarLayout(
    sidebarPanel(
      actionButton("save_tree", "Save Tree")
    ),
    mainPanel(
      h4("Editable Tree:"),
      shinyTree("editable_tree", theme = "proton", contextmenu = TRUE), # Tree with search functionality
      tags$hr(),
      h4("Previous Tree:"),
      verbatimTextOutput("previous_tree_display"),  # Output for the previous tree
      tags$hr(),
      h4("Saved Tree:"),
      verbatimTextOutput("saved_tree_display")  # Output for the current saved tree
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for tree states
  rv <- reactiveValues(
    editable_tree = list(
      Root = list(
        Node1 = list(SubNode1 = "", SubNode2 = ""),
        Node2 = list()
      ),
      saved_tree = NULL,
      previous_tree = NULL  # New reactive value to store the previous tree
    ))
    
    # Initialize original tree within a reactive context
    observe({
      rv$original_tree <- rv$editable_tree
    })
    
    # Render the editable tree
    output$editable_tree <- renderTree({
      rv$editable_tree
    })
    
    # Save button handler
    observeEvent(input$save_tree, {
      req(input$editable_tree)  # Ensure the tree exists before saving
      
      # Update previous tree to be the current saved tree (if it exists)
      if (!is.null(rv$saved_tree)) {
        rv$previous_tree <- rv$saved_tree
      } else {
        # If this is the first save, store the original tree as the previous tree
        rv$previous_tree <- rv$editable_tree
      }
      
      # Save the current state of the tree
      rv$saved_tree <- input$editable_tree
    })
    
    # Display the previous tree
    output$previous_tree_display <- renderPrint({
      if (is.null(rv$previous_tree)) {
        "No previous tree version saved yet."
      } else {
        str(as.list(rv$previous_tree), max.level = 5)  # Print the previous tree structure
      }
    })
    
    # Display the saved tree
    output$saved_tree_display <- renderPrint({
      if (is.null(rv$saved_tree)) {
        "No tree has been saved yet."
      } else {
        str(as.list(rv$saved_tree), max.level = 5)  # Print the saved tree structure
      }
    })
}

shinyApp(ui, server)
