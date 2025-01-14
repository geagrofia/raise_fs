library(shiny)
library(shinyTree)

# Sample initial tree structure (already in the format that shinyTree understands)
initial_tree <- list(
  "Root" = list(
    "Child 1" = NULL,
    "Child 2" = list(
      "Grandchild 1" = NULL,
      "Grandchild 2" = NULL
    )
  )
)

# UI
ui <- pageWithSidebar(
  # Application title
  headerPanel("IRM Hierarchy"),
  sidebarPanel(
    helpText("An example of saving changes"),
    actionButton("save_button", "Save Changes"),  # Button to save changes
    actionButton("reset_button", "Reset to Original") # Button to reset to original         
  ),
  mainPanel(
    # Show a simple table.
  shinyTree("tree", checkbox = F, theme = "proton", stripes = F, themeIcons = TRUE, themeDots = TRUE, dragAndDrop = TRUE, contextmenu = T),  # Display the tree
  
  verbatimTextOutput("tree_output")  # Output the current tree for debugging
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to hold the current tree structure
  current_tree <- reactiveVal(initial_tree)
  
  # Render the tree
  output$tree <- renderTree({
    current_tree()
  })
  
  # the following observEvent conflicts with how shinyTree handles its events so it has been removed
  # 08/01/2025
  
  # # Observe changes in the tree
  # observeEvent(input$tree, {
  #   new_tree <- input$tree  # Get the modified tree structure
  #   current_tree(new_tree)  # Update the current tree
  # })
  # 
  
  # When the save button is clicked
  observeEvent(input$save_button, {
    saved_tree <- input$tree  # Retrieve the current state from the input
    if (!is.null(saved_tree)) {
      current_tree(saved_tree)  # Update the reactive value
      output$tree_output <- renderPrint(saved_tree)  # Output for debugging
    
      # Optionally: Here you could save `saved_tree` to a file or database
      # For example, to save as a JSON file:
      jsonlite::write_json(saved_tree, "data/saved_tree.json")
      write.csv(treeToDf(saved_tree), "data/saved_tree.csv" )
    }
  })
  
  # When the reset button is clicked
  observeEvent(input$reset_button, {
    current_tree(initial_tree)  # Reset to the original tree structure
  })
}

# Run the app
shinyApp(ui = ui, server = server)
