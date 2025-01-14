library(shiny)
library(shinyTree)
library(dplyr)

# Helper function to flatten the tree
flatten_tree <- function(tree, parent = NULL) {
  result <- data.frame(Name = character(), Parent = character(), stringsAsFactors = FALSE)
  
  if (is.null(tree) || !is.list(tree)) {
    stop("Tree structure is invalid. It must be a named list.")
  }
  
  for (node in names(tree)) {
    if (is.null(node) || node == "") {
      warning("Skipping invalid or empty node name.")
      next
    }
    
    # Add the current node, converting NULL parent to NA
    result <- rbind(result, data.frame(Name = node, Parent = ifelse(is.null(parent), NA, parent), stringsAsFactors = FALSE))
    
    # Process children if they exist
    children <- tree[[node]]
    if (!is.null(children) && is.list(children) && length(children) > 0) {
      child_result <- flatten_tree(children, parent = node)
      result <- rbind(result, child_result)
    }
  }
  
  return(result)
}


# Define UI
ui <- fluidPage(
  titlePanel("Shiny Tree Example"),
  sidebarLayout(
    sidebarPanel(
      actionButton("save_tree", "Save Changes"),
      actionButton("reload_tree", "Reload Original Tree"),
      br(),
      br(),
      textOutput("save_status")
    ),
    mainPanel(
      tags$hr(),
      h4("Previous Tree:"),
      shinyTree::shinyTree("original_tree", theme = "proton", contextmenu = TRUE, dragAndDrop = TRUE),
      verbatimTextOutput("previous_tree_display"),  # Output for the previous tree
      tags$hr(),
      h4("Saved Tree:"),
      shinyTree::shinyTree("tree", theme = "proton", contextmenu = TRUE, dragAndDrop = TRUE),
      verbatimTextOutput("saved_tree_display")  # Output for the current saved tree
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to hold the tree structure
  tree_data <- reactiveVal()
  original_data <- reactiveVal()
  
  # Load initial data from CSV
  observe({
    csv_data <- read.csv("data/tree_structure.csv")
    tree_list <- split(csv_data$Name, csv_data$Parent) # Convert to hierarchical list
    tree_data(tree_list)
    original_data(tree_list) # Save a copy of the original data
  })
  
  # Render the tree
  output$tree <- renderTree({
    tree_data()
  })
  
  # # Render the tree
  # output$original_tree <- renderTree({
  #   original_data()
  # })
  
  # Save the modified tree to a data frame
  observeEvent(input$save_tree, {
    req(input$tree)
    print(input$tree)
    tree <- input$tree
    flattened_tree <- flatten_tree(tree) # Custom function to flatten the tree
    write.csv(flattened_tree, "data/modified_tree.csv", row.names = FALSE)
    output$save_status <- renderText("Tree changes saved successfully.")
  })
  
  # Reload the original tree structure
  observeEvent(input$reload_tree, {
    tree_data(original_data())
    output$save_status <- renderText("Original tree reloaded.")
  })
  
  
  # Display the original tree
  output$previous_tree_display <- renderPrint({
    str(original_data)
  })
  
  
  # Display the saved tree
  output$saved_tree_display <- renderPrint({
    str(input$tree)
    str(tree_data)
  })
  
}








# Run the application 
shinyApp(ui = ui, server = server)
