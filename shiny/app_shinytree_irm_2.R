# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)
library(data.tree)


### GET THE IRM HIERARCHY
df <- read.csv("E:/repos/raise_fs/tab_data/input/requirements_pearl_millet_lu_link.csv", na.strings = c("NA",""))

# FROM IRM AUTOMATE
df <- df[complete.cases(df$rulebase_number, df$criterion), ]


df_short <-
  df %>% dplyr::select(stack, criterion, weight)


df_short$stack[is.na(df_short$stack)] <-
  "root2"

# Convert to tree
tree_plot <- FromDataFrameNetwork(df_short, c("weight"))

# Add icon attributes to nodes
set_icons <- function(node) {
  if (node$isLeaf) {
    node$icon <- "map"  # data criteria
  } else {
    node$icon <- "layer-group"  # non-data criteria
  }
  for (child in node$children) {
    set_icons(child)
  }
}
set_icons(tree_plot)



# Convert to json for rendering
testjson <- treeToJSON(tree_plot)

# Print the result
print(testjson)

# Check attributes of the leaf nodes
check_leaf_nodes <- function(node) {
  if (is.list(node)) {
    for (name in names(node)) {
      check_leaf_nodes(node[[name]])  # Recursively check leaf nodes
    }
  } else {
    print(attr(node, "attribute_name"))  # Print attribute of the leaf node
  }
}

# Call function to check leaf node attributes
check_leaf_nodes(testjson)


# inst/examples/02-attributes/ui.R

# Define UI
ui <- 
  
  pageWithSidebar(
  
    # Application title
    headerPanel("IRM Hierarchy"),
    
    sidebarPanel(
      helpText(HTML("An example with pre-defined attributes to dictate the behavior of specific nodes.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>.")),
      actionButton("save_button", "Save Changes"),  # Button to save changes
      actionButton("reset_button", "Reset to Original") # Button to reset to original      
    ),
    
    mainPanel(
      
      # icon colours from https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
        
      tags$style(".fa-map {color:#1b9e77}"),
      tags$style(".fa-layer-group {color:#d95f02}"),
      
      # Show a simple table.
      shinyTree("tree", checkbox = F, theme = "proton", stripes = F, themeIcons = TRUE, themeDots = TRUE, dragAndDrop = TRUE, contextmenu = T),
      verbatimTextOutput("tree_output")  # Output the current tree for debugging
    )
  )

# inst/examples/02-attributes/server.R

# Define server logic
server <- function(input, output, session) {
  
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  # Reactive value to hold the current tree structure
  current_tree <- reactiveVal(testjson)
  
  output$tree <- renderTree({
    current_tree()
  })
  
  # When the save button is clicked
  observeEvent(input$save_button, {
    saved_tree <- input$tree  # Retrieve the current state from the input
    if (!is.null(saved_tree)) {
      current_tree(saved_tree)  # Update the reactive value
      output$tree_output <- renderPrint(saved_tree)  # Output for debugging
      
      # Optionally: Here you could save `saved_tree` to a file or database
      # For example, to save as a JSON file:
      jsonlite::write_json(saved_tree, "data/irm_1_tree.json")
      write.csv(treeToDf(saved_tree), "data/irm_1_tree.csv" )
    }
  })
  
  # When the reset button is clicked
  observeEvent(input$reset_button, {
    current_tree(testjson)  # Reset to the original tree structure
  })
  

}

# Run the application
shinyApp(ui, server)
