# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)



# attr_list <-     list(
#   root1 = structure("", stselected=TRUE, sticon="signal"),
#   root2 = structure(list(
#     SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
#     SubListB = structure(list(leafA = "", leafB = ""), stdisabled=TRUE)
#   ),
#   stopened=T
#   )
# )


###
df <- read.csv("E:/repos/raise_fs/tab_data/input/requirements_pearl_millet_lu_link.csv")


# FROM CHATPGPT

# Function to build a nested list using criterion names
build_nested_list <- function(df, parent) {
  # Find the unique children of the current parent
  children <- unique(df[df$stack_code == parent, c("crit_code", "criterion")])
  
  # If no children, return an empty string and add the attribute
  if (nrow(children) == 0) {
    # Create a named list with an attribute for leaf nodes
    leaf_node <- list()  # You can initialize with a specific value if needed
    attr(leaf_node, "sticon") <- "map"  # Set your desired attribute
    return(leaf_node)
  }
  
  # For each child, recursively call the function
  result <- setNames(lapply(children$crit_code, function(child) build_nested_list(df, child)), children$criterion)
  
  # Add attributes to the higher-level node
  attr(result, "node_type") <- "parent"  # Add a 'node_type' attribute
  attr(result, "sticon") <- "layer-group"  # Example attribute
  
  return(result)
}

# Identify the top-level parent(s)
top_level_parents <- setdiff(df$stack_code, df$crit_code)

# Initialize the nested list
nested_list <- list()

# Build the nested list for each top-level parent
for (parent in top_level_parents) {
  # Get the criterion for the top-level parent
  parent_criterion <- unique(df[df$stack_code == parent, "criterion"])
  
  # Ensure the criterion is a single value
  if (length(parent_criterion) == 1) {
    # Build the structure for this parent and assign it to the named list
    nested_list[[parent_criterion]] <- build_nested_list(df, parent)
  } else {
    warning(paste("Multiple criteria found for parent:", parent))
  }
}

# Print the result
print(nested_list)

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
check_leaf_nodes(nested_list)
















# inst/examples/02-attributes/ui.R

# Define UI
ui <- 
  
  pageWithSidebar(
  
    # Application title
    headerPanel("IRM Hierarchy"),
    
    sidebarPanel(
      helpText(HTML("An example with pre-defined attributes to dictate the behavior of specific nodes.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      
      # icon colours from https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
        
      tags$style(".fa-map {color:#1b9e77}"),
      tags$style(".fa-layer-group {color:#d95f02}"),
      
      # Show a simple table.
      shinyTree("tree", checkbox = F, theme = "proton", stripes = F, themeIcons = TRUE, themeDots = TRUE, dragAndDrop = TRUE, contextmenu = T)
      
    )
  )

# inst/examples/02-attributes/server.R

# Define server logic
server <- function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  output$tree <- renderTree({
    nested_list
  })

}

# Run the application
shinyApp(ui, server)
