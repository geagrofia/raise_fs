# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)


# inst/examples/02-attributes/ui.R

# Define UI
ui <- pageWithSidebar(
  
    # Application title
    headerPanel("shinyTree with attributes!"),
    
    sidebarPanel(
      helpText(HTML("A shinyTree example with pre-defined attributes to dictate the behavior of specific nodes.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree")
    )
  )

# inst/examples/02-attributes/server.R

# Define server logic
server <- function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  output$tree <- renderTree({
    list(
      root1 = structure("", stselected=TRUE, sticon="signal"),
      root2 = structure(list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = structure(list(leafA = "", leafB = ""), stdisabled=TRUE)
      ),
      stopened=TRUE
      )
    )
  })
}

# Run the application
shinyApp(ui, server)
