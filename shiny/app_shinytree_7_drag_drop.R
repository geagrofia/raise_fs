# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)


# inst/examples/07-drag-and-drop/ui.R

# Define UI
ui <- pageWithSidebar(
  headerPanel("Drag-and-Drop shinyTree"),
  
  sidebarPanel(      
    shinyTree("tree", dragAndDrop=TRUE, sort = F, wholerow = T, unique = T),
    HTML("<hr />"),
    helpText(p("Drag some of the nodes in the tree around to see the structure on the right update."),
             HTML("<hr />Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
  ),
  mainPanel(
    verbatimTextOutput("str")  
  )
)

# inst/examples/07-drag-and-drop/server.R

# Define server logic
server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root3 = "234",
      root1 = list(
        SubListA = list(leaf1 = "", leaf2 = "")
      ),
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "")
      )
    )
  })
  
  output$str <- renderPrint({
    # shinyTrees will also be available as inputs so you can
    # monitor changes that occur to the tree as the user interacts
    # with it.
    str(input$tree)
  })
}

# Run the application
shinyApp(ui, server)