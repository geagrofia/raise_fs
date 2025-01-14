# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)


# inst/examples/02-attributes/ui.R

# Define UI
ui <- pageWithSidebar(
    # Application title
    headerPanel("shinyTree with 'selected' input"),
    
    sidebarPanel(
      helpText(HTML("An example of using shinyTree's <code>get_selected</code> function to extract the cells which are currently selected.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      "Currently Selected:",
      shinyTree("tree123"),
      hr(),
      "Currently Selected:",
      verbatimTextOutput("sel_names"),
      verbatimTextOutput("sel_slices"),
      verbatimTextOutput("sel_classid"),
      
      verbatimTextOutput("str")
    )
  )

# inst/examples/04-selected/server.R

# Define server logic
server <- function(input, output, session) {
  
  output$tree123 <- renderTree({
    list(
      root1 = structure("123"),
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
  
  output$sel_names <- renderPrint({
    tree <- input$tree123
    req(tree)
    get_selected(tree)
  })
  output$sel_slices <- renderPrint({
    tree <- input$tree123
    req(tree)
    get_selected(tree, format = "slices")
  })
  output$sel_classid <- renderPrint({
    tree <- input$tree123
    req(tree)
    get_selected(tree, format = "classid")
  })
  
  output$str <- renderPrint({
    # shinyTrees will also be available as inputs so you can
    # monitor changes that occur to the tree as the user interacts
    # with it.
    str(input$tree123)
  })

}

# Run the application
shinyApp(ui, server)