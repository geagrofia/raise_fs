# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)

# inst/examples/11-tree-update/ui.R

# Define UI
ui <- 
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("idTree",
                    label = "Select a tree",
                    choices = list("",
                                   "A" = "A",
                                   "A_closed" = "A_closed",
                                   "B" = "B", 
                                   "List" ="List"),
                    selected = NULL)
      ),
      mainPanel(
        h4("Tree Update"),
        "The tree is initialized, but not populated until a choices is selected in the dropdown menu.",
        shinyTree("tree"),
        hr(),
        "Currently selected:",
        verbatimTextOutput("idSelected")#,
      )
    )
  )


# inst/examples/11-tree-update/server.R

jsonTree<- function(idTree){
  switch(idTree,
         "A" = '[{"id":"node_2","text":"Root node with options","state":{"opened":true,"selected":true},"children":[{"text":"Child 1"},"Child 2"]}]',
         "A_closed" = '[{"id":"node_2","text":"Root node with options","state":{"opened":false,"selected":false},"children":[{"text":"Child 1"},"Child 2"]}]',
         "B" = '[{"id":"ajson1","parent":"#","text":"Simplerootnode","li_attr":{"class":"project","stid":"project-1"}},{"id":"ajson2","parent":"#","text":"Rootnode2"},{"id":"ajson3","parent":"ajson2","text":"Child1"},{"id":"ajson4","parent":"ajson2","text":"Child2"}]',
         "List" = list(a=list(a1=1,a2=2) , b="b") )
}

# Define server logic
server <- function(input, output, session) {
  output$idSelected <- renderPrint({
    tree <- input$tree
    if (is.null(tree)){
      "None"
    } else{
      str(get_selected(input$tree, format = "classid"))
    }
  })
  
  # Tree is initialized without nodes
  output$tree <- renderEmptyTree()
  
  # An observer is used to trigger a tree update with new data.
  observe({
    updated.tree<-jsonTree(input$idTree)
    updateTree(session,"tree",updated.tree)
  })
}

# Run the application
shinyApp(ui, server)