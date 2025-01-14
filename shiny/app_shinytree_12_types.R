# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)

#' Demonstrates types
#' @author Michael Bell \email{bellma@@lilly.com}
#' 
# inst/examples/12-types/ui.R

# Define UI
ui <- pageWithSidebar(
    # Application title
    headerPanel("shinyTree with types!"),
    
    sidebarPanel(
      helpText(HTML("A shinyTree example using types to dictate the behavior of specific nodes.
                  <hr>Created using shinyTree<hr>Update button exists to make sure the output of the update tree is the same as renderTree</a>.")),
      actionButton(inputId = "renderTree", "Render"),
      actionButton(inputId = "updateTree", "Update")
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", dragAndDrop = TRUE,types= #Types is in the same format that jstree expects
                  "{
          '#': { 'max_children' : 2, 'max_depth' : 4, 'valid_children' : ['root'] },
          'root' : { 'icon' : 'fa fa-signal', 'valid_children' : ['file'] },
          'default' : { 'valid_children' : ['default','file'] },
          'file' : { 'icon' : 'glyphicon glyphicon-file', 'valid_children' : [] }
        }"
      )
    )
  )


# Define server logic
server <- function(input, output, session) {
  
  
log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))

treeData <- reactive({
  list(
    root1 = structure("", stselected=TRUE,sttype="root"),
    root2 = structure(list(
      SubListA = structure(list(
        leaf1 = structure("",sttype="file"), 
        leaf2 = structure("",sttype="file"),
        leaf3 = structure("",sttype="file")),
        sttype="root",stopened=TRUE
      ),
      SubListB = structure(list(
        leafA = structure("",sttype="file"),
        leafB = structure("",sttype="file")
      ),stopened=TRUE,sttype="root")
    ),
    sttype="root",stopened=TRUE
    )
  )
})

observeEvent(input$updateTree,{
  updateTree(session, treeId = "tree", data = treeData())
})

output$tree <- renderTree({
  treeData()
})
}

# Run the application
shinyApp(ui, server)