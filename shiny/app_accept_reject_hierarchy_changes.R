library(shiny)
library(shinyTree)
library(shinyjs)


depth <- function(this,thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))    
  }
}


ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Editable ShinyTree with Save Functionality"),
  sidebarLayout(
    sidebarPanel(
      actionButton("save_tree", "Save Tree", icon = icon("save"), class = "btn btn-primary")  # Save button with bootstrap class
    ),
    mainPanel(
      h4("Editable Tree:"),
      shinyTree("editable_tree", theme = "proton", contextmenu = TRUE, dragAndDrop = TRUE), # Tree with search functionality
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
        Node2 = list(SubNode3 = "")
      )
    ),
    saved_tree = NULL,
    previous_tree = NULL,
    changes_made = FALSE  # New reactive value to track changes
  )
  
  # Initialize the saved tree and previous tree when app starts
  observe({
    rv$saved_tree <- NULL
    rv$previous_tree <- NULL
    shinyjs::disable("save_tree")  # Ensure the button is disabled at startup
  })
  
  # Render the editable tree
  output$editable_tree <- renderTree({
    rv$editable_tree
  })
  
  # Observe for changes in the tree structure specifically via context menu actions
  observeEvent(input$editable_tree, {
    # Check if input has been changed through context menu actions
    if (!is.null(input$editable_tree)) {
      # We need to compare the current tree state with the previous state
      if (!identical(input$editable_tree, rv$editable_tree)) {
        # Only enable the save button if the input has major changes
        rv$changes_made <- TRUE  # Mark that changes have been made
        shinyjs::enable("save_tree")  # Enable the save button
      }
      
      # if (depth(as.list(input$editable_tree)) == depth(as.list(rv$editable_tree))) {
      #     # Only enable the save button if the input has major changes
      #     rv$changes_made <- TRUE  # Mark that changes have been made
      #     shinyjs::enable("save_tree")  # Enable the save button
      #   }
      
    }
  }, ignoreInit = TRUE)
  
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
    
    # Reset changes made tracker and disable the save button
    rv$changes_made <- FALSE
    shinyjs::disable("save_tree")  # Disable the save button again
  })
  
  # Display the previous tree
  output$previous_tree_display <- renderPrint({
    if (is.null(rv$previous_tree)) {
      "No previous tree version saved yet."
    } else {
      cat(paste("\ndepth = ", depth(rv$previous_tree), "\n"))
      cat("previous tree structure\n")      
      str(rv$previous_tree) # Print the previous tree structure
      #cat(str(treeToDf(rv$previous_tree, hierarchy = NULL)))
       
    }
  })
  
  # Display the saved tree
  output$saved_tree_display <- renderPrint({
    if (is.null(rv$saved_tree)) {
      "No tree has been saved yet."
    } else {
      cat(paste("\ndepth = ", depth(rv$saved_tree), "\n"))
      cat("saved tree structure\n")      
      str(rv$saved_tree) # Print the previous tree structure
      cat("saved tree DF structure\n")      
      cat(str(treeToDf(rv$saved_tree, hierarchy = NULL)))
    }
  })
}

shinyApp(ui, server)
