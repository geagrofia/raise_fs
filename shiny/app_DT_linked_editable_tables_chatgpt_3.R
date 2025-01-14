library(shiny)
library(DT)

# Sample parent data
parent_data <- data.frame(
  ParentID = 1:5,
  conc_level_1 = c("poor", "low", "suboptimal", "low", "poor"),
  conc_level_2 = c("moderate", "moderate", "optimal", "moderate", "moderate"),
  conc_level_3 = c("good", "high", NA, "high", "good"),
  stringsAsFactors = FALSE
)

child_data <- data.frame(
  ChildID = 1:15,
  ParentID = rep(1:5, each = 3),
  Details = paste("Detail", 1:15),
  weight = rep(0, 15),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Parent-Child Table with Validations"),
  
  tabsetPanel(
    id = "tabs",
    
    # Screen 1: Edit Parent Table
    tabPanel(
      "Edit Parent Table",
      DTOutput("edit_parent_table"),
      actionButton("save_parent", "Save Parent Table"),
      actionButton("go_to_child", "Next")
    ),
    
    # Screen 2: Parent-Child Interaction
    tabPanel(
      "Parent-Child Tables",
      sidebarLayout(
        sidebarPanel(
          h4("Parent Table"),
          DTOutput("parent_table"),
          br(),
          h4("Selected Parent ID"),
          verbatimTextOutput("selected_parent"),
          h4("Editability Status"),
          verbatimTextOutput("edit_status")
        ),
        
        mainPanel(
          h4("Child Table"),
          DTOutput("child_table"),
          actionButton("save_child", "Save Child Table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store parent and child data
  rv <- reactiveValues(
    parent_data = parent_data,
    child_data = child_data
  )
  
  # Screen 1: Edit Parent Table
  output$edit_parent_table <- renderDT({
    datatable(
      rv$parent_data,
      editable = list(target = "cell", columns = 2:4),
      options = list(dom = 't', pageLength = 5)
    )
  })
  
  # Validate and enforce constraints on parent table edits
  observeEvent(input$edit_parent_table_cell_edit, {
    info <- input$edit_parent_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value
    
    # Handle updates to conc_level_1
    if (col == 2) {  # conc_level_1 column
      if (value %in% c("poor", "low", "suboptimal")) {
        rv$parent_data[row, col] <- value
        # Update dependent columns
        if (value == "poor") {
          rv$parent_data[row, "conc_level_2"] <- "moderate"
          rv$parent_data[row, "conc_level_3"] <- "good"
        } else if (value == "low") {
          rv$parent_data[row, "conc_level_2"] <- "moderate"
          rv$parent_data[row, "conc_level_3"] <- "high"
        } else if (value == "suboptimal") {
          rv$parent_data[row, "conc_level_2"] <- "optimal"
          rv$parent_data[row, "conc_level_3"] <- NA
        }
      } else {
        showNotification("Invalid value for conc_level_1. Allowed: poor, low, suboptimal.", type = "error")
      }
    }
  })
  
  # Save Parent Table
  observeEvent(input$save_parent, {
    showNotification("Parent table saved successfully!", type = "message")
  })
  
  # Navigation to Screen 2
  observeEvent(input$go_to_child, {
    updateTabsetPanel(session, "tabs", selected = "Parent-Child Tables")
  })
  
  # Screen 2: Parent-Child Interaction
  output$parent_table <- renderDT({
    datatable(rv$parent_data, selection = "single")
  })
  
  # Capture the selected parent ID
  selected_parent_id <- reactive({
    selected_row <- input$parent_table_rows_selected
    if (!is.null(selected_row)) {
      rv$parent_data$ParentID[selected_row]
    } else {
      NULL
    }
  })
  
  # Display the selected parent ID
  output$selected_parent <- renderText({
    selected_parent_id()
  })
  
  # Check editability of child table
  is_child_editable <- reactive({
    req(selected_parent_id()) # Ensure a parent row is selected
    
    # Get the corresponding parent row
    parent_row <- rv$parent_data[rv$parent_data$ParentID == selected_parent_id(), ]
    
    # Check if the combination is valid for editing
    conc1 <- parent_row$conc_level_1
    conc2 <- parent_row$conc_level_2
    conc3 <- parent_row$conc_level_3
    
    # Allowed combinations
    valid_combinations <- list(
      list(conc_level_1 = "poor", conc_level_2 = "moderate", conc_level_3 = "good"),
      list(conc_level_1 = "low", conc_level_2 = "moderate", conc_level_3 = "high")
    )
    
    # Check if the current row matches any of the valid combinations
    any(sapply(valid_combinations, function(combo) {
      combo$conc_level_1 == conc1 &&
        combo$conc_level_2 == conc2 &&
        combo$conc_level_3 == conc3
    }))
  })
  
  
  # Display editability status
  output$edit_status <- renderText({
    if (is_child_editable()) {
      "Editable"
    } else {
      "Not Editable"
    }
  })
  
  # Render the child table filtered by the selected parent ID
  output$child_table <- renderDT({
    req(selected_parent_id()) # Ensure a parent row is selected
    
    # Filter child rows for the selected parent
    filtered_data <- rv$child_data[rv$child_data$ParentID == selected_parent_id(), ]
    
    # Determine if the table should be editable
    editable <- is_child_editable()
    
    datatable(
      filtered_data,
      editable = if (editable) list(target = "cell", columns = which(colnames(filtered_data) == "weight")) else FALSE,
      options = list(dom = 't', pageLength = 5)
    )
  })
  
  
  # Save Child Table
  observeEvent(input$child_table_cell_edit, {
    req(is_child_editable()) # Ensure child table is editable
    info <- input$child_table_cell_edit
    row <- info$row
    col <- info$col
    value <- as.numeric(info$value)
    
    if (!is.na(value) && value >= 0 && value <= 1) {
      rv$child_data[row, col] <- value
    } else {
      showNotification("Invalid value. Weight must be numeric and between 0 and 1.", type = "error")
    }
  })
  
  observeEvent(input$save_child, {
    # Validate weights sum to 1 for each parent
    valid <- TRUE
    for (parent_id in unique(rv$child_data$ParentID)) {
      sum_weights <- sum(rv$child_data$weight[rv$child_data$ParentID == parent_id])
      if (abs(sum_weights - 1) > 1e-6) {
        valid <- FALSE
        showNotification(paste("Weights for ParentID", parent_id, "do not sum to 1."), type = "error")
      }
    }
    if (valid) {
      showNotification("Child table saved successfully!", type = "message")
    }
  })
}

# Run the app
shinyApp(ui, server)
