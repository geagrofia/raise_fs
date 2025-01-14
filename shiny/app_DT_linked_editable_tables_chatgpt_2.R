library(shiny)
library(DT)

# Sample data with additional columns
parent_data <- data.frame(
  ParentID = 1:5,
  Name = c("Parent A", "Parent B", "Parent C", "Parent D", "Parent E"),
  AdditionalColumn1 = c("This is optimal", "Some text", "Another text", "More text", "Optimal"),
  AdditionalColumn2 = c("Just a string", "Optimal value here", "No optimal", "Optimal entry", "Random"),
  AdditionalColumn3 = c("Text", "Value", "Optimal outcome", "Plain text", "Text without keyword"),
  stringsAsFactors = FALSE
)

child_data <- data.frame(
  ChildID = 1:15,
  ParentID = rep(1:5, each = 3),
  Details = c("Detail 1", "Detail 2", "Detail 3"),
  EditableColumn = rep(0, 15),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Parent-Child Table Interaction with Restrictions"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parent Table"),
      DTOutput("parent_table"),
      br(),
      h4("Selected Parent ID"),
      verbatimTextOutput("selected_parent"),
      br(),
      actionButton("save_btn", "Save Changes"),
      br(),
      verbatimTextOutput("validation_message")
    ),
    
    mainPanel(
      h4("Child Table"),
      DTOutput("child_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store global data
  rv <- reactiveValues(data = child_data)
  
  # Reactive value to store temporary edits
  temp_edits <- reactiveVal()
  
  # Reactive value to store validation messages
  validation_message <- reactiveVal("")
  
  # Render the parent table
  output$parent_table <- renderDT({
    datatable(parent_data, selection = "single", editable = TRUE, options = list(dom = 't', pageLength = 5))
  })
  
  # Capture the selected parent ID
  selected_parent_id <- reactive({
    selected_row <- input$parent_table_rows_selected
    if (!is.null(selected_row)) {
      parent_data$ParentID[selected_row]
    } else {
      NULL
    }
  })
  
  # Display the selected parent ID
  output$selected_parent <- renderText({
    selected_parent_id()
  })
  
  # Render the child table filtered by the selected parent ID
  output$child_table <- renderDT({
    req(selected_parent_id()) # Ensure a parent row is selected
    filtered_data <- rv$data[rv$data$ParentID == selected_parent_id(), ]
    
    # Check if any text column includes 'optimal'
    parent_row <- parent_data[parent_data$ParentID == selected_parent_id(), ]
    contains_optimal <- any(grepl("optimal", parent_row[, 3:5], ignore.case = TRUE))
    
    if (contains_optimal) {
      temp_edits(filtered_data) # Store filtered data for temporary edits
      datatable(
        filtered_data,
        editable = FALSE, # Set editable to FALSE if 'optimal' is found
        options = list(dom = 't', pageLength = 5)
      )
    } else {
      temp_edits(filtered_data) # Store filtered data for temporary edits
      datatable(
        filtered_data,
        editable = list(target = "cell", columns = which(colnames(filtered_data) == "EditableColumn")),
        options = list(dom = 't', pageLength = 5)
      )
    }
  })
  
  # Handle edits in the parent table
  observeEvent(input$parent_table_cell_edit, {
    info <- input$parent_table_cell_edit
    row <- info$row
    col <- info$col
    
    if (!is.null(row) && col > 0) {
      parent_data[row, col] <- info$value # Update parent data
    }
  })
  
  # Handle edits in the child table
  observeEvent(input$child_table_cell_edit, {
    info <- input$child_table_cell_edit
    row <- info$row
    col <- info$col # Use the column index as provided (1-based indexing)
    
    # Ensure the column being edited is 'EditableColumn'
    if (!is.null(col) && colnames(temp_edits())[col] == "EditableColumn") {
      edited_data <- temp_edits()
      
      # Validate input is numeric
      new_value <- as.numeric(info$value)
      if (is.na(new_value) || new_value < 0) {
        validation_message("Values must be numeric and non-negative.")
      } else {
        edited_data[row, col] <- new_value
        temp_edits(edited_data) # Save the temporary edit
        
        # Validate that values for the parent sum to 1
        total <- sum(edited_data$EditableColumn)
        if (abs(total - 1) > 1e-6) {
          validation_message("Values must sum to 1.")
        } else {
          validation_message("") # Clear validation message
        }
      }
    }
  })
  
  # Display validation messages
  output$validation_message <- renderText({
    validation_message()
  })
  
  # Save button action
  observeEvent(input$save_btn, {
    req(selected_parent_id()) # Ensure a parent is selected
    edited_data <- temp_edits()
    
    # Validate that values for the parent sum to 1 before saving
    total <- sum(edited_data$EditableColumn)
    if (abs(total - 1) > 1e-6) {
      validation_message("Values must sum to 1 before saving.")
    } else {
      # Update only the 'EditableColumn' in the global dataset
      for (i in 1:nrow(edited_data)) {
        global_row <- which(rv$data$ChildID == edited_data$ChildID[i])
        rv$data[global_row, "EditableColumn"] <- edited_data[i, "EditableColumn"]
      }
      showNotification("Changes saved successfully!", type = "message")
    }
  })
}

# Run the app
shinyApp(ui, server)
