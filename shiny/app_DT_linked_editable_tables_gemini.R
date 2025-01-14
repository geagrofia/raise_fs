library(shiny)
library(DT)

# Sample data
parent_data <- data.frame(
  ParentID = 1:5,
  Name = c("Parent A", "Parent B", "Parent C", "Parent D", "Parent E"),
  stringsAsFactors = FALSE
)

child_data <- data.frame(
  ChildID = 1:15,
  ParentID = rep(1:5, each = 3),
  Details = c("Detail 1", "Detail 2", "Detail 3"),
  EditableColumn = rep("", 15),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Parent-Child Table Interaction"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parent Table"),
      DTOutput("parent_table"),
      br(),
      h4("Selected Parent ID"),
      verbatimTextOutput("selected_parent"),
      br(),
      actionButton("save_btn", "Save Changes")
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
  
  # Render the parent table
  output$parent_table <- renderDT({
    datatable(parent_data, selection = "single")
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
    temp_edits(filtered_data) # Store filtered data for temporary edits
    datatable(
      filtered_data,
      editable = list(target = "cell", columns = which(colnames(filtered_data) == "EditableColumn")),
      options = list(dom = 't', pageLength = 5)
    )
  })
  
  # Handle edits in the child table
  observeEvent(input$child_table_cell_edit, {
    info <- input$child_table_cell_edit
    row <- info$row
    col <- info$col #+ 1 # Adjust for 0-based indexing in editable
    
    print(col)
    
    # Validate column index and name before editing
    edited_data <- temp_edits()
    if (!is.null(col) && col > 0 && col <= ncol(edited_data)) {
      if (colnames(edited_data)[col] == "EditableColumn") {
        edited_data[row, col] <- info$value
        temp_edits(edited_data) # Save the temporary edit
      }
    }
  })
  
  # Save button action
  observeEvent(input$save_btn, {
    req(selected_parent_id()) # Ensure a parent is selected
    edited_data <- temp_edits()
    
    # Update only the 'EditableColumn' in the global dataset
    for (i in 1:nrow(edited_data)) {
      global_row <- which(rv$data$ChildID == edited_data$ChildID[i])
      rv$data[global_row, "EditableColumn"] <- edited_data[i, "EditableColumn"]
    }
    showNotification("Changes saved successfully!", type = "message")
  })
}

# Run the app
shinyApp(ui, server)
