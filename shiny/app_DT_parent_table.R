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


# UI
ui <- fluidPage(
  titlePanel("Parent-Child Table Interaction with Restrictions"),
  
  sidebarLayout(
    sidebarPanel(
      br(),
      h4("Selected Parent ID"),
      verbatimTextOutput("selected_parent"),
      br(),
      actionButton("save_btn", "Save Changes"),
      br(),
      verbatimTextOutput("validation_message")
    ),
    
    mainPanel(
      h4("Parent Table"),
      DTOutput("parent_table"),
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store global data
  
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
  
  # Handle edits in the parent table
  observeEvent(input$parent_table_cell_edit, {
    info <- input$parent_table_cell_edit
    row <- info$row
    col <- info$col
    
    if (!is.null(row) && col > 0) {
      parent_data[row, col] <- info$value # Update parent data
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
    
  })
}

# Run the app
shinyApp(ui, server)
