library(shiny)
library(DT)
library(tidyverse)
 

# Sample data
parent_table <- data.frame(
  ID = 1:5,
  Name = LETTERS[1:5],
  stringsAsFactors = FALSE
)

child_table <- data.frame(
  ParentID = c(1, 1, 2, 3, 3, 3, 4, 5),
  Value = c("A1", "A2", "B1", "C1", "C2", "C3", "D1", "E1"),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Linked Editable Tables"),
  sidebarLayout(
    sidebarPanel(
      h3("Parent Table"),
      DTOutput("parent_table")
    ),
    mainPanel(
      h3("Child Table"),
      DTOutput("child_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Render parent table
  output$parent_table <- renderDT({
    datatable(parent_table, selection = "single")
  })
  
  # Reactive value to store selected parent ID
  selected_parent_id <- reactive({
    if (!is.null(input$parent_table_rows_selected)) {
      parent_table$ID[input$parent_table_rows_selected]
    } else {
      NULL
    }
  })
  
  # Filter child table based on selected parent
  filtered_child_table <- reactive({
    if (!is.null(selected_parent_id())) {
      child_table %>%
        mutate(Editable = ParentID == selected_parent_id())
    } else {
      child_table %>%
        mutate(Editable = FALSE)
    }
  })
  
  # Render child table
  output$child_table <- renderDT({
    datatable(
      filtered_child_table(),
      selection = "none",
      editable = list(target = "cell", disable = list(columns = c(0, 1, 3))), # Only allow editing of specific rows
      options = list(
        rowCallback = JS(
          "function(row, data, index) {",
          "  if (data[3] === false) {", # Editable flag in column 4 (index 2 in JS)
          "    $(row).css('color', 'red');", # Gray out non-editable rows
          "  }",
          "}"
        )
      )
    )
  })
  
  # Observe edits to child table
  observeEvent(input$child_table_cell_edit, {
    info <- input$child_table_cell_edit
    str(info) # For debugging
    
    # Update the reactive data source
    child_table[info$row, info$col] <<- info$value
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
