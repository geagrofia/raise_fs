library(shiny)
library(DT)

# Define the initial parent table
initial_parent_data <- data.frame(
  ID = 1:5,
  conc_level_1 = c("poor", "low", "suboptimal", NA, NA),
  conc_level_2 = c("moderate", "moderate", "optimal", NA, NA),
  conc_level_3 = c("good", "high", NA, NA, NA),
  stringsAsFactors = FALSE
)

# Define the initial child table
initial_child_data <- data.frame(
  Child_ID = c("Child_1", "Child_2", "Child_3", "Child_4", "Child_5", "Child_6", "Child_7", "Child_8", "Child_9", "Child_10", "Child_11", "Child_12", "Child_13"),
  Parent_ID = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
  Weight = c(0.33, 0.33, 0.34, 0.5, 0.5, 0, 0, 0, 0, 0, 0, 0, 0),
  stringsAsFactors = FALSE
)

# Define valid combinations
valid_combinations <- data.frame(
  combination = c("poor-moderate-good", "low-moderate-high", "suboptimal-optimal-NA"),
  conc_level_1 = c("poor", "low", "suboptimal"),
  conc_level_2 = c("moderate", "moderate", "optimal"),
  conc_level_3 = c("good", "high", NA),
  stringsAsFactors = FALSE
)


ui <- fluidPage(
  titlePanel("Edit Parent and Child Tables"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("dynamic_sidebar")  # Dynamic sidebar to switch between screens
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Screen 1", DTOutput("parent_table")),
        tabPanel("Screen 2", uiOutput("child_ui"))
      )
    )
  )
)


server <- function(input, output, session) {
  # Reactive Values for Parent and Child Tables
  rv <- reactiveValues(
    parent_data = initial_parent_data,   # Use the initial parent table
    child_data = initial_child_data,    # Use the initial child table
    saved_parent_data = NULL            # To store saved parent data
  )
  
  # Dynamic Sidebar
  output$dynamic_sidebar <- renderUI({
    if (input$main_tabs == "Screen 1") {
      # Sidebar for Screen 1
      tagList(
        h4("Select a Combination"),
        selectInput("combination_dropdown", "Combination", 
                    choices = valid_combinations$combination, selected = NULL),
        actionButton("apply_combination", "Apply to Selected Row"),
        br(),
        actionButton("save_parent_table", "Save Parent Table"),
        actionButton("next_screen", "Go to Screen 2")
      )
    } else if (input$main_tabs == "Screen 2") {
      # Sidebar for Screen 2
      tagList(
        actionButton("save_child_table", "Save Child Table"),
        actionButton("back_to_screen1", "Back to Screen 1")
      )
    }
  })
  
  # Render Parent Table
  output$parent_table <- renderDT({
    datatable(
      rv$parent_data,
      selection = "single",  # Allow selecting one row
      options = list(dom = 't', pageLength = 5)
    )
  })
  
  # Apply Combination to Selected Row
  observeEvent(input$apply_combination, {
    selected_row <- input$parent_table_rows_selected
    selected_combination <- input$combination_dropdown
    
    if (!is.null(selected_row) && !is.null(selected_combination)) {
      # Find combination details
      combination_details <- valid_combinations[valid_combinations$combination == selected_combination, ]
      
      # Update the selected row
      rv$parent_data[selected_row, "conc_level_1"] <- combination_details$conc_level_1
      rv$parent_data[selected_row, "conc_level_2"] <- combination_details$conc_level_2
      rv$parent_data[selected_row, "conc_level_3"] <- combination_details$conc_level_3
    }
  })
  
  # Save Parent Table
  observeEvent(input$save_parent_table, {
    rv$saved_parent_data <- rv$parent_data
    showNotification("Parent table saved successfully!", type = "message")
  })
  
  # Navigate to Screen 2
  observeEvent(input$next_screen, {
    if (is.null(rv$saved_parent_data)) {
      showNotification("Please save the parent table before moving to the next screen.", type = "error")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "Screen 2")
    }
  })
  
  # Back to Screen 1
  observeEvent(input$back_to_screen1, {
    updateTabsetPanel(session, "main_tabs", selected = "Screen 1")
  })
  
  # Screen 2 Logic (Child Table)
  output$child_ui <- renderUI({
    req(rv$saved_parent_data)
    fluidPage(
      h4("Parent and Child Tables"),
      DTOutput("parent_table_screen2"),
      DTOutput("child_table_screen2")
    )
  })
  
  # Parent Table on Screen 2
  output$parent_table_screen2 <- renderDT({
    req(rv$saved_parent_data)
    datatable(rv$saved_parent_data, selection = "single", options = list(dom = 't', pageLength = 5))
  })
  
  # Child Table on Screen 2
  output$child_table_screen2 <- renderDT({
    req(input$parent_table_screen2_rows_selected)
    selected_row <- input$parent_table_screen2_rows_selected
    
    # Filter child data based on selected parent row
    if (!is.null(selected_row)) {
      filtered_child_data <- rv$child_data[rv$child_data$Parent_ID == rv$saved_parent_data$ID[selected_row], ]
      datatable(
        filtered_child_data,
        editable = list(target = "cell", columns = 3),  # Make only the Weight column editable
        options = list(dom = 't', pageLength = 5)
      )
    }
  })
  
  # Save Child Table
  observeEvent(input$save_child_table, {
    selected_row <- input$parent_table_screen2_rows_selected
    if (is.null(selected_row)) {
      showNotification("Please select a parent row to save child table changes.", type = "error")
      return()
    }
    
    # Retrieve the edited child data
    updated_child_data <- isolate(input$child_table_screen2_cell_edit)
    if (!is.null(updated_child_data)) {
      # Apply changes
      row <- updated_child_data$row
      col <- updated_child_data$col
      value <- as.numeric(updated_child_data$value)
      
      # Update the reactive child data
      parent_id <- rv$saved_parent_data$ID[selected_row]
      rv$child_data[rv$child_data$Parent_ID == parent_id, "Weight"][row] <- value
      
      # Validate the sum of weights
      child_subset <- rv$child_data[rv$child_data$Parent_ID == parent_id, ]
      if (round(sum(child_subset$Weight), 2) != 1) {
        showNotification("Weights must sum to 1.", type = "error")
      } else {
        showNotification("Child table saved successfully!", type = "message")
      }
    }
  })
}




# Run the app
shinyApp(ui, server)
