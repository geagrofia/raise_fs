library(shiny)
library(DT)

# Sample initial data
initial_parent_data <- data.frame(
  ID = 1:5,
  conc_level_1 = c("low", "poor", "suboptimal", "poor", "low"),
  conc_level_2 = c("moderate", "moderate", "optimal", "moderate", "moderate"),
  conc_level_3 = c("high", "good", NA, "good", "high"),
  stringsAsFactors = FALSE
)

initial_child_data <- data.frame(
  Parent_ID = rep(1:5, each = 3),
  Child_ID = 1:15,
  Details = paste("Child", 1:15),
  Weight = rep(c(0.3, 0.4, 0.3), 5),
  stringsAsFactors = FALSE
)

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
      uiOutput("dynamic_sidebar")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Screen 1", DTOutput("parent_table")),
        tabPanel("Screen 2", uiOutput("child_ui")),
        tabPanel("Screen 3", uiOutput("screen3_content"))
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    parent_data = initial_parent_data,
    child_data = initial_child_data,
    saved_parent_data = NULL
  )
  
  # Dynamic Sidebar
  output$dynamic_sidebar <- renderUI({
    if (input$main_tabs == "Screen 1") {
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
      tagList(
        actionButton("save_child_table", "Save Child Table"),
        actionButton("to_screen3", "Go to Screen 3"),
        actionButton("back_to_screen1", "Back to Screen 1")
      )
    } else if (input$main_tabs == "Screen 3") {
      tagList(
        actionButton("back_to_screen2", "Back to Screen 2")
      )
    }
  })
  
  # Render Parent Table
  output$parent_table <- renderDT({
    datatable(
      rv$parent_data,
      selection = "single",
      options = list(dom = 't', pageLength = 5)
    )
  })
  
  # Apply Combination to Selected Row
  observeEvent(input$apply_combination, {
    selected_row <- input$parent_table_rows_selected
    selected_combination <- input$combination_dropdown
    
    if (!is.null(selected_row) && !is.null(selected_combination)) {
      combination_details <- valid_combinations[valid_combinations$combination == selected_combination, ]
      
      # Update the selected row in the parent table
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
    # Exclude rows with the 'suboptimal-optimal' combination
    filtered_parent_data <- rv$saved_parent_data[
      !(rv$saved_parent_data$conc_level_1 == "suboptimal" & rv$saved_parent_data$conc_level_2 == "optimal"), 
    ]
    datatable(filtered_parent_data, selection = "single", options = list(dom = 't', pageLength = 5))
  })
  
  
  # Child Table on Screen 2
  output$child_table_screen2 <- renderDT({
    req(input$parent_table_screen2_rows_selected)
    filtered_parent_data <- rv$saved_parent_data[
      !(rv$saved_parent_data$conc_level_1 == "suboptimal" & rv$saved_parent_data$conc_level_2 == "optimal"), 
    ]
    selected_row <- input$parent_table_screen2_rows_selected
    
    if (!is.null(selected_row)) {
      parent_row <- filtered_parent_data[selected_row, ]
      filtered_child_data <- rv$child_data[rv$child_data$Parent_ID == parent_row$ID, ]
      
      # Child table is always editable here since 'suboptimal-optimal' rows are excluded
      datatable(
        filtered_child_data,
        editable = list(target = "cell", columns = 4),  # Make only the Weight column editable
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
      row <- updated_child_data$row
      col <- updated_child_data$col
      value <- as.numeric(updated_child_data$value)
      
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
  
  # Navigate to Screen 3
  observeEvent(input$to_screen3, {
    updateTabsetPanel(session, "main_tabs", selected = "Screen 3")
  })
  
  # Navigate back to Screen 2
  observeEvent(input$back_to_screen2, {
    updateTabsetPanel(session, "main_tabs", selected = "Screen 2")
  })
  
  # Screen 3 UI
  output$screen3_content <- renderUI({
    fluidPage(
      h4("Screen 3: Additional Features"),
      p("This is where you can add further features or display summary information.")
    )
  })
  
  
}

shinyApp(ui, server)
