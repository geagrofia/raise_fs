library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)

#Load your CSV file
load_data <- function() {
  df_inn <- read.csv("E:/repos/raise_fs/shiny/data/innovations.csv")
  return(df_inn)
}

#     Variables expected:
#     inn_ID = numeric
#     crop_name	
#     ideotype	
#     scenario


# Define UI
ui <- fluidPage(
  
  useShinyjs(), # for disabling/enabling the actionbuttons
  
  titlePanel("Shiny Table Interface - Single Screen"),
  
  # Layout with sidebar
  sidebarLayout(
    sidebarPanel(
      h3("Existing Innovations:"),
      DTOutput("data_table"),
      actionButton("select_row", "Select an Existing Innovation"),
      actionButton("duplicate_row", "Add New Innovation based on selection"),
      actionButton("add_row", "Add New Innovation"),
      width = 6
    ),
    mainPanel(
      h3("Innovation Details"),
      uiOutput("row_details"),
      uiOutput("edit_controls"),
      width = 6
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  # Reactive data frame to store the CSV data
  df_inn <- reactiveVal(load_data())
  selected_row <- reactiveVal(NULL)
  editing_mode <- reactiveVal(NULL)  # Track the current action: "select", "duplicate", "add"
  
  # Render the data table
  output$data_table <- renderDT({

    datatable(
      df_inn(),
      rownames = F,
      filter = "bottom",
      selection = list(mode = "single", selected = selected_row()),
      editable = FALSE,
      options = list(
        columnDefs = list(list(
          visible = FALSE, targets = c(0) # hide the inn_ID
        )),
        lengthMenu = c(10, 20, 50, 100),
        pageLength = 20,
        order = list(list(1, 'asc'), list(2, 'asc'), list(3, 'asc'))
      )
    )
  }, server = FALSE)
  
  # Initially disable select_row and duplicate_row
  disable("select_row")
  disable("duplicate_row")
  
  # Observe the selected row
  observe({
    selected_row <- input$data_table_rows_selected

    if (length(selected_row) > 0) {
      enable("select_row")
      enable("duplicate_row")
    } else {
      disable("select_row")
      disable("duplicate_row")
    }
  })

  # Handle row selection (Option 1: View without editing)
  observeEvent(input$select_row, {
    req(input$data_table_rows_selected)
    selected_row(df_inn()[input$data_table_rows_selected, ])
    editing_mode("view")  # Set to "view" mode
  })
  
  # Handle duplicate row (Option 2: Edit after duplication)
  observeEvent(input$duplicate_row, {
    req(input$data_table_rows_selected)
    new_row <- df_inn()[input$data_table_rows_selected, ]
    new_row$inn_ID <- max(df_inn()$inn_ID) + 1
    selected_row(new_row)
    editing_mode("edit")  # Set to "edit" mode
  })
  
  # Handle add new row (Option 3: Add a blank row to edit)
  observeEvent(input$add_row, {
    new_row <- data.frame(
      inn_ID = max(df_inn()$inn_ID) + 1,
      crop_name = "",
      ideotype = "",
      scenario = ""
    )
    selected_row(new_row)
    editing_mode("edit")  # Set to "edit" mode
  })
  
  # Render row details (for "view" mode)
  output$row_details <- renderUI({
    req(selected_row())
    if (editing_mode() == "view") {
      tagList(
        h4("Selected Row Values"),
        verbatimTextOutput("selected_values")
      )
    }
  })
  
  # Display selected row's values (read-only)
  output$selected_values <- renderPrint({
    req(selected_row())
    selected_row()
  })
  
  
  
  
  # Render editable inputs (for "edit" mode)
  output$edit_controls <- renderUI({
    req(selected_row())
    if (editing_mode() == "edit") {
      row <- selected_row()
      tagList(
        h4("Edit Innovation"),
        textInput("edit_crop_name", "Crop_name", row$crop_name),
        textInput("edit_ideotype", "Ideotype", row$ideotype),
        textInput("edit_scenario", "Scenario", row$scenario),
        actionButton("save_row", "Save"),
        actionButton("cancel", "Cancel")
      )
    }
  })
  
  # Save edited row and validate (for duplication or adding new rows)
  observeEvent(input$save_row, {
    edited_row <- data.frame(
      inn_ID = selected_row()$inn_ID,
      crop_name = input$edit_crop_name,
      ideotype = input$edit_ideotype,
      scenario = input$edit_scenario
    )
    
    # Validation: Check for duplicates in columns 2-4
    all_df_inn <- df_inn()
    
    duplicate_check <- all_df_inn %>%
      filter(
        crop_name == edited_row$crop_name &
          ideotype == edited_row$ideotype &
          scenario == edited_row$scenario &
          inn_ID != edited_row$inn_ID
      )
    
    if (nrow(duplicate_check) > 0) {
      showModal(
        modalDialog(
          title = "Validation Error",
          "Duplicate values detected. Please modify the values.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } else {
      
      # Validation: check for NA in edited_row
      if ((edited_row$crop_name == "") | grepl("^\\s*$", edited_row$crop_name) |
          (edited_row$ideotype  == "") | grepl("^\\s*$", edited_row$ideotype) |
          (edited_row$scenario  == "") | grepl("^\\s*$", edited_row$scenario)) {
        showModal(
          modalDialog(
            title = "Validation Error",
            "No values detected. Please modify the values.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        
      } else {
        # Save the row
        all_df_inn <- all_df_inn[all_df_inn$inn_ID != edited_row$inn_ID, ]
        all_df_inn <- rbind(all_df_inn, edited_row)
        df_inn(all_df_inn)
        editing_mode("view")  # Reset editing mode
        selected_row(df_inn()[nrow(all_df_inn), ])# select the newly added innovation in the datatable

      }
    }
    
  })

  
  # Cancel editing and clear the UI
  observeEvent(input$cancel, {
    editing_mode(NULL)  # Reset editing mode
    selected_row(NULL)  # Clear selection
  })
}

# Run the application
shinyApp(ui = ui, server = server)
