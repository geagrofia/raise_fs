# modules/screen4.R

library(DT)



bslib_screen4_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  
  tagList(
    h3("Existing Innovations:"),
    
    DTOutput(ns("data_table")),
    actionButton(ns("select_row"), "Select an Existing Innovation"),
    actionButton(ns("duplicate_row"), "Add New Innovation based on selection"),
    actionButton(ns("add_row"), "Add New Innovation"),
    
    # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen3"), "Back to Screen 3"),
    actionButton(ns("to_screen5"), "Go to Screen 5")
  )
}

bslib_screen4_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("value_display")),
    textOutput(ns("level_display")),
    textOutput(ns("selection_display")),
    textOutput(ns("spatres_display")),
    textOutput(ns("aggregation_display")),
    textOutput(ns("num_innovations_display")),
    textOutput(ns("innovation_system_display")),
    uiOutput(ns("row_details")),
    uiOutput(ns("edit_controls"))
  )
}

bslib_screen4_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    #Load your CSV file----
    load_data <- function() {
      df_inn <- read.csv("E:/repos/raise_fs/shiny/data/innovations.csv")
      return(df_inn)
    }
    
    #     Variables expected:
    #     inn_ID = numeric
    #     crop_name	
    #     ideotype	
    #     scenario
    df_inn <- reactiveVal(load_data())
    selected_row <- reactiveVal(NULL)
    editing_mode <- reactiveVal(0)  # Track the current action: "select", "duplicate", "add"
    
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
    
    # Initially disable select_row and duplicate_row, and next_screen
    disable("select_row")
    disable("duplicate_row")
    
    # observe the editing_mode to determine next_screen visibility
    observe({
      if (editing_mode() == 0 | editing_mode() == "edit") {
        disable("next_screen")
      } else {
        enable("next_screen")
      }
    })
    
    # Observe the editing mode
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
      editing_mode(0)  # Reset editing mode
      selected_row(NULL)  # Clear selection
    })
    
    
    output$num_innovations_display <- renderText({
      paste("Number of innovations:", shared_values$num_innovations)
    })
    
    output$innovation_system_display <- renderText({
      if (shared_values$num_innovations == "two_inn") {
        paste("Innovation System:", shared_values$innovation_system)
      }
    })    
    
    output$spatres_display <- renderText({
      paste("Your spatial resolution is:", shared_values$resolution)
    })

    
    output$aggregation_display <- renderText({
      paste("Your aggregation level is:", shared_values$aggregation)
    })
    #     
    # output$value_display <- renderText({
    #   req(shared_values$dropdown)
    #   paste("You selected on Screen 1:", shared_values$dropdown)
    # })
    
    output$level_display <- renderText({
      req(shared_values$level)
      paste("You selected level on Screen 1:", shared_values$level)
    })
    
    output$selection_display <- renderText({
      req(shared_values$level)
      
      if (shared_values$level == "woreda") {
        paste(
          "You selected geography:",
          shared_values$selected_region,
          shared_values$selected_zone,
          shared_values$selected_woreda
        )
      } else {
        if (shared_values$level == "zone") {
          paste(
            "You selected geography:",
            shared_values$selected_region,
            shared_values$selected_zone
          )
        } else {
          if (shared_values$level == "region") {
            paste("You selected geography:",
                  shared_values$selected_region)
          } else {
            paste("You selected geography: Ethiopia")
          }
        }
      }
    }) 
    

    #2 observeEvent back_to_screen1 ----
    observeEvent(input$back_to_screen3, {
      switch_screen("screen3")
    })
    
    #2 observeEvent to_screen3 ----
    observeEvent(input$to_screen5, {
      switch_screen("screen5")
    })
    
  })
}
