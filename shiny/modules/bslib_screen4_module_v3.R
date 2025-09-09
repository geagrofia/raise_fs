# modules/screen4.R

library(DT)


bslib_screen4_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(
    #     ns("back_to_screen3"),
    #     label = tagList(
    #       icon("circle-left"),
    #       # icon first
    #       "Back to Screen 3"
    #       # text second
    #     ),
    #     class = "btn-primary"
    #   ),
    #   actionButton(
    #     ns("to_screen5"),
    #     label = tagList(
    #       "Go to Screen 5",
    #       # text first
    #       icon("circle-right")  # icon second)
    #     ),
    #     class = "btn-primary"
    #   )
    # ), 
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      h4("Screen 4: Select Innovation"),
      
      # UI datatable ----
      DTOutput(ns("data_table")),
      
      # UI actionbuttons row selections----
      actionButton(ns("select_row"), "Select an Existing Innovation", class = "btn-primary"),
      actionButton(ns("duplicate_row"), "Add New Innovation based on selection", class = "btn-primary"),
      actionButton(ns("add_row"), "Add New Innovation", class = "btn-primary")
    ),
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      # h4("Screen 4: Select Innovation"),
      # UI row details (view) or edit controls (edit)----
      uiOutput(ns("edit_controls")),
      uiOutput(ns("validate_butons"))
    )
  )
}

bslib_screen4_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen3"), 
                   title = "Go back to Step 3",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 4", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen5"), 
                   title = "Go to Step 5",
                   label = tagList(
                     #"Go to Screen 2",
                     "Next",
                     # text first
                     icon("circle-right")  # icon second)
                   ),
                   class = "btn-primary")
      #,
      #actionButton(ns("save_progress"), "Save Progress"),
      #actionButton(ns("resume_progress"), "Resume Progress")
    ),
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px; background: rgba(23, 162, 184, 0.5);",
      h4("Summary of IRM setup"),
      textOutput(ns("value_display")),
      textOutput(ns("level_display")),
      textOutput(ns("selection_display")),
      textOutput(ns("spatres_display")),
      textOutput(ns("aggregation_display")),
      textOutput(ns("num_innovations_display")),
      textOutput(ns("innovation_system_display")),
      uiOutput(ns("row_details"))
    )
  )
}

bslib_screen4_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
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
    editing_mode <- reactiveVal("view")  # Track the editing mode: "view", "edit"
    
    # Render the data table----
    output$data_table <- renderDT({
      
      
      message(paste("S4. Initiation. editing_mode:", editing_mode()))
      message(paste("S4. Initiation. shared_values$inn_type_1:", shared_values$inn_type_1))
      message(paste("S4. Initiation. forget:", shared_values$forget))
      message(paste("S4. Initiation. num_innovations", shared_parameters$num_innovations))
      message(paste("S4. Initiation. inn details1:", shared_parameters$crop_name_1,"-", shared_parameters$ideotype_1,"-", shared_parameters$scenario_1))
     
      
      datatable(
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center;',
          "Existing Innovations:"),
        df_inn(),
        rownames = F,
        filter = "bottom",
        selection = list(mode = "single", selected = selected_row()),
        editable = FALSE,
        options = list(
          dom = '<t><"bottom"lip>',
          columnDefs = list(list(
            visible = FALSE, targets = c(0) # hide the inn_ID
          )),
          lengthMenu = c(10, 20, 50, 100),
          pageLength = 20,
          order = list(list(1, 'asc'), list(2, 'asc'), list(3, 'asc'))
        )
      )
    }, server = FALSE)
    
    # Initially disable select_row and duplicate_row, and next_screen ----
    disable("select_row")
    disable("duplicate_row")
    disable("to_screen5")


    # observe the selected row----
    observe({
      selected_row <- input$data_table_rows_selected
      
      if (length(selected_row) > 0) {
        enable("select_row")
        enable("duplicate_row")
        enable("to_screen5")
        cat("\n\ndisable r\n\n")
      } else {
        disable("select_row")
        disable("duplicate_row")
        disable("to_screen5")
        cat("\n\ndisable r\n\n")
      }
    })
    
    
    # observeEvent row selection (Option 1: View without editing)----
    observeEvent(input$select_row, {
      req(input$data_table_rows_selected)
      selected_row(df_inn()[input$data_table_rows_selected, ])
      shared_parameters$crop_name_1 <-  selected_row()$crop_name
      shared_parameters$ideotype_1 <- selected_row()$ideotype
      shared_parameters$scenario_1 <- selected_row()$scenario
      
      # ensure that duplicated innovation is NULL
      shared_values$crop_name_0  <- NULL
      shared_values$ideotype_0 <- NULL
      shared_values$scenario_0 <- NULL
      message(paste("S4. YY observeEvent: existing inn. original inn (should be NULL):", shared_values$crop_name_0,"-", shared_values$ideotype_0,"-", shared_values$scenario_0))
      
      editing_mode("view")  # Set to "view" mode
      shared_values$inn_type_1 <- "existing"#  Set to "existing" innovation mode
    })
    
    # observeEvent duplicate row (Option 2: Edit after duplication)----
    observeEvent(input$duplicate_row, {
      req(input$data_table_rows_selected)
      new_row <- df_inn()[input$data_table_rows_selected, ]
      new_row$inn_ID <- max(df_inn()$inn_ID) + 1
      selected_row(new_row)
      # save the duplicated innovation for later copying
      shared_values$crop_name_0  <- new_row$crop_name
      shared_values$ideotype_0 <- new_row$ideotype
      shared_values$scenario_0 <- new_row$scenario
      message(paste("S4. YY observeEvent: duplicate row. inn to duplicate:", shared_values$crop_name_0,"-", shared_values$ideotype_0,"-", shared_values$scenario_0))
      editing_mode("edit")  # Set to "edit" mode
      shared_values$inn_type_1 <- "duplicate"#  Set to "duplicate" innovation mode
      
    })
    
    # observeEvent add new row (Option 3: Add a blank row to edit)----
    observeEvent(input$add_row, {
      new_row <- data.frame(
        inn_ID = max(df_inn()$inn_ID) + 1,
        crop_name = "",
        ideotype = "",
        scenario = ""
      )
      selected_row(new_row)
      # ensure that duplicated innovation is NULL
      shared_values$crop_name_0  <- NULL
      shared_values$ideotype_0 <- NULL
      shared_values$scenario_0 <- NULL
      message(paste("S4. YY observeEvent: new row. original inn (should be NULL):", shared_values$crop_name_0,"-", shared_values$ideotype_0,"-", shared_values$scenario_0))
      
      editing_mode("edit")  # Set to "edit" mode
      shared_values$inn_type_1 <- "new"#  Set to "new" innovation mode
    })
    
    # output Render row details (for "view" mode)----
    output$row_details <- renderUI({
      req(selected_row())
      if (editing_mode() == "view") {
        tagList(
          p("Step 4. Your innovation is:"),
          verbatimTextOutput(ns("selected_values"))
        )
      }
    })
    
    # output Display selected row's values (read-only)----
    output$selected_values <- renderPrint({
      req(selected_row())
      selected_row()[2:4]
    })
    
    
    # output Render editable inputs (for "edit" mode)----
    output$edit_controls <- renderUI({
      req(selected_row())
      if (editing_mode() == "edit") {
        row <- selected_row()
        tagList(
          h4("Edit Innovation"),
          # textInput(ns("edit_crop_name"), "Crop_name", row$crop_name),
          # textInput(ns("edit_ideotype"), "Ideotype", row$ideotype),
          # textInput(ns("edit_scenario"), "Scenario", row$scenario),
          textInput(ns("edit_crop_name"), "Crop_name", row$crop_name),
          textInput(ns("edit_ideotype"), "Ideotype", row$ideotype),
          textInput(ns("edit_scenario"), "Scenario", row$scenario)#,
          
          # actionButton(ns("save_row_btn"), "Save"),
          # actionButton(ns("cancel_btn"), "Cancel")
        )
        #disable("save_row_btn")
      }
    })
    
   # observe changes to crop_name text input and have new mode - "validate"----
    observe({
       req(input$edit_crop_name)
       req(selected_row())
       
       #changed <- TRUE
       
       changed <- input$edit_crop_name != selected_row()$crop_name ||
         input$edit_ideotype != selected_row()$ideotype ||
         input$edit_scenario != selected_row()$scenario

      if (changed) {
        enable("save_row_btn")
        message(paste("S4. XX observeEvent crop_name text inputs:", selected_row()$crop_name," - ", input$edit_crop_name))    } else {
          disable("save_row_btn")}
    })
    
    # observe changes to  ideotype text input and have new mode - "validate"----
    observe({
      req(input$edit_ideotype)
      req(selected_row())
      #changed <- TRUE
      
       changed <- input$edit_crop_name != selected_row()$crop_name ||
         input$edit_ideotype != selected_row()$ideotype ||
         input$edit_scenario != selected_row()$scenario
      
      if (changed) {
        enable("save_row_btn")
        message(paste("S4. XX observeEvent ideotype text inputs:", selected_row()$ideotype," - ", input$edit_ideotype))    } else {
          disable("save_row_btn")        }
    })
    
    # observe changes to scenariotext input and have new mode - "validate"----
    observe({
      req(input$edit_scenario)
      req(selected_row())
      #changed <- TRUE
      
      changed <- input$edit_crop_name != selected_row()$crop_name ||
        grepl("^\\s*$", input$edit_crop_name) || 
        input$edit_ideotype != selected_row()$ideotype ||
        grepl("^\\s*$", input$edit_ideotype) || 
        input$edit_scenario != selected_row()$scenario ||
        grepl("^\\s*$", input$edit_scenario)
      
      if (changed) {
        enable("save_row_btn")
        message(
          paste(
            "S4. XX observeEvent scenario selected_row:",
            selected_row()$scenario,
            " - input:",
            input$edit_scenario,
            " - grepl:",
            grepl("^\\s*$", input$edit_scenario)
          )
        )
      } else {
      disable("save_row_btn")
    }})
    


    
    # observeEvent Save edited row and validate (for duplication or adding new rows)----
   observeEvent(ns(input$save_row_btn), {
     message(paste("S4. 2a observeEvent: save_row_btn Edit mode:", editing_mode()))
     message(paste("S4. 2a observeEvent: save_row_btn forget:", shared_values$forget))
     message(paste("S4. 2a observeEvent: save_row_btn inn details:", shared_parameters$crop_name_1,"-", shared_parameters$ideotype_1,"-", shared_parameters$scenario_1))
     
     if (shared_values$forget > 0 ) {
       
      message(paste("S4. 2b observeEvent: save_row_btn Edit mode:", editing_mode()))
       
      if (!is.null(selected_row())){ #added
        message(paste("S4. 2c observeEvent: save_row_btn Edit mode:", editing_mode()))  
      edited_row <- data.frame(
        inn_ID = selected_row()$inn_ID,
        crop_name = input$edit_crop_name,
        ideotype = input$edit_ideotype,
        scenario = input$edit_scenario
      )
      
      # Validation: Check for duplicates in columns 2-4
      all_df_inn <- df_inn()
      
      duplicate_check <- all_df_inn |>
        dplyr::filter(
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
          shared_parameters$crop_name_1 <- edited_row$crop_name
          shared_parameters$ideotype_1 <- edited_row$ideotype
          shared_parameters$scenario_1 <- edited_row$scenario
            
          all_df_inn <- all_df_inn[all_df_inn$inn_ID != edited_row$inn_ID, ]
          all_df_inn <- rbind(all_df_inn, edited_row)
          df_inn(all_df_inn)
          editing_mode("view")  # Reset editing mode
          selected_row(df_inn()[nrow(all_df_inn), ])# select the newly added innovation in the datatable

        }
      }
      }
      } #added
     shared_values$forget <- (shared_values$forget + 1)
     message(paste("S4. 2c observeEvent: shared_values$forget = ", shared_values$forget))
    }, ignoreInit = TRUE)
    
    
    # observeEvent cancel_btn editing and clear the UI----
    observeEvent(ns(input$cancel_btn), {
      
      message(paste("S4. 3a observeEvent: cancel_btn. Edit mode:", editing_mode()))
      message(paste("S4. 3a observeEvent: cancel_btn. forget:", shared_values$forget))
      message(paste("S4. 3a observeEvent: cancel_btn. inn details:", shared_parameters$crop_name_1,"-", shared_parameters$ideotype_1,"-", shared_parameters$scenario_1))
      
      
      if (shared_values$forget > 1 ) { 
        
      editing_mode("view")  # Reset editing mode
      message(paste("S4. a observeEvent: cancel_btn. shared_values$forget = ", shared_values$forget))
      selected_row(NULL)  # Clear selection
      }
    }, ignoreInit = TRUE)
    
    
    
    # output Render validation buttons (for "edit" mode)----
    output$validate_butons <- renderUI({
      
      message(paste("S4. 1", shared_values$forget))
      req(selected_row())
      
      message(paste("S4. 2",  shared_values$forget))
      req(input$edit_crop_name)
      message(paste("S4. 3",  shared_values$forget))
      
      if (editing_mode() == "edit") {
        message(paste("S4. 4",  shared_values$forget))
        row <- selected_row()
        tagList(
          h4("Validate"),
          actionButton(ns("save_row_btn"), 
                       label = tagList(
                         "Save Innovation",
                         # text first
                         icon("floppy-disk")  # icon second)
                       ),
                       class = "btn-primary"),
          actionButton(ns("cancel_btn"), 
                       label = tagList(
                         "Cancel",
                         # text first
                         icon("rectangle-xmark")  # icon second)
                       ),
                       class = "btn-primary")
        )
      }
    })
    
    # _----
    
    # outputs from previous screens----
    
    output$num_innovations_display <- renderText({
      paste("Step 3. Number of innovations =", shared_parameters$num_innovations)
    })
    
    output$innovation_system_display <- renderText({
      paste("Step 3. Innovation System =", shared_parameters$innovation_system)
    })   
    
    output$spatres_display <- renderText({
      paste("Step 2. Spatial resolution =", shared_parameters$resolution)
    })
    
    output$aggregation_display <- renderText({
      paste("Step 2. Aggregation level =", shared_parameters$aggregation)
    })
    
    output$level_display <- renderText({
      req(shared_parameters$level)
      paste("Step 1. Spatial level =", shared_parameters$level)
    })
    
    output$selection_display <- renderText({
      req(shared_parameters$level)
      
      if (shared_parameters$level == "woreda") {
        paste(
          "Step 1. Geography =",
          shared_parameters$selected_region,
          "-",
          shared_parameters$selected_zone,
          "-",
          shared_parameters$selected_woreda
        )
      } else {
        if (shared_parameters$level == "zone") {
          paste(
            "Step 1. Geography =",
            shared_parameters$selected_region,
            "-",
            shared_parameters$selected_zone
          )
        } else {
          if (shared_parameters$level == "region") {
            paste("Step 1. Geography =",
                  shared_parameters$selected_region)
          } else {
            paste("Step 1. Geography = Ethiopia")
          }
        }
      }
    }) 
    

    #2 observeEvent back_to_screen3 ----
    observeEvent(input$back_to_screen3, {
      switch_screen("screen3")
    })
    
    #2 observeEvent to_screen5 ----
    observeEvent(input$to_screen5, {
      req(selected_row())
      
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      
      # if existing innovation selected then edit mode is view
      # do nothing - load using values for shared_values$crop_name_1 etc.
      if (shared_values$inn_type_1 == "existing")  {
        message(paste("S4. ZZ1 observeEvent newscreen: existing inn", shared_values$crop_name_0,"-", shared_values$ideotype_0,"-", shared_values$scenario_0))
        message(paste("S4. ZZ1 observeEvent newscreen: new inn", shared_parameters$crop_name_1,"-", shared_parameters$ideotype_1,"-", shared_parameters$scenario_1))
        message(paste("S4. ZZ1 observeEvent newscreen: shared_values$current_tree", shared_values$current_tree))
        
        switch_screen("screen5")
      }
      
      # if duplicate innovation selected then edit mode is edit and crop_name_0 etc have a value
      # create a new requirements table using values for shared_values$crop_name_0 etc.
      
      if (shared_values$inn_type_1 == "duplicate" && !is.null(shared_values$crop_name_0))  {
        message(paste("S4. ZZ2 observeEvent newscreen: existing inn", shared_values$crop_name_0,"-", shared_values$ideotype_0,"-", shared_values$scenario_0))
        message(paste("S4. ZZ2 observeEvent newscreen: new inn", shared_parameters$crop_name_1,"-", shared_parameters$ideotype_1,"-", shared_parameters$scenario_1))
        
        file.copy(
          # existing innovation requirements to be duplicated----
          paste0("E:/repos/raise_fs/shiny/data/", 
                         shared_values$crop_name_0,
                         "_",
                         shared_values$ideotype_0,
                         "_", 
                         shared_values$scenario_0,
                         "_requirements.csv"),
          # new innovation
                  paste0("E:/repos/raise_fs/shiny/data/", 
                         shared_parameters$crop_name_1,
                         "_",
                         shared_parameters$ideotype_1,
                         "_", 
                         shared_parameters$scenario_1,
                         "_requirements.csv"),
                  overwrite = TRUE)

        file.copy(
          # existing innovation links to be duplicated----
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_values$crop_name_0,
                 "_",
                 shared_values$ideotype_0,
                 "_", 
                 shared_values$scenario_0,
                 "_links.csv"),
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_links.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation growth stages to be duplicated----
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_values$crop_name_0,
                 "_",
                 shared_values$ideotype_0,
                 "_", 
                 shared_values$scenario_0,
                 "_gs.csv"),
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_gs.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation soil texture to be duplicated----
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_values$crop_name_0,
                 "_",
                 shared_values$ideotype_0,
                 "_", 
                 shared_values$scenario_0,
                 "_texture.csv"),
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_texture.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation drainage to be duplicated----
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_values$crop_name_0,
                 "_",
                 shared_values$ideotype_0,
                 "_", 
                 shared_values$scenario_0,
                 "_drainage.csv"),
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_drainage.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation yield to be duplicated----
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_values$crop_name_0,
                 "_",
                 shared_values$ideotype_0,
                 "_", 
                 shared_values$scenario_0,
                 "_yield.csv"),
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_yield.csv"),
          overwrite = TRUE)        
        
        switch_screen("screen5")
      }
      
      # if a new innovation selected then edit mode is view and crop_name_0 etc are NULL
      # create a new requirements table using filename from shared_values$crop_name_1 etc. and completely generic values from a generic requirements file
      
      if (shared_values$inn_type_1 == "new" && is.null(shared_values$crop_name_0))  {
        message(paste("S4. ZZ3 observeEvent newscreen: existing inn", shared_values$crop_name_0,"-", shared_values$ideotype_0,"-", shared_values$scenario_0))
        message(paste("S4. ZZ3 observeEvent newscreen: new inn", shared_parameters$crop_name_1,"-", shared_parameters$ideotype_1,"-", shared_parameters$scenario_1))
        file.copy(
          # generic requirements
          "E:/repos/raise_fs/shiny/data/generic-generic-generic.csv", 
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 ".csv"),
          overwrite = TRUE)
        
        
        file.copy(
          # existing innovation requirements to be duplicated----
          "E:/repos/raise_fs/shiny/data/generic_generic_generic_requirements.csv",
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_requirements.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation links to be duplicated----
          "E:/repos/raise_fs/shiny/data/generic_generic_generic_links.csv",
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_links.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation growth stages to be duplicated----
          "E:/repos/raise_fs/shiny/data/generic_generic_generic_gs.csv",
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_gs.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation soil texture to be duplicated----
          "E:/repos/raise_fs/shiny/data/generic_generic_generic_texture.csv",
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_texture.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation drainage to be duplicated----
          "E:/repos/raise_fs/shiny/data/generic_generic_generic_drainage.csv",
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_drainage.csv"),
          overwrite = TRUE)
        
        file.copy(
          # existing innovation yield to be duplicated----
          "E:/repos/raise_fs/shiny/data/generic_generic_generic_yield.csv",
          # new innovation
          paste0("E:/repos/raise_fs/shiny/data/", 
                 shared_parameters$crop_name_1,
                 "_",
                 shared_parameters$ideotype_1,
                 "_", 
                 shared_parameters$scenario_1,
                 "_yield.csv"),
          overwrite = TRUE)
        
        switch_screen("screen5")
      }
      
      # shared_values$crop_name_1 <-  selected_row()$crop_name
      # shared_values$ideotype_1 <-  selected_row()$ideotype
      # shared_values$scenario_1 <-  selected_row()$scenario
      switch_screen("screen5")
      
    })
    
  })
}
