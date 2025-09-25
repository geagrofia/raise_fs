# modules/screen11.R

library(DT)


bslib_screen11_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(
    #     ns("back_to_screen10"),
    #     label = tagList(
    #       icon("circle-left"),
    #       # icon first
    #       "Back to Screen 10"
    #       # text second
    #     ),
    #     class = "btn-primary"
    #   ),
    #   actionButton(
    #     ns("to_screen12"),
    #     label = tagList(
    #       "Go to Screen 12",
    #       # text first
    #       icon("circle-right")  # icon second)
    #     ),
    #     class = "btn-primary"
    #   )
    # ), 
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      div(
        style = "display:inline-block;vertical-align:middle;margin-bottom: 5px;",
        actionButton(
          ns("show_help_11_01"),
          title = "Help for Step 11",
          label = tagList(
            icon("circle-question")  # icon second)
          ),
          style = "background: rgba(23, 162, 184, 0.5);"
        )
        
      ),
      div(
        style = "display: inline-block; vertical-align: middle; margin-left: 10px;",
      h4("Step 11: View or Edit Yield Table")
      ),
      
      scrollable_DT(ns("yield_table")),
      uiOutput(ns("dyanamic_save_reset_yield"))
    )
  )
}

bslib_screen11_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen10"), 
                   title = "Go back to Step 10: View or Edit Soil Texture and Drainage Tables",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 11", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen12"), 
                   title = "Go to Step 12: View or Edit Spatial Data",
                   label = tagList(
                     #"Go to Screen 2",
                     "Next",
                     # text first
                     icon("circle-right")  # icon second)
                   ),
                   class = "btn-primary disabled")
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
      textOutput(ns("crop_1_display")),
      textOutput(ns("ideotype_1_display")),
      textOutput(ns("scenario_1_display")),
      textOutput(ns("inn_type_1_display"))
    )
  )
}

bslib_screen11_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load the initial yield data ----
    initial_yield_data <- reactive({
      req(switch_screen() == "screen11")
      if (file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_yield.csv"
        )
      )) {
        df_yield <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_yield.csv"
          )
        )
        message("S11. 2 str(df_yield)")
        str(df_yield)
        
        # convert your data.frame to data.table
        dt_yield <- as.data.table(df_yield)
        
        dt_yield
      } else {
        NULL
      }
    })
    
    # Store the tables reactively ----
    yield_data_table_data  <- reactiveVal()
    
    observeEvent(initial_yield_data(), {
      req(initial_yield_data())  # only proceed if non-NULL
      yield_data_table_data(initial_yield_data())
    })
    
    # Current tables data ----
    
    current_yield_data <- reactive({
      yield_data_table_data()
    })
    
    
    # Render the yield_table----
    output$yield_table <- DT::renderDT({
      message("S11. renderDT yield_table")
      print(current_yield_data())
      
      if (shared_values$inn_type_1 == "existing") {
        DT::datatable(
          current_yield_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(
            scrollX = TRUE,
            lengthMenu = c(10, 20),
            pageLength = 10,
            sDom  = '<"top">t'
          )
        )
        
      } else {
        DT::datatable(
          current_yield_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "single"),
          editable = list(target = "cell", disable = list(columns = c(0))),
          options = list(
            scrollX = TRUE,
            lengthMenu = c(10, 20),
            pageLength = 10,
            sDom  = '<"top">t'
          )
        )
      }
    })
    
    # _----
    
    # observe parameters ----
    
    # _----
    
    #observeEvent Update yield table on cell edit----
    observeEvent(input$yield_table_cell_edit, {
      message("S11. str(input$yield_table_cell_edit)")
      str(input$yield_table_cell_edit)
      
      info_yd <- input$yield_table_cell_edit
      
      dt_yd <- copy(current_yield_data())
      
      colname_yd <- names(dt_yd)[info_yd$col + 1]  # adjust for 0-based to 1-based
      
      old_class_yd <- class(dt_yd[[colname_yd]])
      
      coerced_val_yd <- tryCatch({
        as(info_yd$value, old_class_yd)
      }, error = function(e)
        info_yd$value)
      
      dt_yd[info_yd$row, (colname_yd) := coerced_val_yd]
      
      yield_data_table_data(dt_yd)
    })
    
    # _----
    
    # dynamic yd save reset controls ----
    output$dyanamic_save_reset_yield <- renderUI({
      tagList(actionButton(ns("save_btn_yd"), "Save Yield table",
                           class = "btn-primary"),
              actionButton(ns("reset_btn_yd"), "Reset Yield table",
                           class = "btn-primary")
              )
    })
    
    # observeEvent yd save button----
    observeEvent(input$save_btn_yd, {
      req(current_yield_data())
      dt_yd <- yield_data_table_data()
      
      message("S11. current_yield_data()")
      print(current_yield_data())
      
      message("S11. yield_data_table_data()")
      print(yield_data_table_data())
      
      message("S11. 2 str(dt_yd)")
      str(dt_yd)
      
      problems_yd_s11 <- c()
      message("S11. problems_yd_s11")
      print(problems_yd_s11)
      
      # problems_yd_s11 that must be flagged:
      
      
      # (1) missing YP value
      if (any(is.na(dt_yd[[1]]) | dt_yd[[1]] == "")) {
        problems_yd_s11 <- c(problems_yd_s11, paste("YP has missing values"))
      }
      
      # (1) missing YW value
      if (any(is.na(dt_yd[[2]]) | dt_yd[[2]] == "")) {
        problems_yd_s11 <- c(problems_yd_s11, paste("YW has missing values"))
      }
      
      message("S11. problems_yd_s11")
      print(problems_yd_s11)
      
      if (length(problems_yd_s11) > 0) {
        message("S11. (length(problems_yd_s11) > 0)")
        print(problems_yd_s11)
        
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste(problems_yd_s11, collapse = "<br>"),
          easyClose = TRUE
        ))
        
      } else {
        message("S11. save logic")
        
        # overwrite and produce a new version of the gs table
        
        fwrite(
          dt_yd,
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_yield_s11.csv"
          )
        )
        
        enable("to_screen12")
        
        removeModal()
        showModal(
          modalDialog(
            title = "Saved",
            "Yield table saved successfully.",
            easyClose = TRUE
          )
        )
      }
    })
    
    
    # observeEvent yd reset button----
    observeEvent(input$reset_btn_yd, {
      req(initial_yield_data())  # only proceed if non-NULL
      yield_data_table_data(initial_yield_data())
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
    
    output$crop_1_display <- renderText({
      req(shared_parameters$crop_name_1)
      paste("Step 4. Crop =", shared_parameters$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      req(shared_parameters$ideotype_1)
      paste("Step 4. Ideotype =", shared_parameters$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      req(shared_parameters$scenario_1)
      paste("Step 4. Scenario =", shared_parameters$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      req(shared_values$inn_type_1)
      paste("Step 4. Innovation type =", shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    # observeEvent back_to_screen10 ----
    observeEvent(input$back_to_screen10, {
      switch_screen("screen10")
    })
    
    # observeEvent to_screen12 ----
    observeEvent(input$to_screen12, {
      shared_values$step <- 12
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      switch_screen("screen12")
      
    })
    
    # help button 11_01----
    observeEvent(input$show_help_11_01, {
      showModal(modalDialog(
        title = "Step 11: View or Edit Yield Table",
        includeMarkdown("docs/step_11_01.md"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
  }) # Module server
} # Server


