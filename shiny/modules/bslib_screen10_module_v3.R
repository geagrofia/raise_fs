# modules/screen10.R

library(DT)


bslib_screen10_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(
    #     ns("back_to_screen9"),
    #     label = tagList(
    #       icon("circle-left"),
    #       # icon first
    #       "Back to Screen 9"
    #       # text second
    #     ),
    #     class = "btn-primary"
    #   ),
    #   actionButton(
    #     ns("to_screen11"),
    #     label = tagList(
    #       "Go to Screen 11",
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
          ns("show_help_10_01"),
          title = "Help for Step 10",
          label = tagList(
            icon("circle-question")  # icon second)
          ),
          style = "background: rgba(23, 162, 184, 0.5);"
        )
        
      ),
      div(
        style = "display: inline-block; vertical-align: middle; margin-left: 10px;",
      h4("Step 10: View or Edit Soil Texture and Drainage Tables")
      ),    
      # soil texture table
      h4("Soil Texture Table"),
      scrollable_DT(ns("soil_texture_table")),
      uiOutput(ns("dyanamic_save_reset_texture"))
    ),
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      # soil drainage table
      h4("Soil Drainage Table"),
      scrollable_DT(ns("soil_drainage_table")),
      uiOutput(ns("dyanamic_save_reset_drainage"))
    )
  )
}

bslib_screen10_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen9"), 
                   title = "Go back to Step 9: View or Edit Crop Growth Stages",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 10", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen11"), 
                   title = "Go to Step 11: View or Edit Yield Table",
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

bslib_screen10_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    save_st <- reactiveVal(F)
    save_sd <- reactiveVal(F)
    disable("to_screen11")
    
    
    # Load the initial soil texture data ----
    initial_soil_texture_data <- reactive({
      req(switch_screen() == "screen10")
      if (file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_texture.csv"
        )
      )) {
        df_texture <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_texture.csv"
          )
        )
        message("S10. 2 str(df_texture)")
        str(df_texture)
        
        # convert your data.frame to data.table
        dt_texture <- as.data.table(df_texture)
        
        dt_texture
      } else {
        NULL
      }
    })
    
    # Load the initial soil drainage data ----
    initial_soil_drainage_data <- reactive({
      req(switch_screen() == "screen10")
      if (file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_drainage.csv"
        )
      )) {
        df_drainage <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_drainage.csv"
          )
        )
        message("S10. 2 str(df_drainage)")
        str(df_drainage)
        
        # convert your data.frame to data.table
        dt_drainage <- as.data.table(df_drainage)
        
        dt_drainage
      } else {
        NULL
      }
    })
    
    # Store the tables reactively ----
    soil_texture_data_table_data  <- reactiveVal()
    soil_drainage_data_table_data  <- reactiveVal()
    
    
    observeEvent(initial_soil_texture_data(), {
      req(initial_soil_texture_data())  # only proceed if non-NULL
      soil_texture_data_table_data(initial_soil_texture_data())
    })
    
    observeEvent(initial_soil_drainage_data(), {
      req(initial_soil_drainage_data())  # only proceed if non-NULL
      soil_drainage_data_table_data(initial_soil_drainage_data())
    })
    
    
    # Current tables data ----
    current_soil_texture_data <- reactive({
      soil_texture_data_table_data()
    })
    current_soil_drainage_data <- reactive({
      soil_drainage_data_table_data()
    })
    
    
    # Render the soil_texture_table----
    output$soil_texture_table <- DT::renderDT({
      message("S10. renderDT soil_texture_table")
      print(current_soil_texture_data())
      
      if (shared_values$inn_type_1 == "existing") {
        DT::datatable(
          current_soil_texture_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(
            scrollX = TRUE,
            lengthMenu = c(10, 20),
            pageLength = 15,
            sDom  = '<"top">rt<"bottom">ip'
          )
        )
        
      } else {
        DT::datatable(
          current_soil_texture_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "single"),
          editable = list(target = "cell", disable = list(columns = c(0:2))),
          options = list(
            scrollX = TRUE,
            lengthMenu = c(10, 20),
            pageLength = 15,
            sDom  = '<"top">rt<"bottom">ip'
          )
        )
      }
    })
    
    # Render the soil_drainage_table----
    output$soil_drainage_table <- DT::renderDT({
      message("S10. renderDT soil_drainage_table")
      print(current_soil_drainage_data())
      
      if (shared_values$inn_type_1 == "existing") {
        DT::datatable(
          current_soil_drainage_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(
            scrollX = TRUE,
            lengthMenu = c(10, 20),
            pageLength = 15,
            sDom  = '<"top">t<"bottom">ip'
          )
        )
        
      } else {
        DT::datatable(
          current_soil_drainage_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "single"),
          editable = list(target = "cell", disable = list(columns = c(0))),
          options = list(
            scrollX = TRUE,
            lengthMenu = c(10, 20),
            pageLength = 15,
            sDom  = '<"top">t<"bottom">ip'
          )
        )
      }
    })
    
    
    # _----
    
    # observe parameters ----
   
    # _----
    
    
    #observeEvent Update soil_texture table on cell edit----
    observeEvent(input$soil_texture_table_cell_edit, {
      message("S10. str(input$soil_texture_table_cell_edit)")
      str(input$soil_texture_table_cell_edit)
      
      info_st <- input$soil_texture_table_cell_edit
      
      dt_st <- copy(current_soil_texture_data())
      
      colname_st <- names(dt_st)[info_st$col + 1]  # adjust for 0-based to 1-based
      
      old_class_st <- class(dt_st[[colname_st]])
      
      coerced_val_st <- tryCatch({
        as(info_st$value, old_class_st)
      }, error = function(e)
        info_st$value)
      
      dt_st[info_st$row, (colname_st) := coerced_val_st]
      
      soil_texture_data_table_data(dt_st)
      
      
    })
    
    #observeEvent Update soil_drainage_ table on cell edit----
    observeEvent(input$soil_drainage_table_cell_edit, {
      message("S10. str(input$soil_drainage_table_cell_edit)")
      str(input$soil_drainage_table_cell_edit)
      
      info_sd <- input$soil_drainage_table_cell_edit
      
      dt_sd <- copy(current_soil_drainage_data())
      
      colname_sd <- names(dt_sd)[info_sd$col + 1]  # adjust for 0-based to 1-based
      
      old_class_sd <- class(dt_sd[[colname_sd]])
      
      coerced_val_sd <- tryCatch({
        as(info_sd$value, old_class_sd)
      }, error = function(e)
        info_sd$value)
      
      if (!is.na(coerced_val_sd) && coerced_val_sd <= 1)  {
        dt_sd[info_sd$row, (colname_sd) := coerced_val_sd]
        
        soil_drainage_data_table_data(dt_sd)
      } else {
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste("IRM Values must have a value, and not be greater than 1"),
          easyClose = TRUE
        ))
        
      }
      
    })
    
        # _----
    
    # dynamic st save reset controls ----
    output$dyanamic_save_reset_texture <- renderUI({
      tagList(
        actionButton(ns("save_btn_st"), "Save Soil Texture table", class = "btn-primary"),
        actionButton(ns("reset_btn_st"), "Reset Soil Texture table", class = "btn-primary")
      )
    })
    
    # observeEvent st save button----
    observeEvent(input$save_btn_st, {
      req(current_soil_texture_data())
      dt_st <- soil_texture_data_table_data()
      
      message("S10. current_soil_texture_data()")
      print(current_soil_texture_data())
      
      message("S10. soil_texture_data_table_data()")
      print(soil_texture_data_table_data())
      
      message("S10. 2 str(dt_st)")
      str(dt_st)
      
      problems_st_s10 <- c()
      message("S10. problems_st_s10")
      print(problems_st_s10)
      
      # problems_st_s10 that must be flagged:

      # (1) IRM value must be above 0
      if (any(dt_st[[5]] < 0, na.rm = TRUE)) {
        message("S10. problems_st_s10 IRM value must be above 0")
        problems_st_s10 <- c(problems_st_s10, paste("IRM Value is below 0"))
      }
      
      message("S10. problems_st_s10")
      print(problems_st_s10)
      
      # (2) IRM value must be below 1
      if (any(dt_st[[5]] > 1, na.rm = TRUE)) {
        message("S10. problems_st_s10 IRM value must be below 1")
        problems_st_s10 <- c(problems_st_s10, paste("IRM Value is above 1"))
      }
      
      message("S10. problems_st_s10")
      print(problems_st_s10)
      
      if (length(problems_st_s10) > 0) {
        message("S10. (length(problems_st_s10) > 0)")
        print(problems_st_s10)
        
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste(problems_st_s10, collapse = "<br>"),
          easyClose = TRUE
        ))
        
      } else {
        message("S10. save logic")
        
        # overwrite and produce a new version of the st table
        
        fwrite(
          dt_st,
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_texture_s10.csv"
          )
        )
        
        save_st(T)
        message("S10. save_st")
        print(save_st())
        message("S10. save_sd")
        print(save_sd())
        message("S10. save_st & save_sd")
        print((save_st() & save_sd()))
        
        if (save_st() & save_sd()){
          enable("to_screen11")
        }
        
        removeModal()
        showModal(
          modalDialog(
            title = "Saved",
            "Soil Texture table saved successfully.",
            easyClose = TRUE
          )
        )
      }
    })
    
    
    # observeEvent st reset button----
    observeEvent(input$reset_btn_st, {
      req(initial_soil_texture_data())  # only proceed if non-NULL
      soil_texture_data_table_data(initial_soil_texture_data())
    })
    
    
    # dynamic sd save reset controls ----
    output$dyanamic_save_reset_drainage <- renderUI({
      tagList(
        actionButton(ns("save_btn_sd"), "Save Soil Drainage table", class = "btn-primary"),
        actionButton(ns("reset_btn_sd"), "Reset Soil Drainage table", class = "btn-primary")
      )
    })
    
    # observeEvent sd save button----
    observeEvent(input$save_btn_sd, {
      req(current_soil_drainage_data())
      dt_sd <- soil_drainage_data_table_data()
      
      message("S10. current_soil_drainage_data()")
      print(current_soil_drainage_data())
      
      message("S10. soil_drainage_data_table_data()")
      print(soil_drainage_data_table_data())
      
      message("S10. 2 str(dt_sd)")
      str(dt_sd)
      
      problems_sd_s10 <- c()
      message("S10. problems_sd_s10")
      print(problems_sd_s10)
      
      # problems_st_s10 that must be flagged:
      
      
      # (1) missing sd IRM code
      if (any(is.na(dt_sd[[2]]) | dt_sd[[2]] == "")) {
        problems_sd_s10 <- c(problems_sd_s10, paste("IRM Value has missing values"))
      }
      
      # (2) IRM value must be above 0
      if (any(dt_sd[[2]] < 0, na.rm = TRUE)) {
        problems_sd_s10 <- c(problems_sd_s10, paste("IRM Value is below 0"))
      }
      
      # (3) IRM value must be above 0 and 1
      if (any(dt_sd[[2]] > 1, na.rm = TRUE)) {
        problems_sd_s10 <- c(problems_sd_s10, paste("IRM Value is above 1"))
      }
      
      message("S10. problems_sd_s10")
      print(problems_sd_s10)
      
      if (length(problems_sd_s10) > 0) {
        message("S10. (length(problems_sd_s10) > 0)")
        print(problems_sd_s10)
        
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste(problems_sd_s10, collapse = "<br>"),
          easyClose = TRUE
        ))
        
      } else {
        message("S10. save logic")
        
        # overwrite and produce a new version of the sd table
        
        fwrite(
          dt_sd,
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_drainage_s10.csv"
          )
        )
        
        save_sd(T)
        message("S10. save_st")
        print(save_st())
        message("S10. save_sd")
        print(save_sd())
        message("S10. save_st & save_sd")
        print((save_st() & save_sd()))
        
        if (save_st() & save_sd()) {
          enable("to_screen11")
        }
        
        removeModal()
        showModal(
          modalDialog(
            title = "Saved",
            "Soil Drainage table saved successfully.",
            easyClose = TRUE
          )
        )
      }
      


    })
    
    
    # observeEvent sd reset button----
    observeEvent(input$reset_btn_sd, {
      req(initial_soil_drainage_data())  # only proceed if non-NULL
      soil_drainage_data_table_data(initial_soil_drainage_data())
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
    
    # observeEvent back_to_screen9 ----
    observeEvent(input$back_to_screen9, {
      switch_screen("screen9")
    })
    
    # observeEvent to_screen11 ----
    observeEvent(input$to_screen11, {
      shared_values$step <- 11
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      switch_screen("screen11")
      
    })
    
    
    # help button 10_01----
    observeEvent(input$show_help_10_01, {
      showModal(modalDialog(
        title = "Step 10: View or Edit Soil Texture and Drainage Tables",
        includeMarkdown("docs/step_10_01.md"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
  }) # Module server
} # Server


