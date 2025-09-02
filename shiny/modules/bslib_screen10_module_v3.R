# modules/screen10.R

library(DT)


bslib_screen10_module_v3_SidebarUI <- function(id, shared_values) {
  ns <- NS(id)
  
  tagList(
    h3("Soil Texture and Drainage:"),
    # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen9"), "Back to Screen 9"),
    actionButton(ns("to_screen11"), "Go to Screen 11")
  )
}

bslib_screen10_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
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
    textOutput(ns("inn_type_1_display")),
    
    # soil texture table
    h4("Soil Texture Table"),
    DTOutput(ns("soil_texture_table")),
    uiOutput(ns("dyanamic_save_reset_texture")),
    
    # soil drainage table
    h4("Soil Drainage Table"),
    DTOutput(ns("soil_drainage_table")),
    uiOutput(ns("dyanamic_save_reset_drainage"))
  )
}

bslib_screen10_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load the initial soil texture data ----
    initial_soil_texture_data <- reactive({
      if (file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_texture.csv"
        )
      )) {
        df_texture <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
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
      if (file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_drainage.csv"
        )
      )) {
        df_drainage <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
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
            lengthMenu = c(10, 20),
            pageLength = 20,
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
            lengthMenu = c(10, 20),
            pageLength = 20,
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
            lengthMenu = c(10, 20),
            pageLength = 10,
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
            lengthMenu = c(10, 20),
            pageLength = 10,
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
        actionButton(ns("save_btn_st"), "Save Soil Texture table"),
        actionButton(ns("reset_btn_st"), "Reset Soil Texture table")
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
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_texture_s10.csv"
          )
        )
        
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
        actionButton(ns("save_btn_sd"), "Save Soil Drainage table"),
        actionButton(ns("reset_btn_sd"), "Reset Soil Drainage table")
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
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_drainage_s10.csv"
          )
        )
        
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
      paste("Number of innovations:", shared_values$num_innovations)
    })
    
    output$innovation_system_display <- renderText({
      if (shared_values$num_innovations == "two_inn") {
        paste("Innovation System:",
              shared_values$innovation_system)
      }
    })
    
    output$spatres_display <- renderText({
      paste("Your spatial resolution is:", shared_values$resolution)
    })
    
    
    output$aggregation_display <- renderText({
      paste("Your aggregation level is:", shared_values$aggregation)
    })
    
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
    
    output$crop_1_display <- renderText({
      message(paste("S10. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S10. You selected crop on Screen 4:",
            shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S10. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S10. You selected ideotype on Screen 4:",
            shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S10. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S10. You selected scenario on Screen 4:",
            shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S10. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S10. You selected Innovation type on Screen 4:",
            shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    # observeEvent back_to_screen9 ----
    observeEvent(input$back_to_screen9, {
      switch_screen("screen9")
    })
    
    # observeEvent to_screen11 ----
    observeEvent(input$to_screen11, {
      switch_screen("screen11")
      
    })
    
  }) # Module server
} # Server


