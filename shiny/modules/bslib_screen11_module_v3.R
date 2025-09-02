# modules/screen11.R

library(DT)


bslib_screen11_module_v3_SidebarUI <- function(id, shared_values) {
  ns <- NS(id)
  
  tagList(
    h3("Yield:"),
    # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen10"), "Back to Screen 10"),
    actionButton(ns("to_screen12"), "Go to Screen 12")
  )
}

bslib_screen11_module_v3_MainUI <- function(id) {
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
    
    # yield table
    h4("Yield Table"),
    DTOutput(ns("yield_table")),
    uiOutput(ns("dyanamic_save_reset_yield"))
  )
}

bslib_screen11_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load the initial yield data ----
    initial_yield_data <- reactive({
      if (file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_yield.csv"
        )
      )) {
        df_yield <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
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
      tagList(actionButton(ns("save_btn_yd"), "Save Yield table"),
              actionButton(ns("reset_btn_yd"), "Reset Yield table"))
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
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_yield_s11.csv"
          )
        )
        
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
      message(paste("S11. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S11. You selected crop on Screen 4:",
            shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S11. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S11. You selected ideotype on Screen 4:",
            shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S11. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S11. You selected scenario on Screen 4:",
            shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S11. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S11. You selected Innovation type on Screen 4:",
            shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    # observeEvent back_to_screen10 ----
    observeEvent(input$back_to_screen10, {
      switch_screen("screen10")
    })
    
    # observeEvent to_screen12 ----
    observeEvent(input$to_screen12, {
      switch_screen("screen12")
      
    })
    
  }) # Module server
} # Server


