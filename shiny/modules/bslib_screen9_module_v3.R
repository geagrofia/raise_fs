# modules/screen9.R

library(DT)


bslib_screen9_module_v3_SidebarUI <- function(id, shared_values) {
  ns <- NS(id)
  
  tagList(
    h3("Crop Growth Stages:"),
    checkboxInput(ns("sos1"), "Spatially Dynamic Growing Season Map", value = FALSE),
    numericInput(ns("sowdate1"), "Sowing Day Number (1-365)", value = 190),
    # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen8"), "Back to Screen 8"),
    actionButton(ns("to_screen10"), "Go to Screen 10")
  )
}

bslib_screen9_module_v3_MainUI <- function(id) {
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
    
    # growth stages table
    h4("Growth Stages Table"),
    DTOutput(ns("growth_stages_table")),
    actionButton(ns("add_btn"), "Add Growth Stage"),
    actionButton(ns("delete_btn"), "Delete Growth Stage"),
    actionButton(ns("move_up_btn"), "Move Up Growth Stage"),
    actionButton(ns("move_down_btn"), "Move Down Growth Stage"),
    uiOutput(ns("dyanamic_save_reset_gs"))
  )
}

bslib_screen9_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load the initial growth stages data ----
    initial_growth_stages_data <- reactive({
      if (file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_gs.csv"
        )
      )) {
        df_gs <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_gs.csv"
          )
        )
        
        message("S9. 2 str(df_gs)")
        str(df_gs)
        
        # convert your data.frame to data.table
        dt_gs <- as.data.table(df_gs)
        
        dt_gs
      } else {
        NULL
      }
    })
    
    
    # Store the tables reactively ----
    growth_stages_data_table_data  <- reactiveVal()
    
    observeEvent(initial_growth_stages_data(), {
      req(initial_growth_stages_data())  # only proceed if non-NULL
      growth_stages_data_table_data(initial_growth_stages_data())
    })
    
    
    # Current tables data ----
    current_growth_stages_data <- reactive({
      growth_stages_data_table_data()
    })
    
    
    # Render the growth_stages_table----
    output$growth_stages_table <- DT::renderDT({
      message("S9. renderDT growth_stages_table")
      print(current_growth_stages_data())
      
      if (shared_values$inn_type_1 == "existing") {
        DT::datatable(
          current_growth_stages_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(
            lengthMenu = c(10, 20),
            pageLength = 10,
            sDom  = '<"top">lrt<"bottom">ip'
          )
        )
        
      } else {
        DT::datatable(
          current_growth_stages_data(),
          rownames = F,
          filter = "none",
          selection = list(mode = "single"),
          editable = list(target = "cell"),
          options = list(
            lengthMenu = c(10, 20),
            pageLength = 10,
            sDom  = '<"top">lrt<"bottom">ip'
          )
        )
      }
    })
    
    
    
    # _----
    
    # observe parameters ----
    observe({
      message("S9. observe parameters")
      shared_values$sos1 <- ifelse(input$sos1, 1, 0)
      
      # shared_values$limitsclass1 <- input$limitsclass1
      # shared_values$concclass1 <- input$concclass1
      
      message("S9. observe parameters")
      print(shared_values$sos1)
      
      # print(shared_values$limitsclass1)
      # print(shared_values$concclass1)
    })
    
    observeEvent(input$sos1, {
      message("S9. observeEvent parameters")
      shared_values$sowdate1 <- ifelse(input$sos1, NA, input$sowdate1)
      message("S9. observeEvent parameters")
      print(shared_values$sowdate1)
    })
    
    # _----
    
    #observeEvent add row button growth_stages_table ----
    observeEvent(input$add_btn, {
      new_row <- data.frame(
        gs_name =	"",
        gs_length	= 0,
        gs_source = "",
        stringsAsFactors = FALSE
      ) |> as.data.table()
      
      message("S9. new_row growth_stages_table")
      print(new_row)
      print(str(new_row))
      
      message("S9. growth_stages_data_table_data()")
      print(growth_stages_data_table_data())
      print(str(growth_stages_data_table_data()))
      
      new_data <- rbind(growth_stages_data_table_data(), new_row)
      message("S9. new_data")
      print(new_data)
      print(str(new_data))
      
      growth_stages_data_table_data(new_data)
    })
    
    #observeEvent delete row button growth_stages_table ----
    observeEvent(input$delete_btn, {
      selected <- input$growth_stages_table_rows_selected
      if (length(selected)) {
        new_data <- growth_stages_data_table_data()
        new_data <- new_data[-selected, ]
        growth_stages_data_table_data(new_data)
      }
    })
    
    #observeEvent move selected growth_stages row up ----
    observeEvent(input$move_up_btn, {
      selected <- input$growth_stages_table_rows_selected
      if (length(selected) != 1 || selected == 1) return()
      dt_gs <- growth_stages_data_table_data()
      if (dt_gs$gs_name[selected] == "total") return()  # Do not move total
      temp <- dt_gs[selected, ]
      dt_gs[selected, ] <- dt_gs[selected - 1, ]
      dt_gs[selected - 1, ] <- temp
      growth_stages_data_table_data(dt_gs)
    })
    
    
    #observeEvent move selected growth_stagesrow down ----
    observeEvent(input$move_down_btn, {
      dt_gs <- growth_stages_data_table_data()
      selected <- input$growth_stages_table_rows_selected
      if (length(selected) != 1 || selected >= nrow(dt_gs) - 1) return()
      if (dt_gs$gs_name[selected] == "total") return()  # Do not move total
      temp <- dt_gs[selected, ]
      dt_gs[selected, ] <- dt_gs[selected + 1, ]
      dt_gs[selected + 1, ] <- temp
      growth_stages_data_table_data(dt_gs)
    })
    
    
    #observeEvent Update growth_stages_table on cell edit----
    observeEvent(input$growth_stages_table_cell_edit, {
      info_gs <- input$growth_stages_table_cell_edit
      
      message("S9. info_gs")
      print(info_gs)
      
      i <- info_gs$row
      j <- info_gs$col
      
      dt_gs <- copy(current_growth_stages_data())
      
      colname_gs <- names(dt_gs)[j + 1]  # adjust for 0-based to 1-based
      
           # Prevent editing 'gs_length' of the total row
      if (dt_gs$gs_name[i] == "total" && (colname_gs == "gs_length" | colname_gs == "gs_name")) {
        message("S9. if TRUE")
        showNotification("Cannot edit 'total' row", type = "error")
        return()
      }
      
      old_class_gs <- class(dt_gs[[colname_gs]])
      
      coerced_val_gs <- tryCatch({
        as(info_gs$value, old_class_gs)
      }, error = function(e)
        info_gs$value)
      
      dt_gs[info_gs$row, (colname_gs) := coerced_val_gs]
      
      growth_stages_data_table_data(dt_gs)
      
    })
    
    # _----
    
    # dynamic gs save reset controls ----
    output$dyanamic_save_reset_gs <- renderUI({
      tagList(
        actionButton(ns("save_btn_gs"), "Save Growth Stages table"),
        actionButton(ns("reset_btn_gs"), "Reset Growth Stages table")
      )
    })
    
    # observeEvent gs save button----
    observeEvent(input$save_btn_gs, {
      req(current_growth_stages_data())
      dt_gs <- growth_stages_data_table_data()
      
      message("S9. current_growth_stages_data()")
      print(current_growth_stages_data())
      
      message("S9. growth_stages_data_table_data()")
      print(growth_stages_data_table_data())
      
      message("S9. 2 str(dt_gs)")
      str(dt_gs)
      
      problems_gs_s9 <- c()
      message("S9. problems_gs_s9")
      print(problems_gs_s9)
      
      # problems_gs_s9 that must be flagged:
      
      
      # (1) missing gs name
      # (2) missing gs length
      # (3) missing gs source
      
      mandatory_gs <- c(1:3)
      for (col_gs in mandatory_gs) {
        if (any(is.na(dt_gs[[col_gs]]) | dt_gs[[col_gs]] == "")) {
          problems_gs_s9 <- c(problems_gs_s9,
                              sprintf("Column %d has missing values", col_gs))
        }
      }
      
      message("S9. problems_gs_s9")
      print(problems_gs_s9)
      
      
      # (4) phenological stage must be unique
      
      duplicated_gs <- dt_gs[[1]][duplicated(dt_gs[[1]]) &
                                    dt_gs[[1]] != ""]
      if (length(duplicated_gs)) {
        problems_gs_s9 <- c(problems_gs_s9, paste("Duplicate growth stage name"))
      }
      message("S9. problems_gs_s9")
      print(problems_gs_s9)
      
      if (length(problems_gs_s9) > 0) {
        message("S9. (length(problems_gs_s9) > 0)")
        print(problems_gs_s9)
        
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste(problems_gs_s9, collapse = "<br>"),
          easyClose = TRUE
        ))
        
      } else {
        message("S9. save logic")
        
        # Separate out total row and others
        non_total <- dt_gs[dt_gs$gs_name != "total", ]
        total_row <- dt_gs[dt_gs$gs_name == "total", ]
        
        # Recalculate total value
        total_value <- sum(as.numeric(non_total$gs_length), na.rm = TRUE)
        total_row$gs_length <- total_value
        
        # Rebuild data frame
        dt_gs <- rbind(non_total, total_row)
        growth_stages_data_table_data(dt_gs)
        
        # add sow date to the top row
        dt_gs <- mutate(dt_gs, gs_day = NA)
        dt_sow_date <- data.table(gs_name = c("sow_date"), gs_day = c(ifelse(shared_values$sos1==1, 0, input$sowdate1)), gs_length = NA, gs_source = c("User"))
        dt_gs <- rbind(dt_sow_date, dt_gs)
        
        # overwrite and produce a new version of the gs table
        
        fwrite(
          dt_gs,
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_gs_s9.csv"
          )
        )
        
        removeModal()
        showModal(modalDialog(
          title = "Saved",
          "Table saved successfully.",
          easyClose = TRUE
        ))
      }
    })
    
    
    # observeEvent gs reset button----
    observeEvent(input$reset_btn_gs, {
      req(initial_growth_stages_data())  # only proceed if non-NULL
      growth_stages_data_table_data(initial_growth_stages_data())
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
      message(paste("S9. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S9. You selected crop on Screen 4:",
            shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S9. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S9. You selected ideotype on Screen 4:",
            shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S9. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S9. You selected scenario on Screen 4:",
            shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S9. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S9. You selected Innovation type on Screen 4:",
            shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    # observeEvent back_to_screen98 ----
    observeEvent(input$back_to_screen8, {
      switch_screen("screen8")
    })
    
    # observeEvent to_screen10 ----
    observeEvent(input$to_screen10, {
      switch_screen("screen10")
      
    })
    
  }) # Module server
} # Server


