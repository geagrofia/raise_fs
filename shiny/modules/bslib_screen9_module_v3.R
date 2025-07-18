# modules/screen9.R

library(DT)


bslib_screen9_module_v3_SidebarUI <- function(id, shared_values) {
  ns <- NS(id)
  
  tagList(
    h3("Growth Stages, Soil and Yield:"),
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
    uiOutput(ns("dyanamic_save_reset_gs")),
    
    # soil texture table
    h4("Soil Texture Table"),
    DTOutput(ns("soil_texture_table")),
    uiOutput(ns("dyanamic_save_reset_texture")),
    
    # soil drainage table
    h4("Soil Drainage Table"),
    DTOutput(ns("soil_drainage_table")),
    uiOutput(ns("dyanamic_save_reset_drainage")),
    
    # yield table
    h4("Yield Table"),
    DTOutput(ns("yield_table")),
    uiOutput(ns("dyanamic_save_reset_yield"))
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
        message("S9. 2 str(df_texture)")
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
        message("S9. 2 str(df_drainage)")
        str(df_drainage)
        
        # convert your data.frame to data.table
        dt_drainage <- as.data.table(df_drainage)
        
        dt_drainage
      } else {
        NULL
      }
    })
    
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
        message("S9. 2 str(df_yield)")
        str(df_yield)
        
        # convert your data.frame to data.table
        dt_yield <- as.data.table(df_yield)
        
        dt_yield
      } else {
        NULL
      }
    })
    
    # Store the tables reactively ----
    growth_stages_data_table_data  <- reactiveVal()
    soil_texture_data_table_data  <- reactiveVal()
    soil_drainage_data_table_data  <- reactiveVal()
    yield_data_table_data  <- reactiveVal()
    
    observeEvent(initial_growth_stages_data(), {
      req(initial_growth_stages_data())  # only proceed if non-NULL
      growth_stages_data_table_data(initial_growth_stages_data())
    })
    
    observeEvent(initial_soil_texture_data(), {
      req(initial_soil_texture_data())  # only proceed if non-NULL
      soil_texture_data_table_data(initial_soil_texture_data())
    })
    
    observeEvent(initial_soil_drainage_data(), {
      req(initial_soil_drainage_data())  # only proceed if non-NULL
      soil_drainage_data_table_data(initial_soil_drainage_data())
    })
    
    observeEvent(initial_yield_data(), {
      req(initial_yield_data())  # only proceed if non-NULL
      yield_data_table_data(initial_yield_data())
    })
    
    # Current tables data ----
    current_growth_stages_data <- reactive({
      growth_stages_data_table_data()
    })
    current_soil_texture_data <- reactive({
      soil_texture_data_table_data()
    })
    current_soil_drainage_data <- reactive({
      soil_drainage_data_table_data()
    })
    current_yield_data <- reactive({
      yield_data_table_data()
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
    
    # Render the soil_texture_table----
    output$soil_texture_table <- DT::renderDT({
      message("S9. renderDT soil_texture_table")
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
      message("S9. renderDT soil_drainage_table")
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
    
    # Render the yield_table----
    output$yield_table <- DT::renderDT({
      message("S9. renderDT yield_table")
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
    
    #observeEvent Update soil_texture table on cell edit----
    observeEvent(input$soil_texture_table_cell_edit, {
      message("S9. str(input$soil_texture_table_cell_edit)")
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
      message("S9. str(input$soil_drainage_table_cell_edit)")
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
    
    
    #observeEvent Update yield table on cell edit----
    observeEvent(input$yield_table_cell_edit, {
      message("S9. str(input$yield_table_cell_edit)")
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
      
      message("S9. current_soil_texture_data()")
      print(current_soil_texture_data())
      
      message("S9. soil_texture_data_table_data()")
      print(soil_texture_data_table_data())
      
      message("S9. 2 str(dt_st)")
      str(dt_st)
      
      problems_st_s9 <- c()
      message("S9. problems_st_s9")
      print(problems_st_s9)
      
      # problems_st_s9 that must be flagged:

      # (1) IRM value must be above 0
      if (any(dt_st[[5]] < 0, na.rm = TRUE)) {
        message("S9. problems_st_s9 IRM value must be above 0")
        problems_st_s9 <- c(problems_st_s9, paste("IRM Value is below 0"))
      }
      
      message("S9. problems_st_s9")
      print(problems_st_s9)
      
      # (2) IRM value must be below 1
      if (any(dt_st[[5]] > 1, na.rm = TRUE)) {
        message("S9. problems_st_s9 IRM value must be below 1")
        problems_st_s9 <- c(problems_st_s9, paste("IRM Value is above 1"))
      }
      
      message("S9. problems_st_s9")
      print(problems_st_s9)
      
      if (length(problems_st_s9) > 0) {
        message("S9. (length(problems_st_s9) > 0)")
        print(problems_st_s9)
        
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste(problems_st_s9, collapse = "<br>"),
          easyClose = TRUE
        ))
        
      } else {
        message("S9. save logic")
        
        # overwrite and produce a new version of the gs table
        
        fwrite(
          dt_st,
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_texture_s9.csv"
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
      
      message("S9. current_soil_drainage_data()")
      print(current_soil_drainage_data())
      
      message("S9. soil_drainage_data_table_data()")
      print(soil_drainage_data_table_data())
      
      message("S9. 2 str(dt_sd)")
      str(dt_sd)
      
      problems_sd_s9 <- c()
      message("S9. problems_sd_s9")
      print(problems_sd_s9)
      
      # problems_st_s9 that must be flagged:
      
      
      # (1) missing sd IRM code
      if (any(is.na(dt_sd[[2]]) | dt_sd[[2]] == "")) {
        problems_sd_s9 <- c(problems_sd_s9, paste("IRM Value has missing values"))
      }
      
      # (2) IRM value must be above 0
      if (any(dt_sd[[2]] < 0, na.rm = TRUE)) {
        problems_sd_s9 <- c(problems_sd_s9, paste("IRM Value is below 0"))
      }
      
      # (3) IRM value must be above 0 and 1
      if (any(dt_sd[[2]] > 1, na.rm = TRUE)) {
        problems_sd_s9 <- c(problems_sd_s9, paste("IRM Value is above 1"))
      }
      
      message("S9. problems_sd_s9")
      print(problems_sd_s9)
      
      if (length(problems_sd_s9) > 0) {
        message("S9. (length(problems_sd_s9) > 0)")
        print(problems_sd_s9)
        
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste(problems_sd_s9, collapse = "<br>"),
          easyClose = TRUE
        ))
        
      } else {
        message("S9. save logic")
        
        # overwrite and produce a new version of the gs table
        
        fwrite(
          dt_sd,
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_drainage_s9.csv"
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
    
    
    
    # dynamic yd save reset controls ----
    output$dyanamic_save_reset_yield <- renderUI({
      tagList(actionButton(ns("save_btn_yd"), "Save Yield table"),
              actionButton(ns("reset_btn_yd"), "Reset Yield table"))
    })
    
    # observeEvent yd save button----
    observeEvent(input$save_btn_yd, {
      req(current_yield_data())
      dt_yd <- yield_data_table_data()
      
      message("S9. current_yield_data()")
      print(current_yield_data())
      
      message("S9. yield_data_table_data()")
      print(yield_data_table_data())
      
      message("S9. 2 str(dt_yd)")
      str(dt_yd)
      
      problems_yd_s9 <- c()
      message("S9. problems_yd_s9")
      print(problems_yd_s9)
      
      # problems_yd_s9 that must be flagged:
      
      
      # (1) missing YP value
      if (any(is.na(dt_yd[[1]]) | dt_yd[[1]] == "")) {
        problems_yd_s9 <- c(problems_yd_s9, paste("YP has missing values"))
      }
      
      # (1) missing YW value
      if (any(is.na(dt_yd[[2]]) | dt_yd[[2]] == "")) {
        problems_yd_s9 <- c(problems_yd_s9, paste("YW has missing values"))
      }
      
      message("S9. problems_yd_s9")
      print(problems_yd_s9)
      
      if (length(problems_yd_s9) > 0) {
        message("S9. (length(problems_yd_s9) > 0)")
        print(problems_yd_s9)
        
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste(problems_yd_s9, collapse = "<br>"),
          easyClose = TRUE
        ))
        
      } else {
        message("S9. save logic")
        
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
            "_yield_s9.csv"
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


