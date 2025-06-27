bslib_screen6_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  
  tagList(
    h3("Rule Base Conclusions:"),
    
    # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen5"), "Back to Screen 5"),
    actionButton(ns("to_screen7"), "Go to Screen 7")
    
  )
}

bslib_screen6_module_v3_MainUI <- function(id) {
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
    DTOutput(ns("conclusions_data_table")),
    uiOutput(ns("dyanamic_save_reset"))
    
  )
}

bslib_screen6_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Allowed values ----
    allowed_values <- c("suboptimal", "optimal", "good", "moderate", "poor", "high", "low")
    
    # Load the initial data ----
    initial_data <- reactive({
      
      message(paste("S6. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_values$crop_name_1,
        "_",
        shared_values$ideotype_1,
        "_",
        shared_values$scenario_1,
        "_saved_tree_network.csv"
      ))) {
      
      message(
        paste(
          "S6. Initiation. inn details1:",
          shared_values$crop_name_1,
          "-",
          shared_values$ideotype_1,
          "-",
          shared_values$scenario_1
        )
      )
      df_inn_tree_net <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_saved_tree_network.csv"
        )
      )
      
      df_inn_tree_net_stack <- dplyr::select(df_inn_tree_net, "stack_code") |> distinct()
      print(str(df_inn_tree_net_stack))
      print(df_inn_tree_net_stack)
      
      df_inn_requirements <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_requirements.csv"
        )
      )
      
      print(str(df_inn_requirements))
      
      # add new rows to requirements based on tree network ----
      
      df_used_codes_req <- df_inn_requirements |> dplyr::select("crit_code") |> dplyr::distinct()
      used_codes_req <- df_used_codes_req[["crit_code"]]
      df_used_codes_tree <- df_inn_tree_net |> dplyr::select("stack_code", "stack") |> dplyr::distinct()
      used_codes_tree <- df_used_codes_tree[["stack_code"]]
      
      # Find codes in df_used_codes_tree that are not in df_used_codes_req
      new_codes <- setdiff(used_codes_tree, used_codes_req)
      
      # if there are new codes ----
      
      if (length(new_codes) > 0) {
        df_new_codes <- data.frame(crit_code = new_codes)
        df_new_codes_stackname <- left_join(df_new_codes,
                                            df_used_codes_tree,
                                            join_by(crit_code == stack_code))
        message("S6. df_new_codes_stackname")
        print(df_new_codes_stackname)
        
        v_req_names <- c(
          "weight",
          "threshold_1",
          "threshold_2",
          "width_1",
          "width_2",
          "thresh_source",
          "data_desc",
          "data_file_prefix",
          "raster_or_brick",
          "agg_fun",
          "rsm_fun",
          "prop_level_1",
          "prop_level_2",
          "prop_level_3",
          "conc_level_1" ,
          "conc_level_2",
          "conc_level_3",
          "fuzzy_partition",
          "yield" ,
          "phen_stage",
          "temp_resolution",
          "prec_temp",
          "texture"
        )
        
        df_new_codes_stackname[, v_req_names] <- NA
        message("S6. df_new_codes_stackname")
        print(df_new_codes_stackname)
        
        #df_new_codes_stackname <- mutate(df_new_codes_stackname, criterion == stack)
        df_new_codes_stackname <- df_new_codes_stackname |> rename(criterion = stack)
        message("S6. df_new_codes_stackname")
        print(df_new_codes_stackname)
        
        # Append new rows to df_inn_requirements
        df_inn_requirements_updated <- rbind(df_inn_requirements, df_new_codes_stackname)
      } else {
        # if there are no new codes ----
        df_inn_requirements_updated <- df_inn_requirements
      }
      
      df_inn_conc <- dplyr::left_join(
        df_inn_tree_net_stack,
        df_inn_requirements_updated,
        join_by(stack_code == crit_code),
        keep = T
      ) |> dplyr::select("crit_code",
                         "criterion",
                         "conc_level_1",
                         "conc_level_2",
                         "conc_level_3")
      
      message("S6. str(df_inn_conc)")
      print(str(df_inn_conc))
      message("S6. df_inn_conc")
      print(df_inn_conc)
      
      df_inn_conc |> as.data.table()
      
      } else {
        NULL
      }
      
    })
    
    # Store the table reactively ----
    table_data  <- reactiveVal()
    #req(initial_data())  # only proceed if non-NULL
    current_data <- reactive({
      table_data()
    })
    
    observeEvent(initial_data(), {
      req(initial_data())  # only proceed if non-NULL
      table_data(initial_data())
    })
    
    # Render the editable data table----
    output$conclusions_data_table <- DT::renderDT({
      if (shared_values$inn_type_1 == "existing") {
        DT::datatable(
          current_data(),
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(lengthMenu = c(10, 20, 50), pageLength = 20))
          } else {
            
            DT::datatable(
              current_data(),
              rownames = F,
              filter = "bottom",
              selection = list(mode = "single"),
              editable = list(target = "cell", disable = list(columns = c(0, 1))),
              # only edit the conclusions
              options = list( lengthMenu = c(10, 20, 50), pageLength = 20))
      }
    })
    
    observeEvent(input$conclusions_data_table_cell_edit, {
      message("S6. str(input$editable_table_cell_edit)")
      str(input$editable_table_cell_edit)
    })
    
    
    #observeEvent Update table on cell edit----
    observeEvent(input$conclusions_data_table_cell_edit, {
      info <- input$conclusions_data_table_cell_edit
      
      message("S6. str(input$editable_table_cell_edit)")
      str(input$editable_table_cell_edit)
      
      dt <- copy(current_data())
      message("S6. 1 str(dt)")
      str(dt)
      
      
      colname <- names(dt)[ info$col + 1 ]  # adjust for 0-based to 1-based
      message("S6. colname")
      print(colname)
      
      
      old_class <- class(dt[[colname]])
      message("S6. old_class")
      print(old_class)
      
      coerced_val <- tryCatch({
        as(info$value, old_class)
      }, error = function(e) info$value)
      
      dt[info$row, (colname) := coerced_val]
      message("S6. 2 str(dt)")
      str(dt)
      
      table_data(dt)
    })
    
    
    # table controls
    output$dyanamic_save_reset <- renderUI({
          tagList(
          actionButton(ns("save_btn"), "Save table"),
          actionButton(ns("reset_btn"), "Reset table")
        )
    })
    
    
    # handle save----
    observeEvent(input$save_btn, {
      req(current_data())
      dt <- table_data()
      
      # validate NAs 
      if (anyNA(dt$conc_level_1) || anyNA(dt$conc_level_2)) {
        showModal(
          modalDialog(
            title = "Missing values detected",
            "Please complete all values in conc_level_1 & conc_level_2 before saving.",
            easyClose = TRUE
          )
        )
        return()
      } 
      
      if (!all(dt$conc_level_1 %in% allowed_values) || !all(dt$conc_level_2 %in% allowed_values)) {
        #bad_values <- unique(dt$special_column[!(dt$special_column %in% allowed_values)])
        showModal(modalDialog(
          title = "Invalid values detected",
          paste(
            "Only the following values are  allowed:",
            paste(allowed_values, collapse = ", ")
          ),
          easyClose = TRUE
        ))
        return()
      }
      
      
      # Check for duplicate values within each row
      rows_with_dupes <- which(apply(dt, 1, function(x) any(duplicated(x))))
      
      if (length(rows_with_dupes) > 0) {
        showModal(modalDialog(
          title = "Duplicate values detected within rows",
          paste(
            "The following rows have duplicate values within their columns:",
            paste(rows_with_dupes, collapse = ", ")
          ),
          easyClose = TRUE
        ))
        return()
      }
      
      
      # save logic here
        fwrite(dt, file = paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_saved_conc_table.csv"
        ))
        showModal(modalDialog(
          title = "Success",
          "Table saved successfully.",
          easyClose = TRUE
        ))
      
    })
    
    
    # handle reset----
    observeEvent(input$reset_btn, {
      #req(initial_data())  # only proceed if non-NULL
      table_data(initial_data())
    })
    
    
    
    
    
    
    
    # 
    # 
    # #observeEvent Update table on cell edit----
    # 
    # observeEvent(input$conclusions_data_table_cell_edit, {
    #   
    #   print(str(input$conclusions_data_table_cell_edit))
    #   
    #   message(paste("observeEvent Update table on cell edit"))
    #   info <- input$conclusions_data_table_cell_edit
    #   print(str(info))
    #   dt <- data.table::copy(as.data.table(conc_data()))  # Ensure reactivity
    #   print(str(dt))
    #   message(paste("is.data.table(dt) =", is.data.table(dt)))
    #   
    #   colname <- colnames(dt)[info$col]
    #   message(paste("observeEvent Update table on cell edit - colname", (colname)))
    #   row <- info$row
    #   message(paste("observeEvent Update table on cell edit - row", row))
    #   val <- info$value
    #   message(paste("observeEvent Update table on cell edit - val", val))
    #   
    # 
    #   dt[row, "conc_level_1" := val]
    #   conc_data(as.data.frame(dt))
    #   print(conc_data())
    #   
    # })
    
#     # render the buttons UI output ----
#     output$dyanamic_save_reset <- renderUI({
#       
#       dt <- as.data.table(conc_data())
#       message(paste("render the buttons UI output"))
#       req(dt)
#       
#       
#       # observe whether conclusions are filled to determine button visibility----
#       target_column <- "conc_level_1"
#       
#       message(paste("dt[[target_column]]"))
#       print(dt[[target_column]])
#       
#       
# #      if (any(is.na(dt[[target_column]])) ||
# #          any(dt[[target_column]] == "")) {
# #        return(NULL)  # Don't render button if any NAs
# #      }
#       
#       tagList(
#         actionButton(ns("save_dyn_button"), "Save Changes / Re-load"),
#         # Button to save changes
#         actionButton(ns("reset_dyn_button"), "Reset to Original")
#       ) # Button to reset to original
#       
#     })
#     
#     # Save action
#     observeEvent(input$save_dyn_button, {
#       showModal(modalDialog("Data saved successfully!", easyClose = TRUE))
#       # Add your save logic here
#     })
    
    
    
    # _----
    
    # outputs from previous screens----
    
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
      message(paste("S6. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S6. You selected crop on Screen 4:", shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S6. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S6. You selected ideotype on Screen 4:", shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S6. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S6. You selected scenario on Screen 4:", shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S6. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S6. You selected Innovation type on Screen 4:", shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    #2 observeEvent back_to_screen5 ----
    observeEvent(input$back_to_screen5, {
      switch_screen("screen5")
    })
    
    #2 observeEvent to_screen7 ----
    observeEvent(input$to_screen7, {
      switch_screen("screen7")
      
    })
    
  })
}