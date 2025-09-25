bslib_screen6_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(
    #     ns("back_to_screen5"),
    #     label = tagList(
    #       icon("circle-left"),
    #       # icon first
    #       "Back to Screen 5"
    #       # text second
    #     ),
    #     class = "btn-primary"
    #   ),
    #   actionButton(
    #     ns("to_screen7"),
    #     label = tagList(
    #       "Go to Screen 7",
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
          ns("show_help_06_01"),
          title = "Help for Step 6",
          label = tagList(
            icon("circle-question")  # icon second)
          ),
          style = "background: rgba(23, 162, 184, 0.5);"
        )
        
      ),
      div(
        style = "display: inline-block; vertical-align: middle; margin-left: 10px;",
      h4("Step 6: View or Edit Higher-level Rule Base Conclusions")
      ),
      scrollable_DT(ns("conclusions_data_table")),
      
      #_---- test section
      DTOutput(ns("table")),
      #_---- endtest section
      
      
      #DTOutput(ns("conclusions_data_table")),
      actionButton(ns("initial_load"), "Load/Reset Table",
                   class = "btn-primary"),
      uiOutput(ns("dyanamic_save_reset"))
      
    )
  )
}

bslib_screen6_module_v3_MainUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen5"), 
                   title = "Go back to Step 5: View or Edit Rule Base Hierarchy",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 6", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen7"), 
                   title = "Go to Step 7: View or Edit Rule Base Weights",
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

bslib_screen6_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    disable("to_screen7")
    
    # Allowed values ----
    allowed_values <- c("suboptimal",
                        "optimal",
                        "good",
                        "moderate",
                        "poor",
                        "high",
                        "low")
    
    # # Load the initial data----
    # initial_data <- reactive({
    #   
    #   message("S6. initial_data")
    #   
    #   req(file.exists(
    #     paste0(
    #       "E:/repos/raise_fs/shiny/data/",
    #       shared_parameters$crop_name_1,
    #       "_",
    #       shared_parameters$ideotype_1,
    #       "_",
    #       shared_parameters$scenario_1,
    #       #"_saved_tree_network.csv"
    #       "_links_s5.csv"
    #     )
    #   ))  # waits until file is available
    #   
    #   # if (file.exists(
    #   #   paste0(
    #   #     "E:/repos/raise_fs/shiny/data/",
    #   #     shared_parameters$crop_name_1,
    #   #     "_",
    #   #     shared_parameters$ideotype_1,
    #   #     "_",
    #   #     shared_parameters$scenario_1,
    #   #     #"_saved_tree_network.csv"
    #   #     "_links_s5.csv"
    #   #   )
    #   # )) {
    #   #   message(
    #   #   paste(
    #   #     "S6. Initiation. inn details1:",
    #   #     shared_parameters$crop_name_1,
    #   #     "-",
    #   #     shared_parameters$ideotype_1,
    #   #     "-",
    #   #     shared_parameters$scenario_1
    #   #   )
    #   # )
    #   df_inn_tree_net <- read.csv(
    #     paste0(
    #       "E:/repos/raise_fs/shiny/data/",
    #       shared_parameters$crop_name_1,
    #       "_",
    #       shared_parameters$ideotype_1,
    #       "_",
    #       shared_parameters$scenario_1,
    #       #"_saved_tree_network.csv"
    #       "_links_s5.csv"
    #     )
    #   )
    #   
    #   df_inn_tree_net_stack <- dplyr::select(df_inn_tree_net, "stack_code") |> distinct()
    #   print(str(df_inn_tree_net_stack))
    #   message("S6. df_inn_tree_net_stack")
    #   print(df_inn_tree_net_stack)
    #   
    #   df_inn_requirements_s5 <- read.csv(
    #     paste0(
    #       "E:/repos/raise_fs/shiny/data/",
    #       shared_parameters$crop_name_1,
    #       "_",
    #       shared_parameters$ideotype_1,
    #       "_",
    #       shared_parameters$scenario_1,
    #       "_requirements_s5.csv"
    #     )
    #   )
    #   message("S6. df_inn_requirements_s5")
    #   print(str(df_inn_requirements_s5))
    #   
    #   # add new rows to requirements based on tree network ----
    #   
    #   # this has now been done in the previous screen but still needed for Ad
    #   
    #    df_used_codes_req <- df_inn_requirements_s5 |> dplyr::select("crit_code") |> dplyr::distinct()
    #    used_codes_req <- df_used_codes_req[["crit_code"]]
    #    df_used_codes_tree <- df_inn_tree_net |> dplyr::select("stack_code", "stack") |> dplyr::distinct()
    #    used_codes_tree <- df_used_codes_tree[["stack_code"]]
    #    
    #   ## Find codes in df_used_codes_tree that are not in df_used_codes_req
    #    new_codes <- setdiff(used_codes_tree, used_codes_req)
    #    
    #    # if there are new codes ----
    # 
    #   if (length(new_codes) > 0) {
    #     df_new_codes <- data.frame(crit_code = new_codes)
    #     df_new_codes_stackname <- left_join(df_new_codes,
    #                                         df_used_codes_tree,
    #                                         join_by(crit_code == stack_code))
    #     message("S6. df_new_codes_stackname")
    #     print(df_new_codes_stackname)
    # 
    #     v_req_names <- c(
    #       "weight",
    #       "threshold_1",
    #       "threshold_2",
    #       "width_1",
    #       "width_2",
    #       "thresh_source",
    #       "data_desc",
    #       "data_file_prefix",
    #       "raster_or_brick",
    #       "agg_fun",
    #       "rsm_fun",
    #       "prop_level_1",
    #       "prop_level_2",
    #       "prop_level_3",
    #       "conc_level_1" ,
    #       "conc_level_2",
    #       "conc_level_3",
    #       "fuzzy_partition",
    #       "yield" ,
    #       "phen_stage",
    #       "temp_resolution",
    #       "prec_temp",
    #       "texture"
    #     )
    # 
    #     df_new_codes_stackname[, v_req_names] <- NA
    #     message("S6. df_new_codes_stackname")
    #     print(df_new_codes_stackname)
    # 
    #     #df_new_codes_stackname <- mutate(df_new_codes_stackname, criterion == stack)
    #     df_new_codes_stackname <- df_new_codes_stackname |> rename(criterion = stack)
    #     message("S6. df_new_codes_stackname")
    #     print(df_new_codes_stackname)
    # 
    #     # Append new rows to df_inn_requirements_s5
    #     df_inn_requirements_updated <- rbind(df_inn_requirements_s5, df_new_codes_stackname)
    #   } else {
    #     # if there are no new codes ----
    #     df_inn_requirements_updated <- df_inn_requirements_s5
    #   }
    #   
    #   df_inn_conc <- dplyr::left_join(
    #     df_inn_tree_net_stack,
    #     df_inn_requirements_updated,
    #     join_by(stack_code == crit_code),
    #     keep = T
    #   ) |> dplyr::select("crit_code",
    #                      "criterion",
    #                      "conc_level_1",
    #                      "conc_level_2",
    #                      "conc_level_3")
    #   
    #   message("S6. str(df_inn_conc)")
    #   print(str(df_inn_conc))
    #   message("S6. df_inn_conc")
    #   print(df_inn_conc)
    #   
    #   df_inn_conc |> as.data.table()
    #   
    #   # } else {
    #   #   NULL
    #   # }
    #   
    # })
    
    # Store the table reactively ----
    table_data  <- reactiveVal()
    
    
    # Store the table reactively ----
    initial_data  <- reactiveVal()
    
    
    #req(initial_data())  # only proceed if non-NULL
    current_data <- reactive({
      req(table_data())  # only proceed if non-NULL
      table_data()
      message("S6. 2 print(table_data())")
      print(table_data())
    })
    
    # observeEvent(initial_data(), {
    #   req(initial_data())  # only proceed if non-NULL
    #   table_data(initial_data())
    #   message("S6.3 print(table_data())")
    #   print(table_data())
    # })
    
    # Render the editable data table----
    output$conclusions_data_table <- DT::renderDT({
      
      message(paste("S6. Initiation. shared_values$inn_type_1:", shared_values$inn_type_1))
      message(paste("S6. Initiation. num_innovations", shared_parameters$num_innovations))
      message(paste("S6. Initiation. inn details1:", shared_parameters$crop_name_1,"-", shared_parameters$ideotype_1,"-", shared_parameters$scenario_1))
      
      
      if (shared_values$inn_type_1 == "existing") {
        
        message(paste("S6.inn_type_1 == existing"))
        
        DT::datatable(
          current_data(),
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2),
            lengthMenu = c(10, 20, 50),
            pageLength = 10
          )
        )
      } else {
        
        message(paste("S6.inn_type_1 == duplicate or new"))
        print(current_data())
        
        DT::datatable(
          current_data(),
          rownames = F,
          filter = "bottom",
          selection = list(mode = "single"),
          editable = list(target = "cell", disable = list(columns = c(0, 1))),
          # only edit the conclusions
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2),
            lengthMenu = c(10, 20, 50),
            pageLength = 10
          )
        )
      }
    })
    
    # observeEvent(input$conclusions_data_table_cell_edit, {
    #   message("S6. str(input$editable_table_cell_edit)")
    #   str(input$editable_table_cell_edit)
    # })
    
    
    #observeEvent Update table on cell edit----
    observeEvent(input$conclusions_data_table_cell_edit, {
      info <- input$conclusions_data_table_cell_edit
      
      
      message("S6. str(input$editable_table_cell_edit)")
      str(info)
      
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
      
      message("S6. print(info$value)")
      print(info$value)
      message("S6. print(coerced_val)")
      print(coerced_val)
      
      dt[info$row, (colname) := coerced_val]
      message("S6. print(dt)")
      print(dt)
      
      table_data(dt)
    })
    
    
    # table controls
    output$dyanamic_save_reset <- renderUI({
          tagList(
          actionButton(ns("save_btn_6"), "Save Table",
                       class = "btn-primary")
          #,
          #actionButton(ns("reset_btn_6_6"), "Reset table")
        )
    })
    
    
    # observeEvent Save button ----
    observeEvent(input$save_btn_6, {
      req(current_data())
      dt <- table_data()
      message("S6. print(dt)")
      print(dt)
      
      
      # validate NAs 
      if (anyNA(dt$conc_level_1) || anyNA(dt$conc_level_2)) {
        removeModal()
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
        removeModal()
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
        removeModal()
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
      
      # saves the conc_table separately
      
      fwrite(
        dt,
        file = paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_conc_s6.csv"
        )
      )
      
      # overwrite and produce a new version of the requirements table
      
      df_inn_requirements_s5 <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_requirements_s5.csv"
        )
      )
      
      # saves the update requirements table separately      
      #df_inn_requirements_s6 <- merge(dt, df_inn_requirements_s5,  by = "crit_code")  # seems self-documenting
      
      df_inn_requirements_s6 <- df_inn_requirements_s5 |>
        rows_update(dt, by = "crit_code")
      
      
      fwrite(
        df_inn_requirements_s6,
        file = paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_requirements_s6.csv"
        )
      )
      removeModal()
        showModal(modalDialog(
          title = "Success",
          "Table saved successfully.",
          easyClose = TRUE
        ))
        
        enable("to_screen7")
      
    })
    
    
    # observeEvent Reset button----
    observeEvent(input$reset_btn_6_6, {
      #req(initial_data())  # only proceed if non-NULL
      table_data(initial_data())
    })
    
    # observeEvent Initial load button----
    observeEvent(input$initial_load, {
      df_inn_tree_net <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          #"_saved_tree_network.csv"
          "_links_s5.csv"
        )
      )
      
      df_inn_tree_net_stack <- dplyr::select(df_inn_tree_net, "stack_code") |> distinct()
      print(str(df_inn_tree_net_stack))
      message("S6. df_inn_tree_net_stack")
      print(df_inn_tree_net_stack)
      
      df_inn_requirements_s5 <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_requirements_s5.csv"
        )
      )
      message("S6. df_inn_requirements_s5")
      print(str(df_inn_requirements_s5))
      
      # add new rows to requirements based on tree network ----
      
      # this has now been done in the previous screen but still needed for Ad
      
      df_used_codes_req <- df_inn_requirements_s5 |> dplyr::select("crit_code") |> dplyr::distinct()
      used_codes_req <- df_used_codes_req[["crit_code"]]
      df_used_codes_tree <- df_inn_tree_net |> dplyr::select("stack_code", "stack") |> dplyr::distinct()
      used_codes_tree <- df_used_codes_tree[["stack_code"]]
      
      ## Find codes in df_used_codes_tree that are not in df_used_codes_req
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
        
        # Append new rows to df_inn_requirements_s5
        df_inn_requirements_updated <- rbind(df_inn_requirements_s5, df_new_codes_stackname)
      } else {
        # if there are no new codes ----
        df_inn_requirements_updated <- df_inn_requirements_s5
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
      dt_init <- df_inn_conc |> as.data.table()
      
      initial_data(dt_init)
      message("S6. 1 print(initial_data())")
      print(initial_data())
      table_data(initial_data())
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
    
    #2 observeEvent back_to_screen5 ----
    observeEvent(input$back_to_screen5, {
      switch_screen("screen5")
    })
    
    #2 observeEvent to_screen7 ----
    observeEvent(input$to_screen7, {
      shared_values$step <- 7
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      
      
      # saves the conc_table separately
      if (!file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          #"_saved_tree_network.csv"
          "_conc_s6.csv"
        )
      )) {
        fwrite(
          table_data(),
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_conc_s6.csv"
          )
        )
      }
      
      # saves the conc_table separately
      if (!file.exists(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          #"_saved_tree_network.csv"
          "_requirements_s6.csv"
        )
      )) {
      
        # overwrite and produce a new version of the requirements table
      
      df_inn_requirements_s5 <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_requirements_s5.csv"
        )
      )
      
      # saves the update requirements table separately      
      #df_inn_requirements_s6 <- merge(dt, df_inn_requirements_s5,  by = "crit_code")  # seems self-documenting
      
      df_inn_requirements_s6 <- df_inn_requirements_s5 |>
        rows_update(table_data(), by = "crit_code")
      
      
      fwrite(
        df_inn_requirements_s6,
        file = paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_requirements_s6.csv"
        )
      )}
      
      switch_screen("screen7")
      
    })
    
    
    # help button 06_01----
    observeEvent(input$show_help_06_01, {
      showModal(modalDialog(
        title = "Step 6: View or Edit Higher-level Rule Base Conclusions",
        includeMarkdown("docs/step_06_01.md"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
     
    
  })
}