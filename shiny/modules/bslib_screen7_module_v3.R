bslib_screen7_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  
  tagList(
    h3("Rule Base Weights:"),
    
    # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen6"), "Back to Screen 6"),
    actionButton(ns("to_screen8"), "Go to Screen 8")
    
  )
}

bslib_screen7_module_v3_MainUI <- function(id) {
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
    DTOutput(ns("weights_data_table")),
    tags$style(
      HTML(
        "
    table.dataTable tbody tr.group-1 td { background-color: #f2f2f2 !important; }
    table.dataTable tbody tr.group-2 td { background-color: #e0f7fa !important; }
    table.dataTable tbody tr.group-3 td { background-color: #ffe0b2 !important; }
    table.dataTable tbody tr.group-4 td { background-color: #dcedc8 !important; }
    table.dataTable tbody tr.group-5 td { background-color: #f8bbd0 !important; }
    table.dataTable tbody tr.group-6 td { background-color: #f2f2f2 !important; }
    table.dataTable tbody tr.group-7 td { background-color: #e0f7fa !important; }
    table.dataTable tbody tr.group-8 td { background-color: #ffe0b2 !important; }
    table.dataTable tbody tr.group-9 td { background-color: #dcedc8 !important; }
  "
      )
    ),
    uiOutput(ns("dyanamic_save_reset"))
    
  )
}

bslib_screen7_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load the initial data ----
    initial_weights_data <- reactive({
      req(switch_screen() == "screen7")
      message(paste("S7. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_values$crop_name_1,
        "_",
        shared_values$ideotype_1,
        "_",
        shared_values$scenario_1,
        "_links_s5.csv"
      ))) {
      
      message(
        paste(
          "S7. Initiation. inn details1:",
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
          "_links_s5.csv"
        )
      )
        
      # df_inn_tree_net_stack <- dplyr::select(df_inn_tree_net, "stack_code") |> distinct()
      # print(str(df_inn_tree_net_stack))
      # print(df_inn_tree_net_stack)
      # 
      # if (file.exists(
      #   paste0(
      #     "E:/repos/raise_fs/shiny/data/",
      #     shared_values$crop_name_1,
      #     "_",
      #     shared_values$ideotype_1,
      #     "_",
      #     shared_values$scenario_1,
      #     "_requirements_s6.csv"
      #   )
      # ))
      #   
      # {
      #   message("S7. ..._requirements_s6 file exists")
      #   
      #   df_inn_requirements_s6 <- read.csv(
      #     paste0(
      #       "E:/repos/raise_fs/shiny/data/",
      #       shared_values$crop_name_1,
      #       "_",
      #       shared_values$ideotype_1,
      #       "_",
      #       shared_values$scenario_1,
      #       "_requirements_s6.csv"
      #     )
      #   )
      # }
      # 
      # 
      # message("S7. df_inn_requirements_s6")
      # print(str(df_inn_requirements_s6))
      # 
      # # add new rows to requirements based on tree network ----
      # 
      # df_used_codes_req <- df_inn_requirements_s6 |> dplyr::select("crit_code") |> dplyr::distinct()
      # used_codes_req <- df_used_codes_req[["crit_code"]]
      # df_used_codes_tree <- df_inn_tree_net |> dplyr::select("stack_code", "stack") |> dplyr::distinct()
      # used_codes_tree <- df_used_codes_tree[["stack_code"]]
      # 
      # # Find codes in df_used_codes_tree that are not in df_used_codes_req
      # new_codes <- setdiff(used_codes_tree, used_codes_req)
      # 
      # # if there are new codes ----
      # 
      # if (length(new_codes) > 0) {
      #   df_new_codes <- data.frame(crit_code = new_codes)
      #   df_new_codes_stackname <- left_join(df_new_codes,
      #                                       df_used_codes_tree,
      #                                       join_by(crit_code == stack_code))
      #   message("S7. df_new_codes_stackname")
      #   print(df_new_codes_stackname)
      #   
      #   v_req_names <- c(
      #     "weight",
      #     "threshold_1",
      #     "threshold_2",
      #     "width_1",
      #     "width_2",
      #     "thresh_source",
      #     "data_desc",
      #     "data_file_prefix",
      #     "raster_or_brick",
      #     "agg_fun",
      #     "rsm_fun",
      #     "prop_level_1",
      #     "prop_level_2",
      #     "prop_level_3",
      #     "conc_level_1" ,
      #     "conc_level_2",
      #     "conc_level_3",
      #     "fuzzy_partition",
      #     "yield" ,
      #     "phen_stage",
      #     "temp_resolution",
      #     "prec_temp",
      #     "texture"
      #   )
      #   
      #   df_new_codes_stackname[, v_req_names] <- NA
      #   message("S7. df_new_codes_stackname")
      #   print(df_new_codes_stackname)
      #   
      #   #df_new_codes_stackname <- mutate(df_new_codes_stackname, criterion == stack)
      #   df_new_codes_stackname <- df_new_codes_stackname |> rename(criterion = stack)
      #   message("S7. df_new_codes_stackname")
      #   print(df_new_codes_stackname)
      #   
      #   # Append new rows to df_inn_requirements
      #   df_inn_requirements_s6_updated <- rbind(df_inn_requirements_s6, df_new_codes_stackname)
      # } else {
      #   # if there are no new codes ----
      #   df_inn_requirements_s6_updated <- df_inn_requirements_s6
      # }
      # 
      # message("S7. df_inn_tree_net")
      # print(df_inn_tree_net)
      # 
      # message("S7. df_inn_requirements_s6_updated")
      # print(df_inn_requirements_s6_updated)
      # 
      # 
      # df_inn_weight <- dplyr::left_join(
      #   df_inn_tree_net_stack,
      #   df_inn_requirements_s6_updated,
      #   join_by(stack_code == crit_code),
      #   keep = T
      # ) 
      # message("S7. df_inn_weight")
      # print(df_inn_weight)
      # 
      # #|> 
      # df_inn_weight <- dplyr::left_join(df_inn_weight,
      #   dplyr::select(df_inn_tree_net, "stack", "crit_code"),
      #   by = c("crit_code")
      # ) 
      # 
      # message("S7. df_inn_weight")
      # print(df_inn_weight)
      # 
      # #|>
      # df_inn_weight <-  dplyr::select(df_inn_weight, "stack_code", "stack", "crit_code", "criterion", "weight")
      # 
      # message("S7. str(df_inn_weight)")
      # print(str(df_inn_weight))
      # message("S7. df_inn_weight")
      # print(df_inn_weight)
      
      #df_inn_tree_net |> as.data.table()
      
      # convert your data.frame to data.table
      dt <- as.data.table(df_inn_tree_net)
      
      # assign an order index
      dt[, orig_order := .I]
      
      # find the first position for each stack_code
      first_pos <- dt[, .(first_occurrence = min(orig_order)), by = stack_code]
      
      # merge it back to get each row’s group start
      dt <- merge(dt, first_pos, by = "stack_code", all.x = TRUE)
      
      # sort first by first_occurrence, then within stack_code by original order
      setorder(dt, first_occurrence, stack_code, orig_order)
      
      # assign a group index for shading
      dt[, group_id := .GRP, by = stack_code]
      
      # drop helper columns if you wish
      dt[, c("orig_order", "first_occurrence") := NULL]
      
      message("S7. print(dt)")
      print(dt)
      
      # dt is now ready to display in shiny
      dt
      
      } else {
        NULL
      }
      
    })
    
    # Store the table reactively ----
    weights_table_data  <- reactiveVal()
    #req(initial_weights_data())  # only proceed if non-NULL
    current_weights_data <- reactive({
      weights_table_data()
    })
    
    observeEvent(initial_weights_data(), {
      req(initial_weights_data())  # only proceed if non-NULL
      weights_table_data(initial_weights_data())
    })
    
    # Render the editable data table----
    output$weights_data_table <- DT::renderDT({
      if (shared_values$inn_type_1 == "existing") {
        
        # define a set of colors for each group
        group_colors <- c("#f2f2f2", "#e0f7fa", "#ffe0b2", "#dcedc8", "#f8bbd0")
        # repeat colors if there are more groups than colors
        group_colors <- rep(group_colors, length.out = max(current_weights_data()$group_id))
        
        message("S7. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S7. print(jsonlite::toJSON(current_weights_data()$group_id))")
        print(jsonlite::toJSON(current_weights_data()$group_id))
        
        message("S7. print(current_weights_data())")
        print(current_weights_data())
        
        # this version does not try to highlight groups
        
        DT::datatable(
          current_weights_data()[, !"group_id"],
          # hide group_id from view
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(lengthMenu = c(10, 20, 50), pageLength = 20)
        )
        
        # this version tries to highlight groups
        #     DT::datatable(
        #       current_weights_data()[, !"group_id"],
        #       # hide group_id from view
        #       rownames = F,
        #       filter = "bottom",
        #       selection = list(mode = "none"),
        #       editable = FALSE,
        #       options = list(
        #         lengthMenu = c(10, 20, 50),
        #         pageLength = 20,
        #         rowCallback = JS(
        #           sprintf(
        #             "
        #   function(row, data, index) {
        #     var group_id = %s[index];
        #     var colors = %s;
        #     $(row).css('background-color', colors[group_id - 1]);
        #   }
        # ",
        #             jsonlite::toJSON(current_weights_data()$group_id),
        #             jsonlite::toJSON(group_colors)
        #           )
        #         )
        #       )
        #     )
      } else {
        

        
        # define a set of colors for each group
        group_colors <- c("#f2f2f2", "#e0f7fa", "#ffe0b2", "#dcedc8", "#f8bbd0")
        # repeat colors if there are more groups than colors
        group_colors <- rep(group_colors, length.out = max(current_weights_data()$group_id))
        
        message("S7. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S7. print(jsonlite::toJSON(current_weights_data()$group_id))")
        print(jsonlite::toJSON(current_weights_data()$group_id))
        
        message("S7. print(current_weights_data())")
        print(current_weights_data())
        
        # this version does not try to highlight groups   
        
        # DT::datatable(
        #   current_weights_data()[, !"group_id"],
        #   # hide group_id from view
        #   rownames = F,
        #   filter = "bottom",
        #   selection = list(mode = "single"),
        #   editable = list(target = "cell", disable = list(columns = c(0:3))),
        #   # only edit the conclusions
        #   options = list(lengthMenu = c(10, 20, 50), pageLength = 20)
        #   
        # )
            
            
            # this version tries to highlight groups
            
        DT::datatable(
          current_weights_data()[, !"group_id"],
          # hide group_id from view
          rownames = F,
          filter = "bottom",
          selection = list(mode = "single"),
          editable = list(target = "cell", disable = list(columns = c(0:3))),
          # only edit the conclusions
          options = list(
            lengthMenu = c(10, 20, 50),
            pageLength = 20,
            
    #         rowCallback = JS(
    #           sprintf(
    #             "
    #   function(row, data, index) {
    #     var group_id = %s[index];
    #     var colors = %s;
    #     $(row).css('background-color', colors[group_id - 1]);
    #   }
    # ",
    #             jsonlite::toJSON(current_weights_data()$group_id),
    #             jsonlite::toJSON(group_colors)
    #           )
    #         )
    
    # added copied from screen8 -  this works
    
    rowCallback = JS(
      sprintf(
        "function(row, data, index) {
               var group_ids = %s;
               var gid = group_ids[index];
               if (gid !== null && gid > 0) {
                 $(row).addClass('group-' + gid);
               }
             }",
        jsonlite::toJSON(current_weights_data()$group_id)
      )
    )
            
            
            
          )
        )
      }
    })
    
    observeEvent(input$weights_data_table_cell_edit, {
      message("S7. str(input$weights_data_table_cell_edit)")
      str(input$weights_data_table_cell_edit)
    })
    
    
    #observeEvent Update table on cell edit----
    observeEvent(input$weights_data_table_cell_edit, {
      info <- input$weights_data_table_cell_edit
      
      message("S7. str(input$weights_data_table_cell_edit)")
      str(input$weights_data_table_cell_edit)
      
      dt <- copy(current_weights_data())
      message("S7. 1 str(dt)")
      str(dt)

      colname <- names(dt)[ info$col + 1 ]  # adjust for 0-based to 1-based
      message("S7. colname")
      print(colname)
      
      old_class <- class(dt[[colname]])
      message("S7. old_class")
      print(old_class)
      
      coerced_val <- tryCatch({
        as(info$value, old_class)
      }, error = function(e) info$value)
      
      dt[info$row, (colname) := coerced_val]
      message("S7. 2 str(dt)")
      str(dt)
      
      weights_table_data(dt)
    })
    
    
    # table controls
    output$dyanamic_save_reset <- renderUI({
          tagList(
          actionButton(ns("save_btn"), "Save table"),
          actionButton(ns("reset_btn"), "Reset table")
        )
    })
    
    
    # observeEvent save button----
    observeEvent(input$save_btn, {
      req(current_weights_data())
      dt <- weights_table_data()
      
      problems <- list()
      NA_weights <- 0
      
      
      for (gid in unique(dt$group_id)) {
        group_sum <- sum(dt$weight[dt$group_id == gid])
        
        message(paste("S7. group_sum", gid))
        print(group_sum)
        
        if (is.na(group_sum)) {
          NA_weights <- 1
        } else {
          if (group_sum < 0.95 || group_sum > 1.05) {
            problems <- c(problems, sprintf("Group %s sums to %.2f", gid, group_sum))
          }
        }
      }
      
      #if (anyNA(dt$weight)) {problems <- c(problems, sprintf("NAs", "gid", "group_sum"))}
      
      if (length(problems) > 0) {
        removeModal()
        showModal(
          modalDialog(
            title = "Validation Error",
            paste("These groups do not sum to 1 +/- 0.1:", paste(problems, collapse=", ")),
            easyClose = TRUE
          )
        )
      } else  {
        
        if ( NA_weights == 1) { 
          removeModal()
          showModal(
            modalDialog(
              title = "Check NAs",
              paste("Some rule bases have no weight values"),
              easyClose = TRUE)
            )
          
          NA_weights <- 0
          
          }
      
    # save logic here
        fwrite(dt, file = paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_weights_s7.csv"
        ))
        
        # overwrite and produce a new version of the requirements table
        
        df_inn_requirements_s6 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_requirements_s6.csv"
          )
        )
        
        # saves the update requirements table separately      
        #df_inn_requirements_s6 <- merge(dt, df_inn_requirements_s5,  by = "crit_code")  # seems self-documenting
        
        df_inn_requirements_s7 <- df_inn_requirements_s6 |> 
          rows_update(dplyr::select(dt, -c( "stack_code", "stack", "group_id")), by = "crit_code")
        
        
        fwrite(
          df_inn_requirements_s7,
          file = paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_requirements_s7.csv"
          )
        )
        
        removeModal()
        showModal(modalDialog(
          title = "Success",
          "Table saved successfully.",
          easyClose = TRUE
        ))
      }
    })
    
    
    # observeEvent reset button----
    observeEvent(input$reset_btn, {
      #req(initial_weights_data())  # only proceed if non-NULL
      weights_table_data(initial_weights_data())
    })
    
    
    
    
    
    
    
    # 
    # 
    # #observeEvent Update table on cell edit----
    # 
    # observeEvent(input$weights_data_table_cell_edit, {
    #   
    #   print(str(input$weights_data_table_cell_edit))
    #   
    #   message(paste("observeEvent Update table on cell edit"))
    #   info <- input$weights_data_table_cell_edit
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
      message(paste("S7. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S7. You selected crop on Screen 4:", shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S7. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S7. You selected ideotype on Screen 4:", shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S7. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S7. You selected scenario on Screen 4:", shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S7. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S7. You selected Innovation type on Screen 4:", shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    #2 observeEvent back_to_screen6 ----
    observeEvent(input$back_to_screen6, {
      switch_screen("screen6")
    })
    
    #2 observeEvent to_screen8 ----
    observeEvent(input$to_screen8, {
      switch_screen("screen8")
      
    })
    
  })
}