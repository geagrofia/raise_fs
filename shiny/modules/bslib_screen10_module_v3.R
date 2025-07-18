bslib_screen10_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  
  tagList(
    h3("Spatial Data:"),
    
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
    DTOutput(ns("spatial_data_table")),
    uiOutput(ns("dyanamic_save_reset"))
  )
}

bslib_screen10_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NA_weights <- 0
    
    # Load the initial data ----
    initial_spatial_data <- reactive({
      req(switch_screen() == "screen10")
      message(paste("S10. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_values$crop_name_1,
        "_",
        shared_values$ideotype_1,
        "_",
        shared_values$scenario_1,
        "_requirements_s8.csv"
      ))) {
        
        message(
          paste(
            "S10. Initiation. inn details1:",
            shared_values$crop_name_1,
            "-",
            shared_values$ideotype_1,
            "-",
            shared_values$scenario_1
          )
        )
        df_requirements_s8 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_requirements_s8.csv"
          )
        )
        
        df_links_s5 <- read.csv(
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
        
        links_s5_crit_codes <- df_links_s5[["crit_code"]]
        links_s5_stack_codes <- df_links_s5[["stack_code"]]
        
        # Find crit_code in df_links_s5 that is not also stack_code
        leaves_codes <- setdiff(links_s5_crit_codes, links_s5_stack_codes)
        
        df_leaves_codes <- data.frame(crit_code = leaves_codes)
        df_requirements_s8_leaves <- left_join(df_leaves_codes,
                                               df_requirements_s8,
                                              by = c("crit_code"))
        
        message("S10. df_requirements_s8_leaves")
        print(df_requirements_s8_leaves)
        
        df_requirements_s8_leaves <- df_requirements_s8_leaves |> dplyr::select(
          c(
            "crit_code",
            "criterion",
            "data_desc",
            "data_file_prefix",
            "raster_or_brick",
            "agg_fun",
            "rsm_fun",
            "yield",
            "phen_stage",
            "temp_resolution",
            "prec_temp",
            "texture"
          )
        )
        
        # convert your data.frame to data.table
        dt <- as.data.table(df_requirements_s8_leaves)
        
        # assign an order index
        dt[, orig_order := .I]
        
        # find the first position for each crit_code
        first_pos <- dt[, .(first_occurrence = min(orig_order)), by = crit_code]
        
        # merge it back to get each row’s group start
        dt <- merge(dt, first_pos, by = "crit_code", all.x = TRUE)
        
        # sort first by first_occurrence, then within crit_code by original order
        setorder(dt, first_occurrence, crit_code, orig_order)
        
        # assign a group index for shading
        dt[, group_id := .GRP, by = crit_code]
        
        # drop helper columns if you wish
        dt[, c("orig_order", "first_occurrence") := NULL]
        
        message("S10. print(dt)")
        print(dt)
        
        # dt is now ready to display in shiny
        dt
        
      } else {
        NULL
      }
      
    })
    
    # Store the table reactively ----
    spatial_data_table_data  <- reactiveVal()
    #req(initial_spatial_data())  # only proceed if non-NULL
    current_spatial_data <- reactive({
      spatial_data_table_data()
    })
    
    observeEvent(initial_spatial_data(), {
      req(initial_spatial_data())  # only proceed if non-NULL
      spatial_data_table_data(initial_spatial_data())
    })
    
    # Render the editable data table----
    output$spatial_data_table <- DT::renderDT({
      if (shared_values$inn_type_1 == "existing") {
        
        # define a set of colors for each group
        group_colors <- c("#f2f2f2", "#e0f7fa", "#ffe0b2", "#dcedc8", "#f8bbd0")
        # repeat colors if there are more groups than colors
        group_colors <- rep(group_colors, length.out = max(current_spatial_data()$group_id))
        
        message("S10. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S10. print(jsonlite::toJSON(current_spatial_data()$group_id))")
        print(jsonlite::toJSON(current_spatial_data()$group_id))
        
        message("S10. print(current_spatial_data())")
        print(current_spatial_data())
        
        # this version does not try to highlight groups
        
        DT::datatable(
          current_spatial_data()[, !"group_id"],
          # hide group_id from view
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(lengthMenu = c(10, 20, 50), pageLength = 20)
        )
        
        # this version tries to highlight groups
        #     DT::datatable(
        #       current_spatial_data()[, !"group_id"],
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
        #             jsonlite::toJSON(current_spatial_data()$group_id),
        #             jsonlite::toJSON(group_colors)
        #           )
        #         )
        #       )
        #     )
      } else {
        
        
        
        # define a set of colors for each group
        group_colors <- c("#f2f2f2", "#e0f7fa", "#ffe0b2", "#dcedc8", "#f8bbd0")
        # repeat colors if there are more groups than colors
        group_colors <- rep(group_colors, length.out = max(current_spatial_data()$group_id))
        
        message("S10. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S10. print(jsonlite::toJSON(current_spatial_data()$group_id))")
        print(jsonlite::toJSON(current_spatial_data()$group_id))
        
        message("S10. print(current_spatial_data())")
        print(current_spatial_data())
        
        # this version does not try to highlight groups   
        
        # DT::datatable(
        #   current_spatial_data()[, !"group_id"],
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
          current_spatial_data()[, !"group_id"],
          # hide group_id from view
          rownames = F,
          filter = "bottom",
          selection = list(mode = "single"),
          editable = list(target = "cell", disable = list(columns = c(0:1))),
          # only edit the conclusions
          options = list(
            lengthMenu = c(10, 20, 50),
            pageLength = 20,
            rowCallback = JS(
              sprintf(
                "function(row, data, index) {
               var group_ids = %s;
               var gid = group_ids[index];
               if (gid !== null && gid > 0) {
                 $(row).addClass('group-' + gid);
               }
             }",
                jsonlite::toJSON(current_spatial_data()$group_id)
              )
            )
          )
        )
      }
    })
    
    observeEvent(input$spatial_data_table_cell_edit, {
      message("S10. str(input$spatial_data_table_cell_edit)")
      str(input$spatial_data_table_cell_edit)
    })
    
    
    #observeEvent Update table on cell edit----
    observeEvent(input$spatial_data_table_cell_edit, {
      info <- input$spatial_data_table_cell_edit
      
      message("S10. str(input$spatial_data_table_cell_edit)")
      str(input$spatial_data_table_cell_edit)
      
      dt <- copy(current_spatial_data())
      message("S10. 1 str(dt)")
      str(dt)
      
      colname <- names(dt)[ info$col + 1 ]  # adjust for 0-based to 1-based
      message("S10. colname")
      print(colname)
      
      old_class <- class(dt[[colname]])
      message("S10. old_class")
      print(old_class)
      
      coerced_val <- tryCatch({
        as(info$value, old_class)
      }, error = function(e) info$value)
      
      dt[info$row, (colname) := coerced_val]
      message("S10. 2 str(dt)")
      str(dt)
      
      spatial_data_table_data(dt)
    })
    
    
    # dynamic save reset controls ----
    output$dyanamic_save_reset <- renderUI({
      tagList(
        actionButton(ns("save_btn"), "Save table"),
        actionButton(ns("reset_btn"), "Reset table")
      )
    })
    
    
    # observeEvent save button----
    observeEvent(input$save_btn, {
      req(current_spatial_data())
      dt <- spatial_data_table_data()
      
      message("S10. 2 str(dt)")
      str(dt)
      
      problems_s10 <- c()
      message("S10. problems_s10")
      print(problems_s10)
      
      # problems_s10 that must be flagged:
      
      
      # (1) missing data description
      # (2) missing data source
      # (3) missing data type
      # (4) missing aggregation function type
      # (5) missing resampling function type
      
      mandatory <- c(3,4,5,6,7)
      for (col in mandatory) {

        if (any(is.na(dt[[col]]) | dt[[col]] == "")) {
          problems_s10 <- c(problems_s10, sprintf("Column %d has missing values", col))
        }
      }
      
      # (6) data type must be "rast" or "brick"

      allowed_rast <- c(
        "rast",
        "brick"
      )
      
      message("S10. allowed_rast")
      print(allowed_rast)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 5]
        if (!(pattern %in% allowed_rast)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in rast/brick: %s", i, pattern))
        }
      }
      
      # (7) aggregation function must be allowed by terra

      allowed_agg_fun <- c(
        "mean",
        "max",
        "min",
        "median",
        "sum",
        "modal",
        "any",
        "all",
        "prod",
        "which.min",
        "which.max",
        "table",
        "sd" ,
        "std"
      )
      
      message("S10. allowed_agg_fun")
      print(allowed_agg_fun)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 6]
        if (!(pattern %in% allowed_agg_fun)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in aggregation function: %s", i, pattern))
        }
      }
      
      # (8) resample function must be allowed by terra
      
      allowed_rsmp_fun <- c(
        "bilinear" ,
        "average" ,
        "near" ,
        "mode" ,
        "cubic" ,
        "cubicspline" ,
        "lanczos" ,
        "sum" ,
        "min" ,
        "q1" ,
        "median" ,
        "q3" ,
        "max:" ,
        "rms"
      )
      
      message("S10. allowed_rsmp_fun")
      print(allowed_rsmp_fun)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 7]
        if (!(pattern %in% allowed_rsmp_fun)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in resample function: %s", i, pattern))
        }
      }    
      
      
      # (9) phenological stage must exist in the growth stages table
        
        if (file.exists(paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1,
          "_gs_s9.csv"
        ))) {
          
          df_gs_s9 <- read.csv(
            paste0(
              "E:/repos/raise_fs/shiny/data/",
              shared_values$crop_name_1,
              "_",
              shared_values$ideotype_1,
              "_",
              shared_values$scenario_1,
              "_gs_s9.csv"
            )
          ) } else {          df_gs_s9 <- read.csv(
            paste0(
              "E:/repos/raise_fs/shiny/data/",
              shared_values$crop_name_1,
              "_",
              shared_values$ideotype_1,
              "_",
              shared_values$scenario_1,
              "_gs.csv"
            ))}
          
        allowed_phen_stage <- df_gs_s9[["gs_name"]]
        allowed_phen_stage <- c(allowed_phen_stage, "", NA)
      
      message("S10. allowed_phen_stage")
      print(allowed_phen_stage)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 9]
        if (!(pattern %in% allowed_phen_stage)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in phenological stage: %s", i, pattern))
        }
      }     
      
      # (10) temporal resolution code must be allowed by database protocol
      
      allowed_temp_resolution <- c(
        "d" ,
        "m",
        "",
        NA
      )
      
      message("S10. allowed_temp_resolution")
      print(allowed_temp_resolution)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 10]
        if (!(pattern %in% allowed_temp_resolution)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in temporal resolution code: %s", i, pattern))
        }
      }       
      
      # (10) precipitation/temperature code must be allowed by database protocol
      
      allowed_prec_temp <- c(
        "p" ,
        "t",
        "",
        NA
      )
      
      message("S10. allowed_prec_temp")
      print(allowed_prec_temp)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 11]
        if (!(pattern %in% allowed_prec_temp)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in precipitation/temperature code: %s", i, pattern))
        }
      } 
      
      # (11) add columns to growth stages table if precipitation or temperature criteria are specific to growth stages 
      
      # get growth stages
      message("S10. growth stages")
      p_gs_codes <- c()
      t_gs_codes <- c()
      
      # 
      for (i in 1:nrow(dt)) {
        pattern <- dt[i, 11]
        
        if ((pattern == "p")) {
          p_gs_codes <- c(p_gs_codes, dt[i, 9] )}
        if ((pattern == "t")) {
          t_gs_codes <- c(t_gs_codes, dt[i, 9] )}
        }
      
      # add variables to growth stages table
      df_gs_s10 <- mutate(df_gs_s9, prec_criteria = ifelse(gs_name %in% p_gs_codes, 1, 0), temp_criteria = ifelse(gs_name %in% t_gs_codes, 1, 0))
      
      message("S10. df_gs_s10")
      print(str(df_gs_s10))
      
      # save growth stages table
      fwrite(
        df_gs_s10, file = paste0(
          "E:/repos/raise_fs/shiny/data/", shared_values$crop_name_1, "_", shared_values$ideotype_1, "_", shared_values$scenario_1, "_gs_s10.csv"
        )
      )
      
      
      # (12) yield code must be allowed by database protocol
      
      allowed_yield <- c(
        "NA",
        "1" ,
        "",
        NA
      )
      
      message("S10. allowed_yield")
      print(allowed_yield)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 8]
        if (!(pattern %in% allowed_yield)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in allowed yield code: %s", i, pattern))
        }
      }      
      
      # (12) soil texture code must be allowed by database protocol
      
      allowed_texture <- c(
        "NA",
        "1" ,
        "",
        NA
      )
      
      message("S10. allowed_texture")
      print(allowed_texture)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 12]
        if (!(pattern %in% allowed_texture)) {
          problems_s10 <- c(problems_s10,
                           sprintf("Row %d invalid value in allowed texture code: %s", i, pattern))
        }
      }       
      
      if (length(problems_s10) > 0) {
        
        message("S10. (length(problems_s10) > 0)")
        print(problems_s10)
        
        removeModal()
        showModal(
          modalDialog(
            title = "Error", 
            paste(problems_s10, collapse = "<br>"), 
            easyClose = TRUE))
        
    } else {
      
      message("S10. save logic")
      
     # overwrite and produce a new version of the requirements table
      
      df_inn_requirements_s8 <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/", shared_values$crop_name_1, "_", shared_values$ideotype_1, "_", shared_values$scenario_1, "_requirements_s8.csv"
        )
      )
      
      # saves the update requirements table separately
      #df_inn_requirements_s6 <- merge(dt, df_inn_requirements_s5,  by = "crit_code")  # seems self-documenting
      
      df_inn_requirements_s10 <- df_inn_requirements_s8 |>
        rows_update(dplyr::select(dt, -c("group_id")), by = "crit_code")
      
      fwrite(
        df_inn_requirements_s10, file = paste0(
          "E:/repos/raise_fs/shiny/data/", shared_values$crop_name_1, "_", shared_values$ideotype_1, "_", shared_values$scenario_1, "_requirements_s10.csv"
        )
      )
      
      removeModal()
      showModal(modalDialog(
        title = "Saved",
        "Table saved successfully.",
        easyClose = TRUE))
    }
      })
    
    
    # observeEvent reset button----
    observeEvent(input$reset_btn, {
      #req(initial_spatial_data())  # only proceed if non-NULL
      spatial_data_table_data(initial_spatial_data())
    })
    
    
    
    
    
    
    
    # 
    # 
    # #observeEvent Update table on cell edit----
    # 
    # observeEvent(input$spatial_data_table_cell_edit, {
    #   
    #   print(str(input$spatial_data_table_cell_edit))
    #   
    #   message(paste("observeEvent Update table on cell edit"))
    #   info <- input$spatial_data_table_cell_edit
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
      message(paste("S10. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S10. You selected crop on Screen 4:", shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S10. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S10. You selected ideotype on Screen 4:", shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S10. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S10. You selected scenario on Screen 4:", shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S10. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S10. You selected Innovation type on Screen 4:", shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    #2 observeEvent back_to_screen9 ----
    observeEvent(input$back_to_screen9, {
      switch_screen("screen9")
    })
    
    #2 observeEvent to_screen11 ----
    observeEvent(input$to_screen11, {
      switch_screen("screen11")
      
    })
    
  })
}