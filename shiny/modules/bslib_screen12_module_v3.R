bslib_screen12_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(
    #     ns("back_to_screen11"),
    #     label = tagList(
    #       icon("circle-left"),
    #       # icon first
    #       "Back to Screen 11"
    #       # text second
    #     ),
    #     class = "btn-primary"
    #   ),
    #   actionButton(
    #     ns("to_screen13"),
    #     label = tagList(
    #       "Go to Screen 13",
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
          ns("show_help_12_01"),
          title = "Help for Step 12",
          label = tagList(
            icon("circle-question")  # icon second)
          ),
          style = "background: rgba(23, 162, 184, 0.5);"
        )
        
      ),
      div(
        style = "display: inline-block; vertical-align: middle; margin-left: 10px;",
      h4("Step 12: View or Edit Spatial Data")
      )
    ),
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      h5("Accessibility Analysis:"),
      checkboxInput(ns("services_local"), "Access to local services (e.g. FTCs)", value = FALSE),
      checkboxInput(ns("services_woreda"), "Access to woreda services (e.g. SMS)", value = FALSE),
      checkboxInput(ns("services_zone"), "Access to zonal services (e.g. Union)", value = FALSE),
      checkboxInput(ns("markets_local"), "Access to local markets", value = FALSE),
      checkboxInput(ns("markets_woreda"), "Access to woreda markets", value = FALSE),
      checkboxInput(ns("markets_zone"), "Access to regional markets (e.g. wholesalers)", value = FALSE),
      downloadButton(ns("run_Acc_btn"), "Run Accessibility Analysis"),
    #downloadButton(ns("run_Accfewpar_btn"), "Run Accessibility Analysis with fewer params"),
      uiOutput(ns("extra_ui"))
    ),
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      h5("Spatial Data:"),    
      scrollable_DT(ns("spatial_data_table")),
      uiOutput(ns("dyanamic_save_reset"))
    )
  )
}



bslib_screen12_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen11"), 
                   title = "Go back to Step 11: View or Edit Yield Table",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 12", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen13"), 
                   title = "Go to Step 13: Run IRM or start Innovation 2",
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

bslib_screen12_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NA_weights <- 0
    
    shared_parameters$services_local <- 0
    shared_parameters$services_woreda <- 0
    shared_parameters$services_zone <- 0
    shared_parameters$markets_local <- 0
    shared_parameters$markets_woreda <- 0
    shared_parameters$markets_zone <- 0
    
    # Load the initial data ----
    initial_spatial_data <- reactive({
      req(switch_screen() == "screen12")
      message(paste("S12. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_parameters$crop_name_1,
        "_",
        shared_parameters$ideotype_1,
        "_",
        shared_parameters$scenario_1,
        "_requirements_s8.csv"
      ))) {
        
        message(
          paste(
            "S12. Initiation. inn details1:",
            shared_parameters$crop_name_1,
            "-",
            shared_parameters$ideotype_1,
            "-",
            shared_parameters$scenario_1
          )
        )
        df_requirements_s8 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_requirements_s8.csv"
          )
        )
        
        df_links_s5 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
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
        
        message("S12. df_requirements_s8_leaves")
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
        
        message("S12. print(dt)")
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
        
        message("S12. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S12. print(jsonlite::toJSON(current_spatial_data()$group_id))")
        print(jsonlite::toJSON(current_spatial_data()$group_id))
        
        message("S12. print(current_spatial_data())")
        print(current_spatial_data())
        
        # this version does not try to highlight groups
        
        DT::datatable(
          current_spatial_data()[, !"group_id"],
          # hide group_id from view
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          extensions = c('FixedColumns', 'FixedHeader'),
          editable = FALSE,
          options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2), lengthMenu = c(10, 20, 50), pageLength = 10)
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
        
        message("S12. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S12. print(jsonlite::toJSON(current_spatial_data()$group_id))")
        print(jsonlite::toJSON(current_spatial_data()$group_id))
        
        message("S12. print(current_spatial_data())")
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
          extensions = c('FixedColumns', 'FixedHeader'),
          editable = list(target = "cell", disable = list(columns = c(0:1))),
          # only edit the conclusions
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2),
            lengthMenu = c(10, 20, 50),
            pageLength = 10,
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
      message("S12. str(input$spatial_data_table_cell_edit)")
      str(input$spatial_data_table_cell_edit)
    })
    
    
    #observeEvent Update table on cell edit----
    observeEvent(input$spatial_data_table_cell_edit, {
      info <- input$spatial_data_table_cell_edit
      
      message("S12. str(input$spatial_data_table_cell_edit)")
      str(input$spatial_data_table_cell_edit)
      
      dt <- copy(current_spatial_data())
      message("S12. 1 str(dt)")
      str(dt)
      
      colname <- names(dt)[ info$col + 1 ]  # adjust for 0-based to 1-based
      message("S12. colname")
      print(colname)
      
      old_class <- class(dt[[colname]])
      message("S12. old_class")
      print(old_class)
      
      coerced_val <- tryCatch({
        as(info$value, old_class)
      }, error = function(e) info$value)
      
      dt[info$row, (colname) := coerced_val]
      message("S12. 2 str(dt)")
      str(dt)
      
      spatial_data_table_data(dt)
    })
    
    
    # dynamic save reset controls ----
    output$dyanamic_save_reset <- renderUI({
      tagList(
        actionButton(ns("save_btn_spatial"), "Save Spatial Data table",
                     class = "btn-primary"),
        actionButton(ns("reset_btn"), "Reset Spatial Data table",
                     class = "btn-primary")
      )
    })
    
    
    # observeEvent save button----
    observeEvent(input$save_btn_spatial, {
      
      req(current_spatial_data())
      
      dt <- spatial_data_table_data()
      
      message("S12. 2 str(dt)")
      str(dt)
      
      problems_s12 <- c()
      message("S12. problems_s12")
      print(problems_s12)
      
      # problems_s12 that must be flagged:
      
      
      # (1) missing data description
      # (2) missing data source
      # (3) missing data type
      # (4) missing aggregation function type
      # (5) missing resampling function type
      
      mandatory <- c(3,4,5,6,7)
      for (col in mandatory) {

        if (any(is.na(dt[[col]]) | dt[[col]] == "")) {
          problems_s12 <- c(problems_s12, sprintf("Column %d has missing values", col))
        }
      }
      
      # (6) data type must be "rast" or "brick"

      allowed_rast <- c(
        "rast",
        "brick"
      )
      
      message("S12. allowed_rast")
      print(allowed_rast)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 5]
        if (!(pattern %in% allowed_rast)) {
          problems_s12 <- c(problems_s12,
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
      
      message("S12. allowed_agg_fun")
      print(allowed_agg_fun)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 6]
        if (!(pattern %in% allowed_agg_fun)) {
          problems_s12 <- c(problems_s12,
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
        "max" ,
        "rms"
      )
      
      message("S12. allowed_rsmp_fun")
      print(allowed_rsmp_fun)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 7]
        if (!(pattern %in% allowed_rsmp_fun)) {
          problems_s12 <- c(problems_s12,
                           sprintf("Row %d invalid value in resample function: %s", i, pattern))
        }
      }    
      
      
      # (9) phenological stage must exist in the growth stages table
        
        if (file.exists(paste0(
          "E:/repos/raise_fs/shiny/data/",
          shared_parameters$crop_name_1,
          "_",
          shared_parameters$ideotype_1,
          "_",
          shared_parameters$scenario_1,
          "_gs_s9.csv"
        ))) {
          
          df_gs_s9 <- read.csv(
            paste0(
              "E:/repos/raise_fs/shiny/data/",
              shared_parameters$crop_name_1,
              "_",
              shared_parameters$ideotype_1,
              "_",
              shared_parameters$scenario_1,
              "_gs_s9.csv"
            )
          ) } else {          df_gs_s9 <- read.csv(
            paste0(
              "E:/repos/raise_fs/shiny/data/",
              shared_parameters$crop_name_1,
              "_",
              shared_parameters$ideotype_1,
              "_",
              shared_parameters$scenario_1,
              "_gs.csv"
            ))}
          
        allowed_phen_stage <- df_gs_s9[["gs_name"]]
        allowed_phen_stage <- c(allowed_phen_stage, "", NA)
      
      message("S12. allowed_phen_stage")
      print(allowed_phen_stage)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 9]
        if (!(pattern %in% allowed_phen_stage)) {
          problems_s12 <- c(problems_s12,
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
      
      message("S12. allowed_temp_resolution")
      print(allowed_temp_resolution)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 10]
        if (!(pattern %in% allowed_temp_resolution)) {
          problems_s12 <- c(problems_s12,
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
      
      message("S12. allowed_prec_temp")
      print(allowed_prec_temp)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 11]
        if (!(pattern %in% allowed_prec_temp)) {
          problems_s12 <- c(problems_s12,
                           sprintf("Row %d invalid value in precipitation/temperature code: %s", i, pattern))
        }
      } 
      
      # (11) add columns to growth stages table if precipitation or temperature criteria are specific to growth stages 
      
      # get growth stages
      message("S12. growth stages")
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
      df_gs_s12 <- mutate(df_gs_s9, prec_criteria = ifelse(gs_name %in% p_gs_codes, 1, 0), temp_criteria = ifelse(gs_name %in% t_gs_codes, 1, 0))
      
      message("S12. df_gs_s12")
      print(str(df_gs_s12))
      
      # save growth stages table
      fwrite(
        df_gs_s12, file = paste0(
          "E:/repos/raise_fs/shiny/data/", shared_parameters$crop_name_1, "_", shared_parameters$ideotype_1, "_", shared_parameters$scenario_1, "_gs_s12.csv"
        )
      )
      
      
      # (12) yield code must be allowed by database protocol
      
      allowed_yield <- c(
        "NA",
        "1" ,
        "",
        NA
      )
      
      message("S12. allowed_yield")
      print(allowed_yield)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 8]
        if (!(pattern %in% allowed_yield)) {
          problems_s12 <- c(problems_s12,
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
      
      message("S12. allowed_texture")
      print(allowed_texture)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- dt[i, 12]
        if (!(pattern %in% allowed_texture)) {
          problems_s12 <- c(problems_s12,
                           sprintf("Row %d invalid value in allowed texture code: %s", i, pattern))
        }
      }       
      
      if (length(problems_s12) > 0) {
        
        message("S12. (length(problems_s12) > 0)")
        print(problems_s12)
        
        removeModal()
        showModal(
          modalDialog(
            title = "Error", 
            paste(problems_s12, collapse = "<br>"), 
            easyClose = TRUE))
        
    } else {
      
      message("S12. save logic")
      
     # overwrite and produce a new version of the requirements table
      
      df_inn_requirements_s8 <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/", shared_parameters$crop_name_1, "_", shared_parameters$ideotype_1, "_", shared_parameters$scenario_1, "_requirements_s8.csv"
        )
      )
      
      # saves the update requirements table separately
      #df_inn_requirements_s6 <- merge(dt, df_inn_requirements_s5,  by = "crit_code")  # seems self-documenting
      
      df_inn_requirements_s12 <- df_inn_requirements_s8 |>
        rows_update(dplyr::select(dt, -c("group_id")), by = "crit_code")
      
      fwrite(
        df_inn_requirements_s12, file = paste0(
          "E:/repos/raise_fs/shiny/data/", shared_parameters$crop_name_1, "_", shared_parameters$ideotype_1, "_", shared_parameters$scenario_1, "_requirements_s12.csv"
        )
      )
      
      enable("to_screen13")
      
      removeModal()
      
      showModal(modalDialog(
        title = "Saved",
        "Spatial Data Table saved successfully.",
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
    
    
    # observe parameters ----
    observe({
     req(input$services_local)
      message("S12. observe parameters")
      shared_parameters$services_local <- ifelse(input$services_local, 1, 0)
      message("S12. shared_parameters$services_local")
      print(shared_parameters$services_local)
    })
    
    # observe parameters ----
    observe({
      req(input$services_woreda)
      message("S12. observe parameters")
      shared_parameters$services_woreda <- ifelse(input$services_woreda, 1, 0)
      message("S12. shared_parameters$services_woreda")
      print(shared_parameters$services_woreda)
    })
    
    # observe parameters ----
    observe({
      req(input$services_zone)
      message("S12. observe parameters")
      shared_parameters$services_zone <- ifelse(input$services_zone, 1, 0)
      message("S12. shared_parameters$services_zone")
      print(shared_parameters$services_zone)
    })
    
    # observe parameters ----
    observe({
      req(input$markets_local)
      message("S12. observe parameters")
      shared_parameters$markets_local <- ifelse(input$markets_local, 1, 0)
      message("S12. shared_parameters$markets_local")
      print(shared_parameters$markets_local)
    })
    
    # observe parameters ----
    observe({
      req(input$markets_woreda)
      message("S12. observe parameters")
      shared_parameters$markets_woreda <- ifelse(input$markets_woreda, 1, 0)
      message("S12. shared_parameters$markets_woreda")
      print(shared_parameters$markets_woreda)
    })
    
    # observe parameters ----
    observe({
      req(input$markets_zone)
      message("S12. observe parameters")
      shared_parameters$markets_zone <- ifelse(input$markets_zone, 1, 0)
      message("S12. shared_parameters$markets_zone")
      print(shared_parameters$markets_zone)
    })
    
    
    # downloadHandler run_Acc_btn ----
    output$run_Acc_btn  <- downloadHandler(
      filename = paste0(
        shared_parameters$crop_name_1,
        "_",
        shared_parameters$ideotype_1,
        "_",
        shared_parameters$scenario_1,
        "_accessibility_",
        shared_parameters$DIVCODEVAL,
        ".html"
      ),
      content = function(file) {
        withProgress(value = 0, message = 'Starting, this may take some time', {
          params_acc <- list(
            EXT = "Ethiopia",
            ZONCODEVAR =  shared_parameters$ZONCODEVAR,
            ZONCODEVAL =  shared_parameters$ZONCODEVAL,
            DIVCODEVAR =  shared_parameters$DIVCODEVAR,
            DIVCODEVAL =  shared_parameters$DIVCODEVAL,
            DIVNAMEVAR =  shared_parameters$DIVNAMEVAR,
            LEVEL = shared_parameters$level,
            ACC_S_L = shared_parameters$services_local,
            ACC_S_W = shared_parameters$services_woreda,
            ACC_S_Z = shared_parameters$services_zone,
            ACC_M_L = shared_parameters$markets_local,
            ACC_M_W = shared_parameters$markets_woreda,
            ACC_M_Z = shared_parameters$markets_zone,
            INN1 =  paste0(
              shared_parameters$crop_name_1,
              "_",
              shared_parameters$ideotype_1,
              "_",
              shared_parameters$scenario_1
            )
          )
          
          shared_env <- new.env(parent = globalenv())
          rendered_params(params_acc)   # store the snapshot

          out <- rmarkdown::render(
            input = "E:/repos/raise_fs/code/rmd/accessibility_mapping_shared_environment.Rmd",
            output_file = file,
            params = params_acc,
            envir = shared_env
          )
          
          # Notify Shiny that rendering is done
          acc_render_done(TRUE)  # <-- set a reactiveVal
          
        })
      }
    )
    
    acc_render_done <- reactiveVal(FALSE)
    rendered_params <- reactiveVal(NULL)

    output$extra_ui <- renderUI({
      req(acc_render_done())
      params <- rendered_params()
      
      # Build a nice UI
            # Extract just the 6 checkbox params
      checks <- params[c("ACC_S_L","ACC_S_W","ACC_S_Z",
                         "ACC_M_L","ACC_M_W","ACC_M_Z")]
      
      # Define multiple messages per checkbox (both shown if TRUE)
      # output messages ----
      
      messages <- list(
        ACC_S_L = list(
          #         tags$span(
          "✔ Walking time to Local Services",
          tags$br(),
          "(seconds):",
          tags$span(style = "color:blue;", paste(
            "rast_cost_w_FTC_", shared_parameters$DIVCODEVAL
          )),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_FTC_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "✔ Walking and vehicle to Local Services",
          tags$br(),
          "(seconds):",
          tags$span(style = "color:blue;", paste(
            "rast_cost_v_FTC_", shared_parameters$DIVCODEVAL
          )),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_FTC_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          tags$br()
          #          )
        ),
        ACC_S_W = list(
          #          tags$span(
          "✔ Walking time to Woreda Services",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_woredaC_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_woredaC_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "✔ Walking and vehicle to Woreda Services",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_woredaC_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_woredaC_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          tags$br()
          #          )
        ),
        ACC_S_Z = list(
          #         tags$span(
          "✔ Walking time to Zonal Services",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_zoneC_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_zoneC_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "✔ Walking and vehicle to Zonal Services",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_zoneC_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_zoneC_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          tags$br()
          #         )
        ),
        ACC_M_L = list(
          #          tags$span(
          "✔ Walking time to Local Markets",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_localmarket_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_localmarket_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "✔ Walking and vehicle to Local Markets",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_localmarket_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_localmarket_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          tags$br()
          #        )
        ),
        ACC_M_W = list(
          #        tags$span(
          "✔ Walking time to Woreda Market",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_woredamarket_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_woredamarket_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "✔ Walking and vehicle to Woreda Market",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_woredamarket_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_woredamarket_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          tags$br()
          #        )
        ),
        ACC_M_Z = list(
          #        tags$span(
          "✔ Walking time to Zonal Market",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_zonemarket_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_w_zonemarket_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "✔ Walking and vehicle to Zonal Market",
          tags$br(),
          "(seconds):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_zonemarket_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          "(hours):",
          tags$span(
            style = "color:blue;",
            paste("rast_cost_v_zonemarket_hr_", shared_parameters$DIVCODEVAL)
          ),
          tags$br(),
          tags$br()
          #        )
        )
      )
      
      tags$div(
        h4("Analysis complete with these spatial data outputs:"),
        tags$ul(
          #unlist(
            lapply(names(checks), function(nm) {
              if (checks[[nm]] == 1) {
                #lapply(messages[[nm]], tags$li)  # both messages
                #tags$li(messages[[nm]])
                do.call(tags$li, messages[[nm]])  # insert text + br inside li
              } else {
                NULL
              }
            })
            #,            recursive = FALSE
          #)
        )
      )
    })
    
    # # downloadHandler run_Accfewpar_btn 
    # output$run_Accfewpar_btn  <- downloadHandler(
    #   filename = paste0(
    #     shared_values$crop_name_1,
    #     "_",
    #     shared_values$ideotype_1,
    #     "_",
    #     shared_values$scenario_1,
    #     "_accessibility.html"
    #   ),
    #   content = function(file) {
    #     withProgress(value = 0, message = 'Starting, this may take some time', {
    #       params_fewacc <- list(
    #         EXT = "Ethiopia",
    #         ZONCODEVAR =  shared_values$ZONCODEVAR,
    #         ZONCODEVAL =  shared_values$ZONCODEVAL,
    #         DIVCODEVAR =  shared_values$DIVCODEVAR,
    #         DIVCODEVAL =  shared_values$DIVCODEVAL,
    #         DIVNAMEVAR =  shared_values$DIVNAMEVAR,
    #         INN1 =  paste0(
    #           shared_values$crop_name_1,
    #           "_",
    #           shared_values$ideotype_1,
    #           "_",
    #           shared_values$scenario_1
    #         )
    #       )
    #       
    #       shared_env <- new.env(parent = globalenv())
    #       
    #       str(params_fewacc)
    #       
    #       rmarkdown::render(
    #         input = "E:/repos/raise_fs/code/rmd/accessibility_mapping_shared_environment_fewerparams.Rmd",
    #         output_file = file,
    #         params = params_fewacc
    #       )
    #     })
    #   }
    # )
    
    
    
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
    
    #2 observeEvent back_to_screen11 ----
    observeEvent(input$back_to_screen11, {
      switch_screen("screen11")
    })
    
    #2 observeEvent to_screen13 ----
    observeEvent(input$to_screen13, {
      shared_values$step <- 13
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      switch_screen("screen13")
      
    })
    
    # help button 12_01----
    observeEvent(input$show_help_12_01, {
      showModal(modalDialog(
        title = "Step 12: View or Edit Spatial Data",
        includeMarkdown("docs/step_12_01.md"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
  })
}