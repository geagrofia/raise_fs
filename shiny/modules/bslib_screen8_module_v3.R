bslib_screen8_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(
    #     ns("back_to_screen7"),
    #     label = tagList(
    #       icon("circle-left"),
    #       # icon first
    #       "Back to Screen 7"
    #       # text second
    #     ),
    #     class = "btn-primary"
    #   ),
    #   actionButton(
    #     ns("to_screen9"),
    #     label = tagList(
    #       "Go to Screen 9",
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
          ns("show_help_08_01"),
          title = "Help for Step 8",
          label = tagList(
            icon("circle-question")  # icon second)
          ),
          style = "background: rgba(23, 162, 184, 0.5);"
        )
        
      ),
      div(
        style = "display: inline-block; vertical-align: middle; margin-left: 10px;",
    h4("Step 8: View or Edit Rule Base Thresholds, Propositions and Conclusions")
    ),
    scrollable_DT(ns("prop_conc_data_table")),
    uiOutput(ns("dyanamic_save_reset"))
    
    )
  )
}

bslib_screen8_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen7"), 
                   title = "Go back to Step 7: View or Edit Rule Base Weights",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 8", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen9"), 
                   title = "Go to Step 9: View or Edit Crop Growth Stages",
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

bslib_screen8_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NA_weights <- 0
    
    disable("to_screen9")
    
    # Load the initial data ----
    initial_prop_conc_data <- reactive({
      req(switch_screen() == "screen8")
      message(paste("S8. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_parameters$crop_name_1,
        "_",
        shared_parameters$ideotype_1,
        "_",
        shared_parameters$scenario_1,
        "_requirements_s7.csv"
      ))) {
        
        message(
          paste(
            "S8. Initiation. inn details1:",
            shared_parameters$crop_name_1,
            "-",
            shared_parameters$ideotype_1,
            "-",
            shared_parameters$scenario_1
          )
        )
        df_requirements_s7 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_requirements_s7.csv"
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
        df_requirements_s7_leaves <- left_join(df_leaves_codes,
                                               df_requirements_s7,
                                              by = c("crit_code"))
        
        message("S8. df_requirements_s7_leaves")
        print(df_requirements_s7_leaves)
        
        df_requirements_s7_leaves <- df_requirements_s7_leaves |> dplyr::select(
          c(
            "crit_code",
            "criterion",
            "threshold_1",
            "threshold_2",
            "width_1",
            "width_2",
            "thresh_source",
            "prop_level_1",
            "prop_level_2",
            "prop_level_3",
            "conc_level_1",
            "conc_level_2",
            "conc_level_3",
            "fuzzy_partition"
          )
        )
        
        # convert your data.frame to data.table
        dt <- as.data.table(df_requirements_s7_leaves)
        
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
        
        message("S8. print(dt)")
        print(dt)
        
        # dt is now ready to display in shiny
        dt
        
      } else {
        NULL
      }
      
    })
    
    # Store the table reactively ----
    prop_conc_table_data  <- reactiveVal()
    #req(initial_prop_conc_data())  # only proceed if non-NULL
    current_prop_conc_data <- reactive({
      prop_conc_table_data()
    })
    
    observeEvent(initial_prop_conc_data(), {
      req(initial_prop_conc_data())  # only proceed if non-NULL
      prop_conc_table_data(initial_prop_conc_data())
    })
    
    # Render the editable data table----
    output$prop_conc_data_table <- DT::renderDT({
      if (shared_values$inn_type_1 == "existing") {
        
        # define a set of colors for each group
        group_colors <- c("#f2f2f2", "#e0f7fa", "#ffe0b2", "#dcedc8", "#f8bbd0")
        # repeat colors if there are more groups than colors
        group_colors <- rep(group_colors, length.out = max(current_prop_conc_data()$group_id))
        
        message("S8. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S8. print(jsonlite::toJSON(current_prop_conc_data()$group_id))")
        print(jsonlite::toJSON(current_prop_conc_data()$group_id))
        
        message("S8. print(current_prop_conc_data())")
        print(current_prop_conc_data())
        
        # this version does not try to highlight groups
        
        DT::datatable(
          current_prop_conc_data()[, !"group_id"],
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
        #       current_prop_conc_data()[, !"group_id"],
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
        #             jsonlite::toJSON(current_prop_conc_data()$group_id),
        #             jsonlite::toJSON(group_colors)
        #           )
        #         )
        #       )
        #     )
      } else {
        
        
        
        # define a set of colors for each group
        group_colors <- c("#f2f2f2", "#e0f7fa", "#ffe0b2", "#dcedc8", "#f8bbd0")
        # repeat colors if there are more groups than colors
        group_colors <- rep(group_colors, length.out = max(current_prop_conc_data()$group_id))
        
        message("S8. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S8. print(jsonlite::toJSON(current_prop_conc_data()$group_id))")
        print(jsonlite::toJSON(current_prop_conc_data()$group_id))
        
        message("S8. print(current_prop_conc_data())")
        print(current_prop_conc_data())
        
        # this version does not try to highlight groups   
        
        # DT::datatable(
        #   current_prop_conc_data()[, !"group_id"],
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
          current_prop_conc_data()[, !"group_id"],
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
                jsonlite::toJSON(current_prop_conc_data()$group_id)
              )
            )
          )
        )
      }
    })
    
    observeEvent(input$prop_conc_data_table_cell_edit, {
      message("S8. str(input$prop_conc_data_table_cell_edit)")
      str(input$prop_conc_data_table_cell_edit)
    })
    
    
    #observeEvent Update table on cell edit----
    observeEvent(input$prop_conc_data_table_cell_edit, {
      info <- input$prop_conc_data_table_cell_edit
      
      message("S8. str(input$prop_conc_data_table_cell_edit)")
      str(input$prop_conc_data_table_cell_edit)
      
      dt <- copy(current_prop_conc_data())
      message("S8. 1 str(dt)")
      str(dt)
      
      colname <- names(dt)[ info$col + 1 ]  # adjust for 0-based to 1-based
      message("S8. colname")
      print(colname)
      
      old_class <- class(dt[[colname]])
      message("S8. old_class")
      print(old_class)
      
      coerced_val <- tryCatch({
        as(info$value, old_class)
      }, error = function(e) info$value)
      
      dt[info$row, (colname) := coerced_val]
      message("S8. 2 str(dt)")
      str(dt)
      
      prop_conc_table_data(dt)
    })
    
    
    # table controls
    output$dyanamic_save_reset <- renderUI({
      tagList(
        actionButton(ns("save_btn_8"), "Save table",
                     class = "btn-primary"),
        actionButton(ns("reset_btn_8"), "Reset table",
                     class = "btn-primary")
      )
    })
    
    
    # observeEvent save button----
    observeEvent(input$save_btn_8, {
      req(current_prop_conc_data())
      dt <- prop_conc_table_data()
      
      message("S8. 2 str(dt)")
      str(dt)
      
      problems_S8 <- c()
      message("S8. problems_S8")
      print(problems_S8)
      
      # problems_S8 that must be flagged:
      
      # (1) different number of thresholds and widths
      # (2) different number of thresholds and propositions
      # (3) different number of thresholds and conclusions
      
      # if col4 present then col6, col10, col13 must be present
      idx <- which(!is.na(dt[[4]]) & dt[[4]] != "")
      
      message("S8. idx")
      print(idx)
      
      for (i in idx) {
        if (is.na(dt[i, 6]) | is.na(dt[i, 10]) | is.na(dt[i, 13])) {
          problems_S8 <- c(problems_S8,
                        sprintf("Row %d: if col4 present then col6, col10, col13 required", i))
        }
      }
      
      # (4) overlapping thresholds
      
      # (5) missing thresholds
      # (6) missing widths
      # (7) missing threshold source
      # (8) missing propositions
      # (9) missing conclusions
      # (10) missing fuzzy partition type
      
      # columns 3,5,7,8,9,11,12,14 cannot be empty
      mandatory <- c(3, 5, 7, 8, 9, 11, 12, 14)
      for (col in mandatory) {

        if (any(is.na(dt[[col]]) | dt[[col]] == "")) {
          problems_S8 <- c(problems_S8, sprintf("Column %d has missing values", col))
        }
      }
      
      # (11) thresholds and widths must be numeric
      
      # numeric constraint
      for (i in 1:nrow(dt)) {
        if (!is.na(dt[i, 4]))
        {
          message("S8. numeric constraint")
          
          c3 <- as.numeric(dt[i, 3])
          c4 <- as.numeric(dt[i, 4])
          c5 <- as.numeric(dt[i, 5])
          c6 <- as.numeric(dt[i, 6])
          
          message("S8. c numeric")
          print(i)
          print(c3)
          print(c4)
          print(c5)
          print(c6)
          
          print((c3 + (c5 / 2)))
          print((c4 - (c6 / 2)))
          
          if ((c3 + (c5 / 2)  - 1e-8 ) > (c4 - (c6 / 2))) {
            message("S8. (c3 + (c5 / 2) - 1e-8) > (c4 - (c6 / 2))")
            print((c3 + (c5 / 2) - 1e-8 ) - (c4 - (c6 / 2)))
            print((c3 + (c5 / 2) - 1e-8 ) > (c4 - (c6 / 2)))
            
            problems_S8 <- c(problems_S8,
                          sprintf("Row %d fails col3 + col5/2 < col4 - col6/2", i))
          }
        }
      }
      
      
      # (11) propositions must be distinct
      message("S8. 8-10 text must be distinct")
      # columns 8-10 text must be distinct
      for (i in 1:nrow(dt)) {
        vals <- dt[i, 8:10]
        if (length(unique(vals)) != 3) {
          problems_S8 <- c(problems_S8, sprintf("Row %d cols 8-10 must be distinct", i))
        }
      }
      
      # (12) conclusions must use certain combinations
      # rcolumns 11–13 in allowed combinations
      allowed_patterns <- c(
        "optimal-suboptimal-",
        "optimal-suboptimal-optimal",
        "suboptimal-optimal-suboptimal",
        "suboptimal-optimal-",
        "high-low-",
        "low-high-",
        "high-moderate-low",
        "low-moderate-high",
        "poor-good-",
        "good-poor-",
        "poor-moderate-good",
        "good-moderate-poor"
      )
      
      message("S8. allowed_patterns")
      print(allowed_patterns)
      
      for (i in 1:nrow(dt)) {
        
        pattern <- paste(dt[i, 11], dt[i, 12], dt[i, 13], sep = "-")
        if (!(pattern %in% allowed_patterns)) {
          problems_S8 <- c(problems_S8,
                        sprintf("Row %d invalid 11-13 combination: %s", i, pattern))
        }
      }
      
      # (11) fuzzy partitions type must use certain value
      # column 14 must be linear or zadeh
      bad14 <- which(!(dt[[14]] %in% c("linear", "zadeh")))
      
      message("S8. bad14")
      print(bad14)
      
      if (length(bad14) > 0) {
        problems_S8 <- c(problems_S8, sprintf("Rows %s col14 invalid", paste(bad14, collapse =
                                                                         ",")))
      }
      
      if (length(problems_S8) > 0) {
        
        message("S8. (length(problems_S8) > 0)")
        print(problems_S8)
        
        removeModal()
        showModal(
          modalDialog(
            title = "Error", 
            paste(problems_S8, collapse = "<br>"), 
            easyClose = TRUE))
        
    } else {
      
      message("S8. save logic")
      
     # overwrite and produce a new version of the requirements table
      
      df_inn_requirements_s7 <- read.csv(
        paste0(
          "E:/repos/raise_fs/shiny/data/", shared_parameters$crop_name_1, "_", shared_parameters$ideotype_1, "_", shared_parameters$scenario_1, "_requirements_s7.csv"
        )
      )
      
      # saves the update requirements table separately
      #df_inn_requirements_s6 <- merge(dt, df_inn_requirements_s5,  by = "crit_code")  # seems self-documenting
      
      df_inn_requirements_s8 <- df_inn_requirements_s7 |>
        rows_update(dplyr::select(dt, -c("group_id")), by = "crit_code")
      
      
      fwrite(
        df_inn_requirements_s8, file = paste0(
          "E:/repos/raise_fs/shiny/data/", shared_parameters$crop_name_1, "_", shared_parameters$ideotype_1, "_", shared_parameters$scenario_1, "_requirements_s8.csv"
        )
      )
      
      enable("to_screen9")
      
      removeModal()
      showModal(modalDialog(
        title = "Saved",
        "Thresholds, Propositions and Conclusions Table saved successfully.",
        easyClose = TRUE))
    }
      })
    
    
    # observeEvent reset button----
    observeEvent(input$reset_btn_8, {
      #req(initial_prop_conc_data())  # only proceed if non-NULL
      prop_conc_table_data(initial_prop_conc_data())
    })
    
    
    
    
    
    
    
    # 
    # 
    # #observeEvent Update table on cell edit----
    # 
    # observeEvent(input$prop_conc_data_table_cell_edit, {
    #   
    #   print(str(input$prop_conc_data_table_cell_edit))
    #   
    #   message(paste("observeEvent Update table on cell edit"))
    #   info <- input$prop_conc_data_table_cell_edit
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
    
    #2 observeEvent back_to_screen7 ----
    observeEvent(input$back_to_screen7, {
      switch_screen("screen7")
    })
    
    #2 observeEvent to_screen9 ----
    observeEvent(input$to_screen9, {
      shared_values$step <- 9
      save_progress(shared_values, shared_parameters)
      switch_screen("screen9")
      
    })
    
    
    # help button 08_01----
    observeEvent(input$show_help_08_01, {
      showModal(modalDialog(
        title = "Step 8: View or Edit Rule Base Thresholds, Propositions and Conclusions",
        includeMarkdown("docs/step_08_01.md"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
  })
}