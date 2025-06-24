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
    
    # Render the data table----
    output$conclusions_data_table <- renderDT({
      req(shared_values$inn_type_1)
      req(switch_screen())
      message(
        paste(
          "Initiation. inn details1:",
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
        message("df_new_codes_stackname")
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
        message("df_new_codes_stackname")
        print(df_new_codes_stackname)
        
        #df_new_codes_stackname <- mutate(df_new_codes_stackname, criterion == stack)
        df_new_codes_stackname <- df_new_codes_stackname |> rename(criterion = stack)
        message("df_new_codes_stackname")
        print(df_new_codes_stackname)
        
        
        # # Create new rows with NA or default values
        # df_new_rows <- data.frame(
        #   crit_code = new_codes,
        #   criterion = NA,
        #   weight = NA,
        #   threshold_1 = NA,
        #   threshold_2 = NA,
        #   width_1 = NA,
        #   width_2 = NA,
        #   thresh_source = NA,
        #   data_desc = NA,
        #   data_file_prefix = NA,
        #   raster_or_brick = NA,
        #   agg_fun = NA,
        #   rsm_fun = NA,
        #   prop_level_1 = NA,
        #   prop_level_2 = NA,
        #   prop_level_3 = NA,
        #   conc_level_1 = NA,
        #   conc_level_2 = NA,
        #   conc_level_3 = NA,
        #   fuzzy_partition = NA,
        #   yield = NA,
        #   phen_stage = NA,
        #   temp_resolution = NA,
        #   prec_temp = NA,
        #   texture = NA
        # )
        
        
        
        
        # Append new rows to df_inn_requirements
        df_inn_requirements_updated <- rbind(df_inn_requirements, df_new_codes_stackname)
      } else { # if there are no new codes ----
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
      print(str(df_inn_conc))
      print(df_inn_conc)
      
      # if existing innovation then not editable
      if (shared_values$inn_type_1 == "existing") {
        datatable(
          df_inn_conc,
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          editable = FALSE,
          # only edit the conclusions
          options = list(
            # columnDefs = list(list(
            #   visible = FALSE, targets = c(0) # hide the inn_ID
            # )),
            lengthMenu = c(10, 20, 50),
            pageLength = 20#,
            #order = list(list(1, 'asc'), list(2, 'asc'), list(3, 'asc'))
          )
        )
        
      } else {
        datatable(
          df_inn_conc,
          rownames = F,
          filter = "bottom",
          selection = list(mode = "single"),
          editable = list(target = "cell", disable = list(columns = c(0, 1))),
          # only edit the conclusions
          options = list(
            # columnDefs = list(list(
            #   visible = FALSE, targets = c(0) # hide the inn_ID
            # )),
            lengthMenu = c(10, 20, 50),
            pageLength = 20#,
            #order = list(list(1, 'asc'), list(2, 'asc'), list(3, 'asc'))
          )
        )
      }
      
      
    }, server = FALSE)
     
    # render the buttons UI output ----
    output$dyanamic_save_reset <- renderUI({
      # observe whether conclusions are filled to determine button visibility----
      if (1 == 2) {
        
        
        
        
        #
      } else {
        tagList(
          actionButton(ns("save_dyn_button"), "Save Changes / Re-load"), # Button to save changes
          actionButton(ns("reset_dyn_button"), "Reset to Original")) # Button to reset to original
      }
    })
    
    
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
      message(paste("crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("You selected crop on Screen 4:", shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("You selected ideotype on Screen 4:", shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("You selected scenario on Screen 4:", shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("You selected Innovation type on Screen 4:", shared_values$inn_type_1)
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