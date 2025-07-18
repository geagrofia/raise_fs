bslib_screen11_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  
  tagList(
    h3("Innovation 1 Summary:"),
       
       # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen10"), "Back to Screen 10"), 
    
    # selectInput(ns("faoclass1"), "Number of levels to map FAO class:", choices = c(1:3), selected = 3), 
    # selectInput(ns("limitsclass1"), "Number of levels to map limitations:", choices = c(1:3), selected = 3), 
    # selectInput(ns("concclass1"), "Number of levels to map limitations:", choices = c(1:3), selected = 3), 
    uiOutput(ns("dynamic_run_inn2"))
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
    DTOutput(ns("summary_table"))
  )
}

bslib_screen11_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NA_weights <- 0
    
    message("S11. session$ns")
    
    # Load the initial data ----
    initial_summary_data <- reactive({
      req(switch_screen() == "screen11")
      message(paste("S11. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_values$crop_name_1,
        "_",
        shared_values$ideotype_1,
        "_",
        shared_values$scenario_1,
        "_requirements_s10.csv"
      ))) {
        
        df_requirements_s10 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_requirements_s10.csv"
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
        df_requirements_s10_leaves <- left_join(df_leaves_codes,
                                               df_requirements_s10,
                                               by = c("crit_code"))
        
        message("S11. df_requirements_s10_leaves")
        print(df_requirements_s10_leaves)
        
        df_requirements_s10_leaves <- df_requirements_s10_leaves |> dplyr::select(
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
        dt <- as.data.table(df_requirements_s10_leaves)
        
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
        
        message("S11. print(dt)")
        print(dt)
        
        # dt is now ready to display in shiny
        dt
        
      } else {
        NULL
      }
      
    })
    
    # Store the table reactively ----
    summary_table_data  <- reactiveVal()
    #req(initial_spatial_data())  # only proceed if non-NULL
    current_summary_data <- reactive({
      summary_table_data()
    })
    
    observeEvent(initial_summary_data(), {
      req(initial_summary_data())  # only proceed if non-NULL
      summary_table_data(initial_summary_data())
    })
    
    # Render the editable data table----
    output$summary_table <- DT::renderDT({
      
        # define a set of colors for each group
        group_colors <- c("#f2f2f2", "#e0f7fa", "#ffe0b2", "#dcedc8", "#f8bbd0")
        # repeat colors if there are more groups than colors
        group_colors <- rep(group_colors, length.out = max(current_summary_data()$group_id))
        
        message("S11. print(jsonlite::toJSON(group_colors))")
        print(jsonlite::toJSON(group_colors))
        
        message("S11. print(jsonlite::toJSON(current_summary()$group_id))")
        print(jsonlite::toJSON(current_summary_data()$group_id))
        
        message("S11. print(current_summary())")
        print(current_summary_data())
        
        # this version does not try to highlight groups
        
        DT::datatable(
          current_summary_data()[, !"group_id"],
          # hide group_id from view
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          editable = FALSE,
          options = list(lengthMenu = c(10, 20, 50), pageLength = 20)
        )
    })


    
    # outputs from previous screens----
    
    output$num_innovations_display <- renderText({
      paste("S11. Number of innovations:", shared_values$num_innovations)
    })
    
    output$innovation_system_display <- renderText({
      if (shared_values$num_innovations == "two_inn") {
        paste("S11. Innovation System:", shared_values$innovation_system)
      }
    })    
    
    output$spatres_display <- renderText({
      paste("S11. Your spatial resolution is:", shared_values$resolution)
    })
    
    
    output$aggregation_display <- renderText({
      paste("S11. Your aggregation level is:", shared_values$aggregation)
    })
    
    output$level_display <- renderText({
      req(shared_values$level)
      paste("S11. You selected level on Screen 1:", shared_values$level)
    })
    
    output$selection_display <- renderText({
      req(shared_values$level)
      
      if (shared_values$level == "woreda") {
        paste(
          "S11. You selected geography:",
          shared_values$selected_region,
          shared_values$selected_zone,
          shared_values$selected_woreda
        )
      } else {
        if (shared_values$level == "zone") {
          paste(
            "S11. You selected geography:",
            shared_values$selected_region,
            shared_values$selected_zone
          )
        } else {
          if (shared_values$level == "region") {
            paste("S11. You selected geography:",
                  shared_values$selected_region)
          } else {
            paste("S11. You selected geography: Ethiopia")
          }
        }
      }
    }) 
    
    output$crop_1_display <- renderText({
      message(paste("S11. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S11. You selected crop on Screen 4:", shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S11. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S11. You selected ideotype on Screen 4:", shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S11. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S11. You selected scenario on Screen 4:", shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S11. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S11. You selected Innovation type on Screen 4:", shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    # observeEvent back_to_screen10 ----
    observeEvent(input$back_to_screen10, {
      switch_screen("screen10")
    })
    
    # dynamic run inn2 controls ----
    output$dynamic_run_inn2 <- renderUI({
      
      print(paste("num_innovations is", shared_values$num_innovations))
      
      if (shared_values$num_innovations == "one_inn") {
        #tagList(actionButton(ns("run_IRM1_btn"), "Run IRM"))
        tagList(downloadButton(ns("run_IRM1_btn"), "Run IRM"))
      } else {
        tagList(actionButton(ns("inn2_btn"), "Choose and Review Innovation 2"))
      }
      
    })
    
    # # downloadHandler run_IRM1_btn ----
    # output$run_IRM1_btn  <- downloadHandler(
    #   # filename = paste0(
    #   #   "E:/repos/raise_fs/code/rmd/",
    #   #   shared_values$crop_name_1,
    #   #   "_",
    #   #   shared_values$ideotype_1,
    #   #   "_",
    #   #   shared_values$scenario_1,
    #   #   ".html"
    #   # ),
    #   filename = function() {
    #     paste0("report_", Sys.Date(), ".html")
    #   },
    #   content = function(file) {
    #     # Set up parameters to pass to Rmd document
    #     withProgress(value = 0, message = 'Starting, this may take some time', {
    #       params_irm <- list(
    #         INT = "1",
    #         SYS = NA,
    #         Agg = shared_values$aggregation(),
    #         MASK = shared_values$resolution(),
    #         EXT = "Ethiopia",
    #         DIVCODEVAR =  shared_values$DIVCODEVAR,
    #         DIVCODEVAL =  shared_values$DIVCODEVAL,
    #         DIVNAMEVAR =  shared_values$DIVNAMEVAR,
    #         SUBDIVNAMEVAR =  shared_values$SUBDIVNAMEVAR,
    #         INN1 =  paste(
    #           shared_values$crop_name_1,
    #           "_",
    #           shared_values$ideotype_1,
    #           "_",
    #           shared_values$scenario_1
    #         ),
    #         RES1 =  3,
    #         SOS1 =  shared_values$sos1,
    #         FAOCLASS1 =  3,
    #         LIMITS1 =  3,
    #         CONCCLASS1 =  3,
    #         TRIAD1 =  Adoption,
    #         TRIBA1 =  Aptitude,
    #         TRISE1 =  Feasible
    #       )
    #       
    #       message("S11. params_irm")
    #       print(params_irm)
    #       
    #       rmarkdown::render(
    #         input = "E:/repos/raise_fs/code/rmd/irm_interface.rmd",
    #         output_file = file,
    #         params = params_irm,
    #         envir = new.env(parent = globalenv())
    #       )
    #     })
    #   }
    # )

    # downloadHandler run_IRM1_btn ----
    output$run_IRM1_btn  <- downloadHandler(
      # filename = function() {
      #   paste0("report_", Sys.Date(), ".html")
      # },
      filename = paste0(
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            ".html"
          ),
      content = function(file) {
        withProgress(value = 0, message = 'Starting, this may take some time', {
                params_irm <- list(
                  INT = "1",
                  SYS = NA,
                  Agg = shared_values$aggregation,
                  MASK = shared_values$resolution,
                  EXT = "Ethiopia",
                  DIVCODEVAR =  shared_values$DIVCODEVAR,
                  DIVCODEVAL =  shared_values$DIVCODEVAL,
                  DIVNAMEVAR =  shared_values$DIVNAMEVAR,
                  SUBDIVNAMEVAR =  shared_values$SUBDIVNAMEVAR,
                  INN1 =  paste0(
                    shared_values$crop_name_1,
                    "_",
                    shared_values$ideotype_1,
                    "_",
                    shared_values$scenario_1
                  ),
                  RES1 =  3,
                  SOS1 =  shared_values$sos1,
                  FAOCLASS1 =  3,
                  LIMITS1 =  3,
                  CONCCLASS1 =  3,
                  TRIAD1 =  "Adoption",
                  TRIBA1 =  "Aptitude",
                  TRISE1 =  "Feasible"
                )
          
        rmarkdown::render(
          input = "E:/repos/raise_fs/code/rmd/report_template.Rmd",
          output_file = file,
          params = params_irm,
          envir = new.env(parent = globalenv())
        )
        })
      }
      )
    
    # observeEvent inn2_btn ----
    observeEvent(input$inn2_btn, {
      switch_screen("screen12")
    })

      
    })
}