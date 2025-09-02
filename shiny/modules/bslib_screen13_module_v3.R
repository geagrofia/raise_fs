bslib_screen13_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  
  tagList(
    h3("Innovation 1 Summary:"),
    checkboxInput(ns("triangulation"), "Does this innovation have triangulation data from local experts?", value = FALSE),
    
       # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen12"), "Back to Screen 12"), 
    
    # selectInput(ns("faoclass1"), "Number of levels to map FAO class:", choices = c(1:3), selected = 3), 
    # selectInput(ns("limitsclass1"), "Number of levels to map limitations:", choices = c(1:3), selected = 3), 
    # selectInput(ns("concclass1"), "Number of levels to map limitations:", choices = c(1:3), selected = 3), 
    uiOutput(ns("dynamic_run_inn2"))
    )
}

bslib_screen13_module_v3_MainUI <- function(id) {
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

bslib_screen13_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NA_weights <- 0
    
    message("S13. session$ns")
    
    # Load the initial data ----
    initial_summary_data <- reactive({
      req(switch_screen() == "screen13")
      message(paste("S13. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_values$crop_name_1,
        "_",
        shared_values$ideotype_1,
        "_",
        shared_values$scenario_1,
        "_requirements_s12.csv"
      ))) {
        
        df_requirements_s12 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1,
            "_requirements_s12.csv"
          )
        )


        # convert your data.frame to data.table
        dt <- as.data.table(df_requirements_s12)
        
        message("S13. print(dt)")
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
      
        message("S13. print(current_summary())")
        print(current_summary_data())
        
        # this version does not try to highlight groups
        
        DT::datatable(
          current_summary_data(),
          # hide group_id from view
          rownames = F,
          filter = "bottom",
          selection = list(mode = "none"),
          extensions = c('FixedColumns', 'FixedHeader'),
          editable = FALSE,
          options = list(lengthMenu = c(10, 20, 50), 
                         pageLength = 10, 
                         scrollX = TRUE,
                         fixedHeader= TRUE,
                         fixedColumns = list(leftColumns = 2))
        )
    })


    
    # observe parameters ----
    observe({
      message("S13. observe parameters")
      shared_values$triangulation <- ifelse(input$triangulation, 1, 0)
      message("S12. shared_values$triangulation")
      print(shared_values$triangulation)
    })
    
    # outputs from previous screens----
    
    output$num_innovations_display <- renderText({
      paste("S13. Number of innovations:", shared_values$num_innovations)
    })
    
    output$innovation_system_display <- renderText({
      if (shared_values$num_innovations == "two_inn") {
        paste("S13. Innovation System:", shared_values$innovation_system)
      }
    })    
    
    output$spatres_display <- renderText({
      paste("S13. Your spatial resolution is:", shared_values$resolution)
    })
    
    
    output$aggregation_display <- renderText({
      paste("S13. Your aggregation level is:", shared_values$aggregation)
    })
    
    output$level_display <- renderText({
      req(shared_values$level)
      paste("S13. You selected level on Screen 1:", shared_values$level)
    })
    
    output$selection_display <- renderText({
      req(shared_values$level)
      
      if (shared_values$level == "woreda") {
        paste(
          "S13. You selected geography:",
          shared_values$selected_region,
          shared_values$selected_zone,
          shared_values$selected_woreda
        )
      } else {
        if (shared_values$level == "zone") {
          paste(
            "S13. You selected geography:",
            shared_values$selected_region,
            shared_values$selected_zone
          )
        } else {
          if (shared_values$level == "region") {
            paste("S13. You selected geography:",
                  shared_values$selected_region)
          } else {
            paste("S13. You selected geography: Ethiopia")
          }
        }
      }
    }) 
    
    output$crop_1_display <- renderText({
      message(paste("S13. crop details:", shared_values$crop_name_1))
      req(shared_values$crop_name_1)
      paste("S13. You selected crop on Screen 4:", shared_values$crop_name_1)
    })
    
    
    output$ideotype_1_display <- renderText({
      message(paste("S13. ideotype details:", shared_values$ideotype_1))
      req(shared_values$ideotype_1)
      paste("S13. You selected ideotype on Screen 4:", shared_values$ideotype_1)
    })
    
    
    output$scenario_1_display <- renderText({
      message(paste("S13. scenario details:", shared_values$scenario_1))
      req(shared_values$scenario_1)
      paste("S13. You selected scenario on Screen 4:", shared_values$scenario_1)
    })
    
    
    output$inn_type_1_display <- renderText({
      message(paste("S13. Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("S13. You selected Innovation type on Screen 4:", shared_values$inn_type_1)
    })
    
    # _ navigation----
    
    # observeEvent back_to_screen12 ----
    observeEvent(input$back_to_screen12, {
      switch_screen("screen12")
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
    
    # # downloadHandler run_IRM1_btn 
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
    #       message("S13. params_irm")
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
            TRIANGULATION = shared_values$triangulation,
            TRILIMITS1 =  "LIMITS",
            TRIDEC1 =  "DECISION",
            TRISUIT1 =  "SUITABILITY",
            TRIPCS1 =  "PC_SUIT",
            INN2 = NA
          )
          
          # rmarkdown::render(
          #   input = "E:/repos/raise_fs/code/rmd/report_template.Rmd",
          #   output_file = file,
          #   params = params_irm,
          #   envir = results_env
          # )
          
          # shared environment
          # Create a clean environment shared between helpers.R and report.Rmd
          shared_env <- new.env(parent = globalenv())
          
          # Render using that environment
          rmarkdown::render(
            input = "E:/repos/raise_fs/code/rmd/report_template_shared_environment.Rmd",
            output_file = file,
            params = params_irm,
            envir = shared_env
          )
        
        
        })
      }
      )
    
    # observeEvent inn2_btn ----
    observeEvent(input$inn2_btn, {
      switch_screen("screen14")
    })

      
    })
}