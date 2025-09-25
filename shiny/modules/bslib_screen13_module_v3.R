bslib_screen13_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(
    #     ns("back_to_screen12"),
    #     label = tagList(
    #       icon("circle-left"),
    #       # icon first
    #       "Back to Screen 12"
    #       # text second
    #     ),
    #     class = "btn-primary"
    #   ),
    #   actionButton(
    #     ns("add_inn"),
    #     label = tagList(
    #       "Add innovation",
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
          ns("show_help_13_01"),
          title = "Help for Step 13",
          label = tagList(
            icon("circle-question")  # icon second)
          ),
          style = "background: rgba(23, 162, 184, 0.5);"
        )
        
      ),
      div(
        style = "display: inline-block; vertical-align: middle; margin-left: 10px;",
      h4("Step 13: Run IRM or start Innovation 2")
      ),
      checkboxInput(ns("triangulation"), "Does this innovation have triangulation data from local experts?", value = FALSE),
      #,
      # selectInput(ns("faoclass1"), "Number of levels to map FAO class:", choices = c(1:3), selected = 3), 
      # selectInput(ns("limitsclass1"), "Number of levels to map limitations:", choices = c(1:3), selected = 3), 
      # selectInput(ns("concclass1"), "Number of levels to map limitations:", choices = c(1:3), selected = 3), 
      uiOutput(ns("dynamic_run_inn2"))
    ),
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      h4("Review Innovation 1"),
      scrollable_DT(ns("summary_table"))
    )
  )
}

bslib_screen13_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(
        ns("back_to_screen12"),
        title = "Go back to Step 12: View or Edit Spatial Data",
        label = tagList(
          icon("circle-left"),
          # icon first
          #"Go to Introduction"
          "Back"
          # text second
        ),
        class = "btn-primary"
      ),
      
      tags$span(
        tagList("Step 13", icon("location-crosshairs")),
        # text + icon
        class = "btn btn-info disabled"
      ),
      uiOutput(ns("dynamic_navbutn"))
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
      textOutput(ns("inn_type_1_display")),
      textOutput(ns("tri_1_display"))
    )
  )
}

bslib_screen13_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NA_weights <- 0
    
    # Load the initial data ----
    initial_summary_data <- reactive({
      req(switch_screen() == "screen13")
      message(paste("S13. switch screen()", switch_screen()))
      
      if (file.exists(paste0(
        "E:/repos/raise_fs/shiny/data/",
        shared_parameters$crop_name_1,
        "_",
        shared_parameters$ideotype_1,
        "_",
        shared_parameters$scenario_1,
        "_requirements_s12.csv"
      ))) {
        
        df_requirements_s12 <- read.csv(
          paste0(
            "E:/repos/raise_fs/shiny/data/",
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
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
          options = list(scrollX = TRUE,
            lengthMenu = c(10, 20, 50), 
                         pageLength = 10, 
                         scrollX = TRUE,
                         fixedHeader= TRUE,
                         fixedColumns = list(leftColumns = 2))
        )
    })


    
    # observe parameters ----
    observe({
      #req(input$triangulation)
      #message("S13. observe parameters")
      shared_parameters$triangulation <- ifelse(input$triangulation, 1, 0)
      #message("S13. shared_parameters$triangulation")
      #print(shared_parameters$triangulation)
    })
    
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
    
    output$tri_1_display <- renderText({
      req(shared_parameters$triangulation)
      paste("Step 13. Triangulation =", shared_parameters$triangulation)
    })
    
    
    # _ navigation----
    
    # observeEvent back_to_screen12 ----
    observeEvent(input$back_to_screen12, {
      switch_screen("screen12")
    })
    
    # observeEvent add_inn ----
    observeEvent(input$add_inn, {
      
      req(input$add_inn)
      # if the innovation is not already in the database add a row
      if (shared_values$inn_type_1 != "existing") {
        
        cat("S13. ADD INN BUTTON\n\n")
        
        df_inn_old <- read.csv("E:/repos/raise_fs/shiny/data/innovations.csv")
        
        cat("\n\ndf_inn_old 1\n\n")
        print(df_inn_old)  
        
        new_inn_id <- (max(df_inn_old$inn_ID) + 1)
        
        cat("\n\nnew_inn_id 1\n\n")
        print(new_inn_id)          

        new_crop <- shared_parameters$crop_name_1
        
        cat("\n\nnew_crop\n\n")
        print(new_crop)
        
        new_ideotype <- shared_parameters$ideotype_1
        
        cat("\n\nnew_ideotype\n\n")
        print(new_ideotype)  
        
        new_scenario <- shared_parameters$scenario_1
        
        cat("\n\nnew_scenario\n\n")
        print(new_scenario)  
        
        df_inn_old <- df_inn_old |> add_row(
          inn_ID = new_inn_id,
          crop_name = new_crop,
          ideotype = new_ideotype,
          scenario = new_scenario
        )
        
        cat("\n\ndf_inn_old 2\n\n")
        print(df_inn_old)
        
        df_inn_old |> write.csv("E:/repos/raise_fs/shiny/data/innovations.csv", row.names = FALSE)

      }
    })
    
    # dynamic run inn2 controls ----
    output$dynamic_run_inn2 <- renderUI({
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      
      print(paste("num_innovations is", shared_parameters$num_innovations))
      
      if (shared_parameters$num_innovations == "one_inn") {
        #tagList(actionButton(ns("run_IRM1_btn"), "Run IRM"))
        tagList(downloadButton(ns("run_IRM1_btn"), "Run IRM",
                               class = "btn-primary"))
      } else {
        tagList(
          span(
            "Choose and Review Innovation 2",
            style = "color:red; font-size: 12;"
          )
        )
      }
      
    })
    
    
    # dynamic dynamic_navbutn controls ----
    output$dynamic_navbutn <- renderUI({
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      
      print(paste("num_innovations is", shared_parameters$num_innovations))
      
      if (shared_parameters$num_innovations == "one_inn") {
        
      } else {
        actionButton(
          ns("to_screen14"),
          title = "Go to Step 14: Select Innovation 2",
          label = tagList(
            #"Go to Screen 2",
            "Next",
            # text first
            icon("circle-right")  # icon second)
          ),
          class = "btn-primary"
        )
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
            shared_parameters$crop_name_1,
            "_",
            shared_parameters$ideotype_1,
            "_",
            shared_parameters$scenario_1,
            "_",
            shared_parameters$resolution,
            "_",
            shared_parameters$DIVCODEVAL,
            ".html"
          ),
      content = function(file) {
        withProgress(value = 0, message = 'Starting, this may take some time', {
          params_irm <- list(
            INT = "1",
            SYS = NA,
            Agg = shared_parameters$aggregation,
            MASK = shared_parameters$resolution,
            EXT = "Ethiopia",
            DIVCODEVAR =  shared_parameters$DIVCODEVAR,
            DIVCODEVAL =  shared_parameters$DIVCODEVAL,
            DIVNAMEVAR =  shared_parameters$DIVNAMEVAR,
            SUBDIVNAMEVAR =  shared_parameters$SUBDIVNAMEVAR,
            INN1 =  paste0(
              shared_parameters$crop_name_1,
              "_",
              shared_parameters$ideotype_1,
              "_",
              shared_parameters$scenario_1
            ),
            RES1 =  3,
            SOS1 =  shared_parameters$sos1,
            FAOCLASS1 =  3,
            LIMITS1 =  3,
            CONCCLASS1 =  3,
            TRIANGULATION = shared_parameters$triangulation,
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
    # observe triangulation ----
    # observe({
    #   req(input$triangulation)
    #   message(paste("S3. 1 observe: radio buttons", shared_parameters$innovation_system))
    #   shared_parameters$num_innovations <- input$num_innovations
    #   
    # })
    
    # observeEvent inn2_btn ----
    observeEvent(input$inn2_btn, {
      shared_values$step <- 14
      save_progress(shared_values, shared_parameters)
     switch_screen("screen14")
    })
    
    # help button 13_01----
    observeEvent(input$show_help_13_01, {
      showModal(modalDialog(
        title = "Step 13: Run IRM or start Innovation 2",
        includeMarkdown("docs/step_13_01.md"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
      
    })
}