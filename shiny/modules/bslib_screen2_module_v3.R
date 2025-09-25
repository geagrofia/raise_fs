# modules/screen2.R

bslib_screen2_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  
  ns <- NS(id)
  
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(ns("back_to_screen1"), label = tagList(
    #     icon("circle-left"),  # icon first 
    #     "Back to Screen 1"
    #     # text second
    #   ),
    #   class = "btn-primary"),
    #   actionButton(ns("to_screen3"), label = tagList(
    #     "Go to Screen 3",
    #     # text first
    #     icon("circle-right")  # icon second)
    #   ),
    #   class = "btn-primary")
    # ), 
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      div(
        style = "display:inline-block;vertical-align:middle;margin-bottom: 5px;",
        actionButton(
          ns("show_help_02_01"),
          title = "Help for Step 2",
          label = tagList(
            icon("circle-question")  # icon second)
          ),
          style = "background: rgba(23, 162, 184, 0.5);"
        )
        
      ),
      div(
        style = "display: inline-block; vertical-align: middle; margin-left: 10px;",
        h4("Step 2: Define IRM Spatial Resolution")
      ),
      # UI numericInput for entering value for spatial resolution of the mask----
      numericInput(ns("resolution"), "Resolution (m)",
               ifelse(!is.null(shared_parameters$resolution),
                      shared_parameters$resolution,
                      5000), min = 10, max = 100000, step = 10, width = NULL), 
  
      # UI numericInput for entering value for aggregation level----
      numericInput(ns("aggregation"), "Aggregation level",
                ifelse(!is.null(shared_parameters$aggregation),
                       shared_parameters$aggregation,
                       1), min = 1, max = 100, step = 1, width = NULL) 
    )
  )
}

bslib_screen2_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen1"), 
                   title = "Go back to Step 1: Select Geography",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 2", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen3"), 
                   title = "Go to Step 3: Number of Innovations",
                   label = tagList(
                     #"Go to Screen 2",
                     "Next",
                     # text first
                     icon("circle-right")  # icon second)
                   ),
                   class = "btn-primary")
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
      textOutput(ns("aggregation_display"))
    )
  )
}

bslib_screen2_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # output$value <- renderText({ input$resolution })
    # 
    # output$value <- renderText({ input$aggregation })
    
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
    
    #1 observe spatial resolution and aggregation ----
    observe({
      shared_parameters$resolution <- input$resolution
      shared_parameters$aggregation <- input$aggregation

    })
    
    #2 observeEvent back_to_screen1 ----
    observeEvent(input$back_to_screen1, {
      switch_screen("screen1")
    })
    
    #2 observeEvent to_screen3 ----
    observeEvent(input$to_screen3, {
      shared_values$step <- 3
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      switch_screen("screen3")
    })
    
    # help button 02_01----
    observeEvent(input$show_help_02_01, {
      showModal(modalDialog(
        title = "Step 2: Define IRM Spatial Resolution",
        includeMarkdown("docs/step_02_01.md"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
  })
}
