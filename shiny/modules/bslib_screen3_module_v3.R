# modules/screen3.R

bslib_screen3_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  
  ns <- NS(id)
  tagList(
    # wellPanel(
    #   style = "padding: 10px; margin-bottom: 5px;",
    #   actionButton(ns("back_to_screen2"), label = tagList(
    #     icon("circle-left"),  # icon first 
    #     "Back to Screen 2"
    #     # text second
    #   ),
    #   class = "btn-primary"),
    #   actionButton(ns("to_screen4"), label = tagList(
    #     "Go to Screen 4",
    #     # text first
    #     icon("circle-right")  # icon second)
    #   ),
    #   class = "btn-primary")
    # ), 
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      h4("Screen 3: Number of Innovations"),
      
      # First set of radio buttons----
      radioButtons(
        ns("num_innovations"),
        "Number of innovations to be modelled:",
        choices = list("1" = "one_inn", "2" = "two_inn"),
        selected = shared_parameters$num_innovations
      ),
      
      # Second radio button (conditionally shown)----
      uiOutput(ns("innovation_system_ui"))
    )
  )
}

bslib_screen3_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("back_to_screen2"), 
                   title = "Go back to Step 2",
                   label = tagList(
                     icon("circle-left"),  # icon first 
                     #"Go to Introduction"
                     "Back"
                     # text second
                   ),
                   class = "btn-primary"),
      
      tags$span(
        tagList("Step 3", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen4"), 
                   title = "Go to Step 4",
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
      textOutput(ns("aggregation_display")),
      textOutput(ns("num_innovations_display")),
      textOutput(ns("innovation_system_display"))
    )
  )
}

bslib_screen3_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$innovation_system_ui <- renderUI({
      if (input$num_innovations == "two_inn") {
        radioButtons(ns("innovation_system"), "System:",
                     choices = list("Intercrop / Rotation" = "intercrop", "Compare" = "compare"),
                     selected = shared_parameters$innovation_system)
      } else {
        radioButtons(ns("innovation_system"), "System:",
                           choices = list("Single" = "single"),
                     selected = shared_parameters$innovation_system)}
    })
    
    # output$value <- renderText({ input$resolution })
    # 
    # output$value <- renderText({ input$aggregation })
    
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
    
    #1 observe radio buttons ----
    observe({
      req(input$num_innovations)
      message(paste("S3. 1 observe: radio buttons", shared_parameters$innovation_system))
      shared_parameters$num_innovations <- input$num_innovations
      shared_parameters$innovation_system <- input$innovation_system

    })
    
    #1 observeevent radio button 2 ----
    observeEvent(ns(input$innovation_system), {
      req(input$innovation_system)
      message(paste("S3. 2 observeevent: radio button 2", shared_parameters$innovation_system))
      shared_parameters$num_innovations <- input$num_innovations
      shared_parameters$innovation_system <- input$innovation_system
      
    })
    
    #2 observeEvent back_to_screen1 ----
    observeEvent(input$back_to_screen2, {
      switch_screen("screen2")
    })
    
    #2 observeEvent to_screen3 ----
    observeEvent(input$to_screen4, {
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
      switch_screen("screen4")
      message(paste("S3. To screen 4. forget:", shared_values$forget))
    })
    
  })
}
