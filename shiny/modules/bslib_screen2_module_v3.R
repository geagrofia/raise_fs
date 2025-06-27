# modules/screen2.R

bslib_screen2_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  tagList(
  # UI numericInput for entering value for spatial resolution of the mask----
  numericInput(ns("resolution"), "Resolution (m)",
               ifelse(!is.null(shared_values$resolution),
                      shared_values$resolution,
                      250), min = 10, max = 100000, step = 10, width = NULL
  ), 
  
  # UI numericInput for entering value for aggregation level----
  numericInput( ns("aggregation"), "Aggregation level",
                ifelse(!is.null(shared_values$aggregation),
                       shared_values$aggregation,
                       1), min = 1, max = 100, step = 1, width = NULL
  ), 
  
  # UI actionButton Back to Screen 1 ----
  actionButton(ns("back_to_screen1"), "Back to Screen 1"),
  actionButton(ns("to_screen3"), "Go to Screen 3")
  )
  
  
}

bslib_screen2_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("value_display")),
    textOutput(ns("level_display")),
    textOutput(ns("selection_display")),
    textOutput(ns("spatres_display")),
    textOutput(ns("aggregation_display"))
    
  )
}

bslib_screen2_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # output$value <- renderText({ input$resolution })
    # 
    # output$value <- renderText({ input$aggregation })
    
    output$spatres_display <- renderText({
      paste("S2. Your spatial resolution is:", shared_values$resolution)
    })
    
    output$aggregation_display <- renderText({
      paste("S2. Your aggregation level is:", shared_values$aggregation)
    })
    #     
    # output$value_display <- renderText({
    #   req(shared_values$dropdown)
    #   paste("You selected on Screen 1:", shared_values$dropdown)
    # })
    
    output$level_display <- renderText({
      req(shared_values$level)
      paste("S2. You selected level on Screen 1:", shared_values$level)
    })
    
    output$selection_display <- renderText({
      req(shared_values$level)
      
      if (shared_values$level == "woreda") {
        paste(
          "S2. You selected geography:",
          shared_values$selected_region,
          shared_values$selected_zone,
          shared_values$selected_woreda
        )
      } else {
        if (shared_values$level == "zone") {
          paste(
            "S2. You selected geography:",
            shared_values$selected_region,
            shared_values$selected_zone
          )
        } else {
          if (shared_values$level == "region") {
            paste("S2. You selected geography:",
                  shared_values$selected_region)
          } else {
            paste("S2. You selected geography: Ethiopia")
          }
        }
      }
    }) 
    
    #1 observe spatial resolution and aggregation ----
    observe({
      shared_values$resolution <- input$resolution
      shared_values$aggregation <- input$aggregation

    })
    
    #2 observeEvent back_to_screen1 ----
    observeEvent(input$back_to_screen1, {
      switch_screen("screen1")
    })
    
    #2 observeEvent to_screen3 ----
    observeEvent(input$to_screen3, {
      switch_screen("screen3")
    })
    
  })
}
