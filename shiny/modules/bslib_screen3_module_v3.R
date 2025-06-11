# modules/screen3.R

bslib_screen3_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  tagList(
    
    # First set of radio buttons----
    radioButtons(ns("num_innovations"), "Number of innovations to be modelled:",
                 choices = list("1" = "one_inn", "2" = "two_inn")),
    
    # Second radio button (conditionally shown)----
    uiOutput(ns("innovation_system_ui")),
  
    # UI actionButtons screen navigation ----
  actionButton(ns("back_to_screen2"), "Back to Screen 2"),
  actionButton(ns("to_screen4"), "Go to Screen 4")
  )
  
  
}

bslib_screen3_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("value_display")),
    textOutput(ns("level_display")),
    textOutput(ns("selection_display")),
    textOutput(ns("spatres_display")),
    textOutput(ns("aggregation_display")),
    textOutput(ns("num_innovations_display")),
    textOutput(ns("innovation_system_display"))
  )
}

bslib_screen3_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$innovation_system_ui <- renderUI({
      if (input$num_innovations == "two_inn") {
        radioButtons(ns("innovation_system"), "System:",
                     choices = list("Intercrop / Rotation" = "intercrop", "Compare" = "compare"))
      }
    })
    
    # output$value <- renderText({ input$resolution })
    # 
    # output$value <- renderText({ input$aggregation })
    
    output$num_innovations_display <- renderText({
      paste("Number of innovations:", shared_values$num_innovations)
    })
    
    output$innovation_system_display <- renderText({
      #if (shared_values$num_innovations == "two_inn") {
      if (input$num_innovations == "two_inn") {
        paste("Innovation System:", shared_values$innovation_system)
      }
    })    
    
    output$spatres_display <- renderText({
      paste("Your spatial resolution is:", shared_values$resolution)
    })

    
    output$aggregation_display <- renderText({
      paste("Your aggregation level is:", shared_values$aggregation)
    })
    #     
    # output$value_display <- renderText({
    #   req(shared_values$dropdown)
    #   paste("You selected on Screen 1:", shared_values$dropdown)
    # })
    
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
    
    #1 observe radio buttons ----
    observe({
      message(paste("1 observe: radio buttons", shared_values$innovation_system))
      shared_values$num_innovations <- input$num_innovations
      shared_values$innovation_system <- input$innovation_system

    })
    
    #1 observeevent radio button 2 ----
    observeEvent(ns(input$innovation_system), {
      message(paste("2 observeevent: radio button 2", shared_values$innovation_system))
      shared_values$num_innovations <- input$num_innovations
      shared_values$innovation_system <- input$innovation_system
      
    })
    
    #2 observeEvent back_to_screen1 ----
    observeEvent(input$back_to_screen2, {
      switch_screen("screen2")
    })
    
    #2 observeEvent to_screen3 ----
    observeEvent(input$to_screen4, {
      switch_screen("screen4")
    })
    
  })
}
