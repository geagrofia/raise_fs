# modules/screen2.R

bslib_screen2_module_v3_SidebarUI <- function(id) {
  # No sidebar for screen 2
  NULL
}

bslib_screen2_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("value_display")),
    textOutput(ns("level_display")),
    textOutput(ns("selection_display")),
    actionButton(ns("back_to_screen1"), "Back to Screen 1")
  )
}

bslib_screen2_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    output$value_display <- renderText({
      req(shared_values$dropdown)
      paste("You selected on Screen 1:", shared_values$dropdown)
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
    
    observeEvent(input$back_to_screen1, {
      switch_screen("screen1")
    })
  })
}
