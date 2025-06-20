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
    DTOutput(ns("conclusions_data_table"))
    
  )
}

bslib_screen6_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Render the data table----
    output$conclusions_data_table <- renderDT({
      message(paste("Initiation. inn details1:", shared_values$crop_name_1,"-", shared_values$ideotype_1,"-", shared_values$scenario_1))
      df_inn_tree_net <- read.csv(paste0("E:/repos/raise_fs/shiny/data/",
                                                shared_values$crop_name_1,
                                                "_",
                                                shared_values$ideotype_1,
                                                "_",
                                                shared_values$scenario_1,
                                                "_saved_tree_network.csv"))
      
      message(paste("Initiation. df_inn_tree_net"))
      print(df_inn_tree_net)
      
      df_inn_tree_net_stack <- dplyr::select(df_inn_tree_net, "stack_code") |> distinct()
      
      message(paste("Initiation. df_inn_tree_net_stack"))
      print(df_inn_tree_net_stack)

      
      datatable(
        df_inn_tree_net_stack,
        rownames = F,
        filter = "bottom",
        selection = list(mode = "single"),
        editable = T,
        options = list(
          # columnDefs = list(list(
          #   visible = FALSE, targets = c(0) # hide the inn_ID
          # )),
          lengthMenu = c(10, 20, 50),
          pageLength = 20#,
          #order = list(list(1, 'asc'), list(2, 'asc'), list(3, 'asc'))
        )
      )
    }, server = FALSE)
     
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
    
    # _ navigation----
    
    #2 observeEvent back_to_screen5 ----
    observeEvent(input$back_to_screen5, {
      shared_values$current_tree <- NULL
      switch_screen("screen5")
    })
    
    #2 observeEvent to_screen7 ----
    observeEvent(input$to_screen7, {
      switch_screen("screen7")
      
    })
    
  })
}