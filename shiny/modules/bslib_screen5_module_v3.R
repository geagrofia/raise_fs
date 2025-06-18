library(shinyTree)
library(data.tree)

bslib_screen5_module_v3_SidebarUI <- function(id, shared_values) {
  
  ns <- NS(id)
  
  tagList(
    h3("Rule Base Hierarchy:"),
    
    # UI actionButtons screen navigation ----
    actionButton(ns("back_to_screen4"), "Back to Screen 4"),
    actionButton(ns("to_screen6"), "Go to Screen 6")
  )
}

bslib_screen5_module_v3_MainUI <- function(id) {
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
    #DTOutput(ns("inn_req_data_table")),
    h3("Edit Hierarchy:"),
    shinyTree(ns("tree"),
      checkbox = F,
      theme = "proton",
      stripes = F,
      themeIcons = TRUE,
      themeDots = TRUE,
      dragAndDrop = TRUE,
      contextmenu = T
    ),
    actionButton(ns("save_button"), "Save Changes"),  # Button to save changes
    actionButton(ns("reset_button"), "Reset to Original") # Button to reset to original      
    
  )
}

bslib_screen5_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
     # df_inn_req <- reactive(read.csv(paste0("E:/repos/raise_fs/shiny/data/", 
     #                               shared_values$crop_name_1,
     #                               "_",
     #                               shared_values$ideotype_1,
     #                               "_", 
     #                               shared_values$scenario_1,
     #                               "_links.csv")))
    
    #print(df_inn_req())
    
    # FROM IRM AUTOMATE - Return a logical vector indicating which cases are complete, i.e., have no missing values
    # df_inn_complete <- reactive(df_inn_req()[complete.cases(df_inn_req()$crit_code, df_inn_req()$criterion), ])
    # 
    #print(df_inn_req())
    
    # df_short <- reactive(dplyr::select(df_inn_complete(), stack, criterion, weight))
    
    # Render the data table
    # output$inn_req_data_table <- renderDT({
    #   
    #   # alternative
    #   
    #   df_inn_req <- read.csv(paste0("E:/repos/raise_fs/shiny/data/",
    #                                          shared_values$crop_name_1,
    #                                          "_",
    #                                          shared_values$ideotype_1,
    #                                          "_",
    #                                          shared_values$scenario_1,
    #                                          "_links.csv"))
    #   
    #   #print(df_inn_req())
    #   
    #   # FROM IRM AUTOMATE - Return a logical vector indicating which cases are complete, i.e., have no missing values
    #   df_inn_complete <- df_inn_req[complete.cases(df_inn_req$crit_code, df_inn_req$criterion), ]
    #   # # 
    #   df_short <-  df_inn_complete |> dplyr::select("stack", "criterion", "weight")
    #   # print(df_short)
    #   
    #   df_short$stack[is.na(df_short$stack)] <- "root2"
    # 
    #  
    #  # Convert to tree
    #  tree_plot <- FromDataFrameNetwork(df_short, c("weight"))
    #  
    #  # Add icon attributes to nodes
    #  set_icons <- function(node) {
    #    if (node$isLeaf) {
    #      node$icon <- "map"  # data criteria
    #    } else {
    #      node$icon <- "layer-group"  # non-data criteria
    #    }
    #    for (child in node$children) {
    #      set_icons(child)
    #    }
    #  }
    #  set_icons(tree_plot)
    # # 
    # # 
    # # 
    # # # Convert to json for rendering
    #  testjson <- treeToJSON(tree_plot)
    #  
    #  # Reactive value to hold the current tree structure
    #  current_tree <- reactiveVal(testjson)
    # # 
    # # # Print the result
    #  #print(testjson)
    #   
    #   datatable(
    #     df_short,
    #     rownames = F,
    #     filter = "bottom",
    #     selection = list(mode = "single"),
    #     editable = FALSE#,
    #     # options = list(
    #     #   columnDefs = list(list(
    #     #     visible = FALSE, targets = c(0) # hide the inn_ID
    #     #   )),
    #     #   lengthMenu = c(10, 20, 50, 100),
    #     #   pageLength = 20,
    #     #   order = list(list(1, 'asc'), list(2, 'asc'), list(3, 'asc'))
    #     # )
    #   )
    # }, server = FALSE)
    
    #print(df_short())
    
    # render the tree----
    output$tree <- renderTree({
      
      df_inn_req <- read.csv(paste0("E:/repos/raise_fs/shiny/data/",
                                    shared_values$crop_name_1,
                                    "_",
                                    shared_values$ideotype_1,
                                    "_",
                                    shared_values$scenario_1,
                                    "_links.csv"))
      
      #print(df_inn_req())
      
      # FROM IRM AUTOMATE - Return a logical vector indicating which cases are complete, i.e., have no missing values
      df_inn_complete <- df_inn_req[complete.cases(df_inn_req$crit_code, df_inn_req$criterion), ]
      # # 
      df_short <-  df_inn_complete |> dplyr::select("stack", "criterion", "weight")
      # print(df_short)
      
      df_short$stack[is.na(df_short$stack)] <- "root2"
      
      
      # Convert to tree
      tree_plot <- FromDataFrameNetwork(df_short, c("weight"))
      
      
      # Add weights to node names if they exist
      tree_plot$Do(function(node) {
        if (!is.null(node$weight) && !is.na(node$weight)) {
          node$name <- paste0(node$name, " (weight: ", node$weight, ")")
        }
      })
      
      tree_plot$Do(function(node) {
        node$stopened <- TRUE
      })
      
      #print(tree_plot)
      
      # Add icon attributes to nodes
      set_icons <- function(node) {
        if (node$isLeaf) {
          node$icon <- "map"  # data criteria
        } else {
          node$icon <- "layer-group"  # non-data criteria
        }
        for (child in node$children) {
          set_icons(child)
        }
      }
      set_icons(tree_plot)
      
      # 
      # 
      # 
      # # Convert to json for rendering
      tree_json <- treeToJSON(tree_plot)
      
      if (is.null(shared_values$current_tree))
        {# Reactive value to hold the current tree structure
        message(paste("renderTree 1.  shared_values$current_tree is NULL"))
        shared_values$current_tree <- tree_json
        } 
      
      message(paste("renderTree 2.  shared_values$current_tree is not NULL"))
          # 
      # # Print the result
      #print(tree_json)
      
      shared_values$current_tree
      
    })
    
    # observeEvent save button is clicked ----
    observeEvent(input$save_button, {
      
      message(paste("observeEvent: save_button  print(input$tree):"))
      print(input$tree)
      
      saved_tree <- input$tree  # Retrieve the current state from the input
      
      message(paste("observeEvent: save_button  print(saved_tree):"))
      print(saved_tree)
      
      message(paste("observeEvent: save_button  print(shared_values$current_tree):"))
      print(shared_values$current_tree)
      
      
      if (!is.null(saved_tree)) {
        shared_values$current_tree <- saved_tree  # Update the reactive value
        #print(shared_values$current_tree)
        
        message(paste("observeEvent: save_button IF  print(shared_values$current_tree):"))
        print(shared_values$current_tree)
        
        #output$tree_output <- renderPrint(saved_tree)  # Output for debugging
        #print(saved_tree)
        #output$tree_output <- renderTree(saved_tree)
        # Optionally: Here you could save `saved_tree` to a file or database
        # For example, to save as a JSON file:
        jsonlite::write_json(
          saved_tree,
          paste(
            "data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1 ,
            "_tree.json"
          ))
        write.csv(treeToDf(saved_tree), paste(
          "data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1 ,
          "_tree.csv"))
        
        
      }
    })
    
    # observeEvent reset button is clicked----
    observeEvent(input$reset_button, {
      print(shared_values$current_tree)
      shared_values$current_tree <- NULL
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

    # _ navigation----
    
    #2 observeEvent back_to_screen4 ----
    observeEvent(input$back_to_screen4, {
      shared_values$current_tree <- NULL
      switch_screen("screen4")
    })
    
    #2 observeEvent to_screen6 ----
    observeEvent(input$to_screen6, {
      switch_screen("screen6")
      
    })
    
  })
}
