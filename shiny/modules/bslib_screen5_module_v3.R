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
    textOutput(ns("inn_type_1_display")),
    #DTOutput(ns("inn_req_data_table")),
    htmlOutput(ns("inn_type_edit_mode_1_display")),
    
    # shinyTree(ns("tree"),
    #   checkbox = F,
    #   theme = "proton",
    #   stripes = F,
    #   themeIcons = TRUE,
    #   themeDots = TRUE,
    #   dragAndDrop = TRUE,
    #   contextmenu = T
    # ),
    uiOutput(ns("shiny_tree_editing")),
    uiOutput(ns("dyanamic_save_reset"))
    #actionButton(ns("save_button"), "Save Changes / Re-load"),  # Button to save changes
    #actionButton(ns("reset_button"), "Reset to Original") # Button to reset to original      
    
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
    
    # --- STEP 1: Recursive cleaner to convert 0 to list() and remove attrs ----
    clean_tree <- function(tree) {
      if (is.null(tree)) return(NULL)
      
      out <- list()
      for (name in names(tree)) {
        child <- tree[[name]]
        
        # Extract crit_code
        crit_code <- attr(child, "crit_code")
        
        # Ensure child is a list even if it's a leaf
        if (!is.list(child)) {
          child <- list()
        } else {
          child <- child[!grepl("^st", names(child))]  # remove shinyTree artifacts
        }
        
        # Reattach crit_code
        attr(child, "crit_code") <- crit_code
        out[[name]] <- clean_tree(child)
        attributes(out[[name]]) <- attributes(child)  # reapply crit_code
      }
      return(out)
    }
    
    # --- STEP 2: Edge extractor ----
    get_edges <- function(node) {
      edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
      for (child in node$children) {
        edges <- rbind(edges, data.frame(from = node$name, to = child$name, stringsAsFactors = FALSE))
        edges <- rbind(edges, get_edges(child))
      }
      return(edges)
    }
    
    # # gives NA values
    # get_edges_with_codes <- function(node) {
    #   edges <- list()
    #   
    #   from_code <- attr(node, "crit_code")
    #   
    #   for (child in node$children) {
    #     to_code <- attr(child, "crit_code")
    #     
    #     edges[[length(edges) + 1]] <- data.frame(
    #       from = node$name,
    #       to = child$name,
    #       from_code = if (!is.null(from_code)) from_code else NA,
    #       to_code = if (!is.null(to_code)) to_code else NA,
    #       stringsAsFactors = FALSE
    #     )
    #     
    #     # Recurse on the child
    #     child_edges <- get_edges_with_codes(child)
    #     if (nrow(child_edges) > 0) {
    #       edges[[length(edges) + 1]] <- child_edges
    #     }
    #   }
    #   
    #   # Combine all into a single data.frame
    #   if (length(edges) > 0) {
    #     do.call(rbind, edges)
    #   } else {
    #     data.frame(from = character(), to = character(),
    #                from_code = character(), to_code = character(),
    #                stringsAsFactors = FALSE)
    #   }
    # }
    
    
    # 20/06/2025----
    
    # Recursive function to build a data.tree Node from the shinyTree-style list
    build_tree <- function(name, node_data) {
      node <- Node$new(name)
      
      # Add crit_code if it exists
      crit_code <- attr(node_data, "crit_code")
      if (!is.null(crit_code)) {
        node$crit_code <- crit_code
      }
      
      # Check if node_data has children (i.e., is a list)
      if (is.list(node_data)) {
        for (child_name in names(node_data)) {
          child <- build_tree(child_name, node_data[[child_name]])
          node$AddChildNode(child)
        }
      }
      
      return(node)
    }
    
    # Function to extract edges and crit_codes
    get_edges_with_codes <- function(node) {
      edges <- list()
      
      for (child in node$children) {
        edges[[length(edges) + 1]] <- data.frame(
          stack_code = if (!is.null(node$crit_code)) node$crit_code else NA,
          stack = node$name,
          crit_code = if (!is.null(child$crit_code)) child$crit_code else NA,
          criterion = child$name,
          stringsAsFactors = FALSE
        )
        
        # Recurse on children
        child_edges <- get_edges_with_codes(child)
        if (nrow(child_edges) > 0) {
          edges[[length(edges) + 1]] <- child_edges
        }
      }
      
      if (length(edges) > 0) {
        do.call(rbind, edges)
      } else {
        data.frame(stack = character(), criterion = character(),
                   stack_code = character(), crit_code = character(),
                   stringsAsFactors = FALSE)
      }
    }
    
    
    
    #render the tree ui output----
    
    output$shiny_tree_editing <- renderUI({
      if (shared_values$inn_type_1 == "existing") {
        shinyTree(
          ns("tree"),
          checkbox = F,
          theme = "proton",
          stripes = F,
          themeIcons = TRUE,
          themeDots = TRUE,
          dragAndDrop = FALSE,
          contextmenu = FALSE
        )
      } else {
        shinyTree(
          ns("tree"),
          checkbox = F,
          theme = "proton",
          stripes = F,
          themeIcons = TRUE,
          themeDots = TRUE,
          dragAndDrop = TRUE,
          contextmenu = T
        )
        
        
      }
      
      
      
      #shinyTree::shinyTreeOutput(ns("tree"))
    })
    
    
    # render the tree----
    output$tree <- renderTree({
      req(switch_screen())
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
      df_short <-  df_inn_complete |> dplyr::select("stack", "criterion", "crit_code")
      # print(df_short)
      
      df_short$stack[is.na(df_short$stack)] <- "root2"
      
      
      # Convert to tree
      tree_plot <- FromDataFrameNetwork(df_short, c("crit_code"))
      
      
      #Add crit_code to node names if they exist
      tree_plot$Do(function(node) {
        if (!is.null(node$crit_code) && !is.na(node$crit_code)) {
          #node$name <- paste0(node$name, " (crit_code: ", node$crit_code, ")")
          #node$crit_code <- paste0(node$name, " (crit_code: ", node$crit_code, ")")
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
        print(shared_values$current_tree)
        shared_values$current_tree <- tree_json
        } 
      
      message(paste("renderTree 2.  shared_values$current_tree is not NULL"))
      print(shared_values$current_tree)
          # 
      # # Print the result
      #print(tree_json)
      
      shared_values$current_tree
      
    })
    
    # render the buttons UI output ----
    output$dyanamic_save_reset <- renderUI({
      if (shared_values$inn_type_1 == "existing") {
        # observe the inn_type to determine button visibility----
      } else {
        tagList(
        actionButton(ns("save_dyn_button"), "Save Changes / Re-load"), # Button to save changes
        actionButton(ns("reset_dyn_button"), "Reset to Original")) # Button to reset to original
      }
    })
  # }
  # })
    
    # observeEvent save button is clicked ----
    observeEvent(input$save_dyn_button, {
      
      #message(paste("observeEvent: save_button  print(input$tree):"))
      #print(input$tree)
      message("str(input$tree)")
      print(str(input$tree))
      saved_tree <- input$tree  # Retrieve the current state from the input
      
      #message(paste("observeEvent: save_button  print(saved_tree):"))
      #print(saved_tree)
      
      #message(paste("observeEvent: save_button  print(shared_values$current_tree):"))
      #print(shared_values$current_tree)
      
      
      if (!is.null(saved_tree)) {
        shared_values$current_tree <- saved_tree  # Update the reactive value
        #print(shared_values$current_tree)
        
        #message(paste("observeEvent: save_button IF  print(shared_values$current_tree):"))
        #print(shared_values$current_tree)
        
        #output$tree_output <- renderPrint(saved_tree)  # Output for debugging
        #print(saved_tree)
        #output$tree_output <- renderTree(saved_tree)
        # Optionally: Here you could save `saved_tree` to a file or database
        # For example, to save as a JSON file:
        jsonlite::write_json(
          saved_tree,
          paste0(
            "data/",
            shared_values$crop_name_1,
            "_",
            shared_values$ideotype_1,
            "_",
            shared_values$scenario_1 ,
            "_saved_tree.json"
          ))
        
        write.csv(treeToDf(saved_tree), paste0(
          "data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1 ,
          "_saved_tree.csv"))
        
        # # Step 1: Clean the tree (remove attrs, fix leaves)
        # cleaned <- clean_tree(input$tree)
        # message("str(cleaned)")
        # print(str(cleaned))
        # # Step 2: Convert to data.tree structure
        # root <- tryCatch({
        #   node <- as.Node(cleaned)
        #   message("node")
        #   print(node)
        #   node$name <- "Root"  # Optional: set root name
        #   node
        # }, error = function(e) {
        #   message("Error converting tree: ", e$message)
        #   return(NULL)
        # })
        # 
        # message("root")
        # print(root)
        # 
        # # Step 3: Generate edges
        # if (!is.null(root)) {
        #   edges_df <- get_edges_with_codes(root)
        #   message("edges_df")
        #   print(edges_df)
        # } else {
        #   print("Tree conversion failed.")
        # }
        
        if (is.null(input$tree)) return()
        
        root_name <- names(input$tree)[1]
        root_data <- input$tree[[1]]
        
        tree <- build_tree(root_name, root_data)
        
        edges_df <- get_edges_with_codes(tree)
        print(edges_df)
        

        write.csv(edges_df, paste0(
          "data/",
          shared_values$crop_name_1,
          "_",
          shared_values$ideotype_1,
          "_",
          shared_values$scenario_1 ,
          "_saved_tree_network.csv"))
        
      }
    })
    
    # observeEvent reset button is clicked----
    observeEvent(input$reset_dyn_button, {
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
    
    output$inn_type_1_display <- renderText({
      message(paste("Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      paste("You selected Innovation type on Screen 4:", shared_values$inn_type_1)
    })
    
    output$inn_type_edit_mode_1_display <- renderUI({
      message(paste("Innovation type:", shared_values$inn_type_1))
      req(shared_values$inn_type_1)
      if (shared_values$inn_type_1 == "existing") {
        str1 <- paste("")
        str2 <- paste(h3("View Hierarchy"))
      } else {
        str1 <- paste("")
        str2 <- paste(h3("Edit Hierarchy"))
      }
      HTML(paste(str1, str2, sep = '<br/>'))
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
      switch_screen("screen4")
    })
    
    #2 observeEvent to_screen6 ----
    observeEvent(input$to_screen6, {
      switch_screen("screen6")
      
    })
    
  })
}
