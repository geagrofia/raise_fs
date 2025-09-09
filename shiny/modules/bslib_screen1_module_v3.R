# modules/screen1.R

library(terra)
library(leaflet)


bslib_screen1_module_v3_SidebarUI <- function(id, shared_values, shared_parameters) {
  ns <- NS(id)
  tagList(
    # selectInput(ns("my_dropdown"), "Choose a value", choices = c("A", "B", "C"),
    #             selected = shared_values$dropdown  # <-- Persist selected value
    # ),
    
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      h4("Step 1: Select Geography"),
      radioButtons(
        ns("level"),
        "Select level:",
        choices = c(
          "Nation" = "nation",
          "Region" = "region",
          "Zone" = "zone",
          "Woreda" = "woreda"
        ),
        selected = shared_parameters$level
      ),
      uiOutput(ns("region_ui")),
      uiOutput(ns("zone_ui")),
      uiOutput(ns("woreda_ui")),
      span(
        "The depiction and use of boundaries, geographic names and related data
       shown on maps and included in lists, tables, documents, and databases
       in this tool are not warranted to be error-free nor do they necessarily
       imply official endorsement or acceptance",
        style = "color:red; font-style: italic;"
      )
    )
    #,  
    
    # #---new
    # radioButtons(ns("mode"), "Choose mode", choices = c("Option 1" = 1, "Option 2" = 2, "Option 3" = 3, "Option 4" = 4)),
    # uiOutput(ns("dropdown1_ui")),
    # uiOutput(ns("dropdown2_ui")),
    # uiOutput(ns("dropdown3_ui")),
    # 
    # #--- end new
    #actionButton(ns("auto_reload"), "auto_reload"),

  )
}

bslib_screen1_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Navigate", style = "color: var(--bs-secondary);"),
      style = "padding: 10px; margin-bottom: 5px;",
      actionButton(ns("to_screen0"), 
        title = "Go back to Introduction",
        label = tagList(
          icon("circle-left"),  # icon first 
          #"Go to Introduction"
          "Back"
          # text second
          ),
        class = "btn-primary"),
      
      tags$span(
        tagList("Step 1", icon("location-crosshairs")),  # text + icon
        class = "btn btn-info disabled"
      ),
      # <button type="button" class="btn btn-secondary" data-bs-toggle="tooltip" data-bs-placement="left" data-bs-original-title="Tooltip on left">Left</button>
      actionButton(ns("to_screen2"), 
                   title = "Go to Step 2",
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
      textOutput(ns("main_output")), 
      textOutput(ns("main_level_output")),
      textOutput(ns("main_geography_output"))
    ),
    wellPanel(
      style = "padding: 10px; margin-bottom: 5px;",
      h4("Selected Geography"), 
      leafletOutput(ns("base_map"), height = "500px")
    )
  )
}

bslib_screen1_module_v3_Server <- function(id, shared_values, shared_parameters, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Initially disable select_row and duplicate_row, and next_screen ----
    disable("home")
    
    # load geojson files and convert to df
    vect_nation <- terra::vect("E:/repos/raise_fs/shiny/data/eth_nation.geojson")
    df_nation <- as.data.frame(vect_nation)
    
    vect_region <- vect("E:/repos/raise_fs/shiny/data/admin_Ethiopia_region.geojson")
    df_region <- as.data.frame(vect_region)
    
    vect_zone <- vect("E:/repos/raise_fs/shiny/data/admin_Ethiopia_zone.geojson")
    df_zone <- as.data.frame(vect_zone)[, c("ADM2_EN", "ADM1_EN", "ADM2_CODE", "ADM1_CODE")]
    
    vect_woreda <- vect("E:/repos/raise_fs/shiny/data/admin_Ethiopia_woreda.geojson")
    df_woreda <- as.data.frame(vect_woreda)[, c("ADM3_EN", "ADM2_EN", "ADM2_CODE", "ADM3_CODE")]
    
    #0 ---- Render initial map ----
    output$base_map <- renderLeaflet({
      message("S1. 0 output$base_map")
      
      req(switch_screen())
      #req(switch_screen() == "screen2")
      #req(input$level)
      
      nation_ext <- unlist(unname(as.vector(ext(vect_nation)))) # Extent of nation as unnamed vector
      
      leaflet() |>
        addTiles() |>
        fitBounds(nation_ext[1], nation_ext[3], nation_ext[2], nation_ext[4])  # Zoom to selected nation
    })

    #0 ----Update region dropdown UI----
    output$region_ui <- renderUI({
      if (input$level %in% c("region", "zone", "woreda")) {
        message("S1. 0 output$region_ui")
        selectInput(ns("region"), "Region:",
                    choices = unique(df_region$ADM1_EN), 
                    #selected = isolate(input$region))
                    selected = shared_parameters$selected_region)
      }
    })

    #0 ----Update zone dropdown UI----
    output$zone_ui <- renderUI({
      req(input$level %in% c("zone", "woreda"))
      req(input$region)
      message("S1. 0 output$zone_ui")
      filtered_zones <- unique(df_zone$ADM2_EN[df_zone$ADM1_EN == input$region])
      selectInput(ns("zone"), "Zone:",
        choices = filtered_zones,
        #selected = isolate(input$zone))
        selected = shared_parameters$selected_zone)
    })
    
    #0 ----Update woreda dropdown UI----
    output$woreda_ui <- renderUI({
      req(input$level == "woreda")
      req(input$region, input$zone)
      message("S1. 0 output$woreda_ui")
      filtered_woredas <- unique(df_woreda$ADM3_EN[df_woreda$ADM2_EN == input$zone])
      selectInput(ns("woreda"), "Woreda:",
                  choices = filtered_woredas,
                  #selected = isolate(input$woreda))
      selected = shared_parameters$selected_woreda)
    })
    
    #0 Update test text in main UI----
    output$main_output <- renderText({
      req(shared_values$dropdown)
      paste("Step 1. You selected:", shared_values$dropdown)
    })
    
    #0 Update geo level text in main UI----
    output$main_level_output <- renderText({
      req(shared_parameters$level)
      paste("Step 1. Spatial level =", shared_parameters$level)
    })
    
    #0 Update geography text in main UI----
    output$main_geography_output <- renderText({
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
    
    
    # ###---new
    # 
    # ## Dummy values for dropdowns
    # level1_choices <- c("A", "B")
    # level2_map <- list(A = c("A1", "A2"), B = c("B1", "B2"))
    # level3_map <- list(A1 = c("A1x", "A1y"), B1 = c("B1x", "B1y"))
    # 
    # # Dropdown 1
    # output$dropdown1_ui <- renderUI({
    #   req(as.numeric(input$mode) >= 2)
    #   selectInput(ns("dropdown1"), "Choose first", choices = level1_choices)
    # })
    # 
    # # Dropdown 2, depends on dropdown1
    # output$dropdown2_ui <- renderUI({
    #   req(as.numeric(input$mode) >= 3, input$dropdown1)
    #   selectInput(ns("dropdown2"), "Choose second", choices = level2_map[[input$dropdown1]])
    # })
    # 
    # # Dropdown 3, depends on dropdown2
    # output$dropdown3_ui <- renderUI({
    #   req(as.numeric(input$mode) == 4, input$dropdown2)
    #   selectInput(ns("dropdown3"), "Choose third", choices = level3_map[[input$dropdown2]])
    # })
    # 
    # # Save selection in shared_values if needed
    # observe({
    #   shared_values$mode <- input$mode
    #   shared_values$dropdown1 <- input$dropdown1
    #   shared_values$dropdown2 <- input$dropdown2
    #   shared_values$dropdown3 <- input$dropdown3
    # })
    # 
    # ###---end new
    
    #1 Observe selection and update map----
    observe({
      req(input$level)
      map <- leafletProxy(ns("base_map"), session) 
      map |> clearShapes()
      
      if (input$level == "nation") {
        message("S1. 1 Leaflet update observe: nation")
        nation_ext <- unlist(unname(as.vector(ext(vect_nation)))) # Extent of nation as unnamed vector
        map |>
          addPolygons(data = vect_nation, color = "blue", weight = 2) %>%
          fitBounds(nation_ext[1], nation_ext[3], nation_ext[2], nation_ext[4])  # Zoom to selected nation
      }
      
      if (input$level == "region" && !is.null(input$region)) {
        message("S1. 1 Leaflet update observe: region")
        region_id <- df_region |> dplyr::filter(ADM1_EN == input$region) |> pull(ADM1_EN)
        region_data <- vect_region |> subset(vect_region$ADM1_EN == region_id)
        region_ext <- unlist(unname(as.vector(ext(region_data)))) # Extent of selected region as unnamed vector
        map |>
          addPolygons(data = region_data, color = "green", weight = 2) |> 
          fitBounds(region_ext[1], region_ext[3], region_ext[2], region_ext[4])  # Zoom to selected region
      }
      
      if (input$level == "zone" && !is.null(input$zone)) {
        message("S1. 1 Leaflet update observe: zone")
        zone_id <- df_zone %>% dplyr::filter(ADM2_EN == input$zone) %>% pull(ADM2_EN)
        zone_data <- vect_zone %>% subset(vect_zone$ADM2_EN == zone_id)
        zone_ext <- unlist(unname(as.vector(ext(zone_data)))) # Extent of selected zone as unnamed vector
        map %>%
          addPolygons(data = zone_data, color = "red", weight = 2) %>% 
          fitBounds(zone_ext[1], zone_ext[3], zone_ext[2], zone_ext[4])  # Zoom to selected zone
      }
      
      if (input$level == "woreda" && !is.null(input$woreda)) {
        message("S1. 1 Leaflet update observe: woreda")
        woreda_id <- df_woreda %>% dplyr::filter(ADM3_EN == input$woreda) %>% pull(ADM3_EN)
        woreda_data <- vect_woreda %>% subset(vect_woreda$ADM3_EN == woreda_id)
        woreda_ext <- unlist(unname(as.vector(ext(woreda_data)))) # Extent of selected woreda as unnamed vector
        map %>%
          addPolygons(data = woreda_data, color = "black", weight = 2) %>% 
          fitBounds(woreda_ext[1], woreda_ext[3], woreda_ext[2], woreda_ext[4])  # Zoom to selected woreda
      }
    })
    
    # observeEvent(input$my_dropdown, {
    #   message("2 input$my_dropdown observeEvent")
    #   shared_values$dropdown <- input$my_dropdown
    # })
    
    #3 observeEvent input$level ----
    observeEvent(input$level, {
      message("S1. 3 input$level observeEvent")
      shared_parameters$level <- input$level
    })
    
    #4 observeEvent input$to_screen0 ----
    observeEvent(input$to_screen0, {
      message("S1. 4 input$to_screen0 observeEvent")
      #save_progress(shared_values, shared_parameters)
      #showNotification("Progress saved!", type = "message")
      switch_screen("screen0")
    })
    
    #4 observeEvent input$to_screen2 ----
    observeEvent(input$to_screen2, {
      message("S1. 4 input$to_screen2 observeEvent")
      #save_progress(shared_values, shared_parameters)
      #showNotification("Progress saved!", type = "message")
      switch_screen("screen2")
    })

    
    # # Update input to reflect shared value if returning
    # observe({
    #   updateSelectInput(session, "my_dropdown", selected = shared_values$dropdown)
    # })
    # 
    # # Update input to reflect shared value if returning
    # observe({
    #   updateSelectInput(session, "level", selected = shared_values$level)
    # })
    
    #5 observe levels and geography ----
    observe({
      req(input$level)
      shared_parameters$level <- input$level
      shared_parameters$selected_nation <- "Ethiopia"
      shared_parameters$selected_region <- input$region
      shared_parameters$selected_zone <- input$zone
      shared_parameters$selected_woreda <- input$woreda
      
      message("S1. 5 observe level and selected geography name from input")
      
    })
    
    
    #6 observe Store selected value in shared reactiveValues (optional) ----
    observe({
      req(input$level)
      if (input$level == "nation") {
        
      shared_parameters$selected_nation <- "Ethiopia"
      shared_parameters$ZONCODEVAR <- "ADM0_CODE"
      shared_parameters$ZONCODEVAL <-  "ET"
      shared_parameters$DIVCODEVAR <- "ADM0_CODE"
      shared_parameters$DIVCODEVAL <- "ET"
      shared_parameters$DIVNAMEVAR <- "ADM0_EN"
      shared_parameters$SUBDIVNAMEVAR <- "ADM1_EN"
      }
      
      if (input$level == "region") {
      req(input$region)  
      shared_parameters$selected_region <- input$region
      shared_parameters$ZONCODEVAR <- "ADM0_CODE"
      shared_parameters$ZONCODEVAL <-  "ET"
      shared_parameters$DIVCODEVAR <- "ADM1_CODE"
      shared_parameters$DIVCODEVAL <- dplyr::filter(df_region, ADM1_EN == input$region) |> pull(ADM1_CODE)
      message("S1. shared_parameters$DIVCODEVAL")
      print(shared_parameters$DIVCODEVAL)
      shared_parameters$DIVNAMEVAR <- "ADM1_EN"
      shared_parameters$SUBDIVNAMEVAR <- "ADM2_EN"
      }
      
      if (input$level == "zone")   {
      req(input$zone)   
      shared_parameters$selected_zone <- input$zone
      shared_parameters$ZONCODEVAR <- "ADM1_CODE"
      shared_parameters$ZONCODEVAL <-  dplyr::filter(df_region, ADM1_EN == input$region) |> pull(ADM1_CODE)
      shared_parameters$DIVCODEVAR <- "ADM2_CODE"
      shared_parameters$DIVCODEVAL <- dplyr::filter(df_zone, ADM2_EN == input$zone) |> pull(ADM2_CODE)
      message("S1. shared_parameters$DIVCODEVAL")
      print(shared_parameters$DIVCODEVAL)
      shared_parameters$DIVNAMEVAR <- "ADM2_EN"
      shared_parameters$SUBDIVNAMEVAR <- "ADM3_EN"
      }
      
      if (input$level == "woreda") {
      req(input$woreda)     
      shared_parameters$selected_woreda <- input$woreda
      shared_parameters$ZONCODEVAR <- "ADM2_CODE"
      shared_parameters$ZONCODEVAL <-  dplyr::filter(df_zone, ADM2_EN == input$zone) |> pull(ADM2_CODE)
      shared_parameters$DIVCODEVAR <- "ADM3_CODE"
      shared_parameters$DIVCODEVAL <- dplyr::filter(df_woreda, ADM3_EN == input$woreda) |> pull(ADM3_CODE)
      message("S1. shared_parameters$DIVCODEVAL")
      print(shared_parameters$DIVCODEVAL)
      shared_parameters$DIVNAMEVAR <- "ADM3_EN"
      shared_parameters$SUBDIVNAMEVAR <- "ADM4_EN"
      }
      
      message("S1. 6 observe level")
      
    })

    #7 observeEvent switch_screen() change leaflet----
    observeEvent(switch_screen(), {
      req(switch_screen() == "screen1")
      req(input$level)
      
      map <- leafletProxy(ns("base_map"), session) 
      map |> clearShapes()
      
      #if (input$level == "nation") {
      if (shared_parameters$level == "nation") {
        message("S1. 7 Leaflet update observeEvent level = nation, screen changed to: ", switch_screen())
        nation_ext <- unlist(unname(as.vector(ext(vect_nation)))) # Extent of nation as unnamed vector
        map |>
          addPolygons(data = vect_nation, color = "blue", weight = 2)%>%
          fitBounds(nation_ext[1], nation_ext[3], nation_ext[2], nation_ext[4])  # Zoom to selected nation
      }
      
      if (input$level == "region" && !is.null(input$region)) {
        message("S1. 7 Leaflet update observeEvent level = region, screen changed to: ", switch_screen())
        region_id <- df_region |> dplyr::filter(ADM1_EN == input$region) |> pull(ADM1_EN)
        region_data <- vect_region |> subset(vect_region$ADM1_EN == region_id)
        region_ext <- unlist(unname(as.vector(ext(region_data)))) # Extent of selected region as unnamed vector
        map |>
          addPolygons(data = region_data, color = "green", weight = 2) |> 
          fitBounds(region_ext[1], region_ext[3], region_ext[2], region_ext[4])  # Zoom to selected region
      }
      
      if (input$level == "zone" && !is.null(input$zone)) {
        message("S1. 7 Leaflet update observeEvent level = zone, screen changed to: ", switch_screen())
        zone_id <- df_zone %>% dplyr::filter(ADM2_EN == input$zone) %>% pull(ADM2_EN)
        zone_data <- vect_zone %>% subset(vect_zone$ADM2_EN == zone_id)
        zone_ext <- unlist(unname(as.vector(ext(zone_data)))) # Extent of selected zone as unnamed vector
        map %>%
          addPolygons(data = zone_data, color = "red", weight = 2) %>% 
          fitBounds(zone_ext[1], zone_ext[3], zone_ext[2], zone_ext[4])  # Zoom to selected zone
      }
      
      if (input$level == "woreda" && !is.null(input$woreda)) {
        message("S1. 7 Leaflet update observeEvent level = woreda, screen changed to: ", switch_screen())
        woreda_id <- df_woreda %>% dplyr::filter(ADM3_EN == input$woreda) %>% pull(ADM3_EN)
        woreda_data <- vect_woreda %>% subset(vect_woreda$ADM3_EN == woreda_id)
        woreda_ext <- unlist(unname(as.vector(ext(woreda_data)))) # Extent of selected woreda as unnamed vector
        map %>%
          addPolygons(data = woreda_data, color = "black", weight = 2) %>% 
          fitBounds(woreda_ext[1], woreda_ext[3], woreda_ext[2], woreda_ext[4])  # Zoom to selected woreda
      }
      
      })
    
    # #8 observeEvent switch_screen() message----
    # observeEvent(switch_screen(), {
    #   message("S1. 8 switch_screen() observeEvent.Screen changed to: ", switch_screen())
    # })
    
    #9 observeEvent auto_reload ----
    observeEvent(input$auto_reload, {
      message("S1. 9 input$auto_reload observeEvent")
      map <- leafletProxy(ns("base_map"), session) 
      map |> fitBounds(50, 51, 0, 1)
    })
    
    # Save and load
    observeEvent(input$save_progress, {
      #save_progress(shared_values, session$token)
      save_progress(shared_values, shared_parameters)
      showNotification("Progress saved!", type = "message")
    })
    
    observeEvent(input$resume_progress, {
      load_progress(shared_values, shared_parameters, session$token)
      showNotification("Progress resumed!", type = "message")
    })
    
    
    
  })
}
