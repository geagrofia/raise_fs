# modules/screen1.R

library(terra)
library(leaflet)


bslib_screen1_module_v3_SidebarUI <- function(id, shared_values) {
  ns <- NS(id)
  tagList(
    # selectInput(ns("my_dropdown"), "Choose a value", choices = c("A", "B", "C"),
    #             selected = shared_values$dropdown  # <-- Persist selected value
    # ),
    radioButtons(ns("level"), "Select level:",
                 choices = c("Nation" = "nation",
                             "Region" = "region",
                             "Zone" = "zone",
                             "Woreda" = "woreda"),
                 selected = shared_values$level ),
    uiOutput(ns("region_ui")),
    uiOutput(ns("zone_ui")),
    uiOutput(ns("woreda_ui")),
    
    # #---new
    # radioButtons(ns("mode"), "Choose mode", choices = c("Option 1" = 1, "Option 2" = 2, "Option 3" = 3, "Option 4" = 4)),
    # uiOutput(ns("dropdown1_ui")),
    # uiOutput(ns("dropdown2_ui")),
    # uiOutput(ns("dropdown3_ui")),
    # 
    # #--- end new
    #actionButton(ns("auto_reload"), "auto_reload"),
    actionButton(ns("to_screen2"), "Go to Screen 2")
  )
}

bslib_screen1_module_v3_MainUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("main_output")), 
    textOutput(ns("main_level_output")),
    textOutput(ns("main_geography_output")),
    leafletOutput(ns("base_map"), height = "500px")
  )
}

bslib_screen1_module_v3_Server <- function(id, shared_values, switch_screen) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # load geojson files and convert to df
    vect_nation <- terra::vect("E:/repos/raise_fs/shiny/data/eth_nation.geojson")
    df_nation <- as.data.frame(vect_nation)
    
    vect_region <- vect("E:/repos/raise_fs/shiny/data/eth_region.geojson")
    df_region <- as.data.frame(vect_region)
    
    vect_zone <- vect("E:/repos/raise_fs/shiny/data/eth_zone_2.geojson")
    df_zone <- as.data.frame(vect_zone)[, c("ADM2_EN", "ADM1_EN")]
    
    vect_woreda <- vect("E:/repos/raise_fs/shiny/data/eth_woreda.geojson")
    df_woreda <- as.data.frame(vect_woreda)[, c("ADM3_EN", "ADM2_EN")]
    
    #0 ---- Render initial map ----
    output$base_map <- renderLeaflet({
      message("0 output$base_map")
      
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
        message("0 output$region_ui")
        selectInput(ns("region"), "Region:",
                    choices = unique(df_region$ADM1_EN), 
                    #selected = isolate(input$region))
                    selected = shared_values$selected_region)
      }
    })

    #0 ----Update zone dropdown UI----
    output$zone_ui <- renderUI({
      req(input$level %in% c("zone", "woreda"))
      req(input$region)
      message("0 output$zone_ui")
      filtered_zones <- unique(df_zone$ADM2_EN[df_zone$ADM1_EN == input$region])
      selectInput(ns("zone"), "Zone:",
        choices = filtered_zones,
        #selected = isolate(input$zone))
        selected = shared_values$selected_zone)
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
      selected = shared_values$selected_woreda)
    })
    
    #0 Update test text in main UI----
    output$main_output <- renderText({
      req(shared_values$dropdown)
      paste("S1. You selected:", shared_values$dropdown)
    })
    
    #0 Update geo level text in main UI----
    output$main_level_output <- renderText({
      req(shared_values$level)
      paste("S1. You selected level:", shared_values$level)
    })
    
    #0 Update geography text in main UI----
    output$main_geography_output <- renderText({
      req(shared_values$level)
      
      if (shared_values$level == "woreda") {
        paste(
          "S1. You selected geography:",
          shared_values$selected_region,
          shared_values$selected_zone,
          shared_values$selected_woreda
        )
      } else {
        if (shared_values$level == "zone") {
          paste(
            "S1. You selected geography:",
            shared_values$selected_region,
            shared_values$selected_zone
          )
        } else {
          if (shared_values$level == "region") {
            paste("S1. You selected geography:",
                  shared_values$selected_region)
          } else {
            paste("S1. You selected geography: Ethiopia")
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
        region_id <- df_region |> filter(ADM1_EN == input$region) |> pull(ADM1_EN)
        region_data <- vect_region |> subset(vect_region$ADM1_EN == region_id)
        region_ext <- unlist(unname(as.vector(ext(region_data)))) # Extent of selected region as unnamed vector
        map |>
          addPolygons(data = region_data, color = "green", weight = 2) |> 
          fitBounds(region_ext[1], region_ext[3], region_ext[2], region_ext[4])  # Zoom to selected region
      }
      
      if (input$level == "zone" && !is.null(input$zone)) {
        message("S1. 1 Leaflet update observe: zone")
        zone_id <- df_zone %>% filter(ADM2_EN == input$zone) %>% pull(ADM2_EN)
        zone_data <- vect_zone %>% subset(vect_zone$ADM2_EN == zone_id)
        zone_ext <- unlist(unname(as.vector(ext(zone_data)))) # Extent of selected zone as unnamed vector
        map %>%
          addPolygons(data = zone_data, color = "red", weight = 2) %>% 
          fitBounds(zone_ext[1], zone_ext[3], zone_ext[2], zone_ext[4])  # Zoom to selected zone
      }
      
      if (input$level == "woreda" && !is.null(input$woreda)) {
        message("S1. 1 Leaflet update observe: woreda")
        woreda_id <- df_woreda %>% filter(ADM3_EN == input$woreda) %>% pull(ADM3_EN)
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
      shared_values$level <- input$level
    })

    #4 observeEvent input$to_screen2 ----
    observeEvent(input$to_screen2, {
      message("S1. 4 input$to_screen2 observeEvent")
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
      shared_values$level <- input$level
      shared_values$selected_nation <- "Ethiopia"
      shared_values$selected_region <- input$region
      shared_values$selected_zone <- input$zone
      shared_values$selected_woreda <- input$woreda
      
      message("S1. 5 observe level and selected geography name from input")
      
    })
    
    
    #6 observe Store selected value in shared reactiveValues (optional) ----
    observe({
      req(input$level)
      if (input$level == "nation") shared_values$selected_nation <- "Ethiopia"
      if (input$level == "region") shared_values$selected_region <- input$region
      if (input$level == "zone")   shared_values$selected_zone <- input$zone
      if (input$level == "woreda") shared_values$selected_woreda <- input$woreda
      
      message("6 observe level")
      
    })

    #7 observeEvent switch_screen() change leaflet----
    observeEvent(switch_screen(), {
      req(switch_screen() == "screen1")
      req(input$level)
      
      map <- leafletProxy(ns("base_map"), session) 
      map |> clearShapes()
      
      #if (input$level == "nation") {
      if (shared_values$level == "nation") {
        message("S1. 7 Leaflet update observeEvent level = nation, screen changed to: ", switch_screen())
        nation_ext <- unlist(unname(as.vector(ext(vect_nation)))) # Extent of nation as unnamed vector
        map |>
          addPolygons(data = vect_nation, color = "blue", weight = 2)%>%
          fitBounds(nation_ext[1], nation_ext[3], nation_ext[2], nation_ext[4])  # Zoom to selected nation
      }
      
      if (input$level == "region" && !is.null(input$region)) {
        message("S1. 7 Leaflet update observeEvent level = region, screen changed to: ", switch_screen())
        region_id <- df_region |> filter(ADM1_EN == input$region) |> pull(ADM1_EN)
        region_data <- vect_region |> subset(vect_region$ADM1_EN == region_id)
        region_ext <- unlist(unname(as.vector(ext(region_data)))) # Extent of selected region as unnamed vector
        map |>
          addPolygons(data = region_data, color = "green", weight = 2) |> 
          fitBounds(region_ext[1], region_ext[3], region_ext[2], region_ext[4])  # Zoom to selected region
      }
      
      if (input$level == "zone" && !is.null(input$zone)) {
        message("S1. 7 Leaflet update observeEvent level = zone, screen changed to: ", switch_screen())
        zone_id <- df_zone %>% filter(ADM2_EN == input$zone) %>% pull(ADM2_EN)
        zone_data <- vect_zone %>% subset(vect_zone$ADM2_EN == zone_id)
        zone_ext <- unlist(unname(as.vector(ext(zone_data)))) # Extent of selected zone as unnamed vector
        map %>%
          addPolygons(data = zone_data, color = "red", weight = 2) %>% 
          fitBounds(zone_ext[1], zone_ext[3], zone_ext[2], zone_ext[4])  # Zoom to selected zone
      }
      
      if (input$level == "woreda" && !is.null(input$woreda)) {
        message("S1. 7 Leaflet update observeEvent level = woreda, screen changed to: ", switch_screen())
        woreda_id <- df_woreda %>% filter(ADM3_EN == input$woreda) %>% pull(ADM3_EN)
        woreda_data <- vect_woreda %>% subset(vect_woreda$ADM3_EN == woreda_id)
        woreda_ext <- unlist(unname(as.vector(ext(woreda_data)))) # Extent of selected woreda as unnamed vector
        map %>%
          addPolygons(data = woreda_data, color = "black", weight = 2) %>% 
          fitBounds(woreda_ext[1], woreda_ext[3], woreda_ext[2], woreda_ext[4])  # Zoom to selected woreda
      }
      
      })
    
    #8 observeEvent switch_screen() message----
    observeEvent(switch_screen(), {
      message("S1. 8 switch_screen() observeEvent.Screen changed to: ", switch_screen())
    })
    
    #9 observeEvent auto_reload ----
    observeEvent(input$auto_reload, {
      message("S1. 9 input$auto_reload observeEvent")
      map <- leafletProxy(ns("base_map"), session) 
      map |> fitBounds(50, 51, 0, 1)
    })
    
    
    
  })
}
