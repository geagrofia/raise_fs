library(shiny)
library(leaflet)
library(dplyr)
library(terra)

vect_nation <- vect("E:/repos/raise_fs/shiny/data/eth_nation.geojson")
#plot(vect_nation)
df_nation <- as.data.frame(vect_nation)

vect_region <- vect("E:/repos/raise_fs/shiny/data/eth_region.geojson")
#plot(vect_region)
df_region <- as.data.frame(vect_region)

vect_zone <- vect("E:/repos/raise_fs/shiny/data/eth_zone_2.geojson")
#plot(vect_zone)
df_zone <- as.data.frame(vect_zone)

vect_woreda <- vect("E:/repos/raise_fs/shiny/data/eth_woreda.geojson")
#plot(vect_woreda)
df_woreda <- as.data.frame(vect_woreda)


# ---- UI ----
# Shiny App
ui <- fluidPage(
  titlePanel("Geographic Modelling Interface"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("geo_level", "Select Geographic Level:",
                   choices = list("Nation" = "nation", "Region" = "region", "Zone" = "zone", "Woreda" = "woreda"),
                   selected = "nation"),
      
      uiOutput("region_dropdown"),
      uiOutput("zone_dropdown"),
      uiOutput("woreda_dropdown"),
      actionButton("next_screen", "Go to Screen 2")
    ),
    
    mainPanel(
      leafletOutput("map", height = 600),
      
      # Text output for selected region/zone/woreda
      textOutput("selected_text")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Reactive values to track selected region and zone ----
  rv <- reactiveValues(
    selected_region = NULL,
    selected_zone = NULL,
    selected_woreda = NULL
  )
  
  # ---- Render dropdown for region dynamically ----
  output$region_dropdown <- renderUI({
    if (input$geo_level == "region" || input$geo_level == "zone" || input$geo_level == "woreda") {
      selectInput("region", "Select Region:",
                  choices = df_region$ADM1_EN,
                  selected = rv$selected_region)
    }
  })
  
  # ---- Render dropdown for zone dynamically ----
  output$zone_dropdown <- renderUI({
    if (input$geo_level == "zone" || input$geo_level == "woreda") {
      req(input$region)  # Ensure region is selected first
      region_id <- df_region %>% filter(ADM1_EN == input$region) %>% pull(ADM1_EN)
      filtered_zones <- df_zone %>% filter(ADM1_EN == region_id)
      
      selectInput("zone", "Select Zone:",
                  choices = filtered_zones$ADM2_EN,
                  selected = rv$selected_zone)
    }
  })
  
  # ---- Render dropdown for woreda dynamically ----
  output$woreda_dropdown <- renderUI({
    if (input$geo_level == "woreda") {
      req(input$zone)  # Ensure zone is selected first
      zone_id <- df_zone %>% filter(ADM2_EN == input$zone) %>% pull(ADM2_EN)
      filtered_woredas <- df_woreda %>% filter(ADM2_EN == zone_id)
      
      selectInput("woreda", "Select Woreda:",
                  choices = filtered_woredas$ADM3_EN,
                  selected = rv$selected_woreda)
    }
  })
  
  # ---- Update reactive values based on dropdown selections ----
  observeEvent(input$region, {
    rv$selected_region <- input$region
    rv$selected_zone <- NULL  # Reset zone if region changes
    rv$selected_woreda <- NULL  # Reset woreda if region changes
  })
  
  observeEvent(input$zone, {
    rv$selected_zone <- input$zone
    rv$selected_woreda <- NULL  # Reset woreda if region changes
  })
  
  observeEvent(input$woreda, {
    rv$selected_woreda <- input$woreda
  })  
  
  
  # ---- Render text output for selected region/zone ----
  output$selected_text <- renderText({
    if (input$geo_level == "nation") {
      "Nation is selected."
    } else if (input$geo_level == "region" && !is.null(rv$selected_region)) {
      paste("Selected Region:", rv$selected_region)
    } else if (input$geo_level == "zone" && !is.null(rv$selected_zone)) {
      paste("Selected Region:", rv$selected_region, "| Selected Zone:", rv$selected_zone)
    } else if (input$geo_level == "woreda" && !is.null(rv$selected_woreda)) {
      paste("Selected Region:", rv$selected_region, "| Selected Zone:", rv$selected_zone, "| Selected Woreda:", rv$selected_woreda)
    } else {
      "No selection made."
    }
  })
  
  
  
  # ---- Render map ----
  output$map <- renderLeaflet({
    nation_ext <- unlist(unname(as.vector(ext(vect_nation)))) # Extent of nation as unnamed vector
    leaflet() %>%
      addTiles() %>%
      fitBounds(nation_ext[1], nation_ext[3], nation_ext[2], nation_ext[4])  # Zoom to selected nation
  })
  
  
  # ---- Update map based on selections ----
  observe({
    map <- leafletProxy("map")
    map %>% clearShapes()
    
    if (input$geo_level == "nation") {
      
      nation_ext <- unlist(unname(as.vector(ext(vect_nation)))) # Extent of nation as unnamed vector
      map %>%
        addPolygons(data = vect_nation, color = "blue", weight = 2)%>%
        fitBounds(nation_ext[1], nation_ext[3], nation_ext[2], nation_ext[4])  # Zoom to selected nation
      
    } else if (input$geo_level == "region" && !is.null(input$region)) {
      region_id <- df_region %>% filter(ADM1_EN == input$region) %>% pull(ADM1_EN)
      region_data <- vect_region %>% subset(vect_region$ADM1_EN == region_id)
      region_ext <- unlist(unname(as.vector(ext(region_data)))) # Extent of selected region as unnamed vector
      map %>%
        addPolygons(data = region_data, color = "green", weight = 2) %>% 
        fitBounds(region_ext[1], region_ext[3], region_ext[2], region_ext[4])  # Zoom to selected region
      
    } else if (input$geo_level == "zone" && !is.null(input$zone)) {
      zone_id <- df_zone %>% filter(ADM2_EN == input$zone) %>% pull(ADM2_EN)
      zone_data <- vect_zone %>% subset(vect_zone$ADM2_EN == zone_id)
      zone_ext <- unlist(unname(as.vector(ext(zone_data)))) # Extent of selected zone as unnamed vector
      map %>%
        addPolygons(data = zone_data, color = "red", weight = 2) %>% 
        fitBounds(zone_ext[1], zone_ext[3], zone_ext[2], zone_ext[4])  # Zoom to selected zone
      
    } else if (input$geo_level == "woreda" && !is.null(input$woreda)) {
      woreda_id <- df_woreda %>% filter(ADM3_EN == input$woreda) %>% pull(ADM3_EN)
      woreda_data <- vect_woreda %>% subset(vect_woreda$ADM3_EN == woreda_id)
      woreda_ext <- unlist(unname(as.vector(ext(woreda_data)))) # Extent of selected woreda as unnamed vector
      map %>%
        addPolygons(data = woreda_data, color = "black", weight = 2) %>% 
        fitBounds(woreda_ext[1], woreda_ext[3], woreda_ext[2], woreda_ext[4])  # Zoom to selected woreda
    }
  })
  
  # ---- Navigate to Screen 2 ----
  observeEvent(input$next_screen, {
    if (input$geo_level == "region" & is.null(rv$selected_region)) {
      showNotification("Please choose a region before moving to the next screen.", type = "error")
    } else if (input$geo_level == "zone" & is.null(rv$selected_zone)) {
      showNotification("Please choose a zone before moving to the next screen.", type = "error")
    } else if (input$geo_level == "woreda" & is.null(rv$selected_woreda)) {
      showNotification("Please choose a woreda before moving to the next screen.", type = "error")
    } else {
      showNotification("Ready to go the next screen.", type = "message")
      #updateTabsetPanel(session, "main_tabs", selected = "Screen 2")
    }
  })
}


shinyApp(ui, server)
