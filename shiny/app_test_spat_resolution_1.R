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
  titlePanel("Spatial resolution"),
  
  sidebarLayout(
    sidebarPanel(
      
      # numericInput for entering value for spatial resolution of the mask
      numericInput(
        "resolution",
        "Resolution (m)",
        100,
        min = 10,
        max = 10000,
        step = 10,
        width = NULL
      ),

      # numericInput for entering value for aggregation level
      numericInput(
        "aggregation",
        "Aggregation level",
        1,
        min = 1,
        max = 100,
        step = 1,
        width = NULL
      ),
      
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
  
    output$value <- renderText({ input$resolution })
    output$value <- renderText({ input$aggregation })
    
  # ---- Navigate to Screen 2 ----
  observeEvent(input$next_screen, {
      showNotification("Ready to go the next screen.", type = "message")
    })
  }

shinyApp(ui, server)