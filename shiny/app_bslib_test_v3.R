library(shiny)
library(bslib)
library(tidyverse)
library(shinylogs)

source("E:/repos/raise_fs/shiny/modules/bslib_screen1_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen2_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen3_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen4_module_v3.R")

ui <- fluidPage(
  # use_tracking(),
  theme = bs_theme(version = 5, bootswatch = "united"),
  sidebarLayout(
    sidebarPanel(uiOutput("dynamic_sidebar")),
    mainPanel(uiOutput("dynamic_main"))
  )
)

server <- function(input, output, session) {
  
  # track_usage(
  #   storage_mode =  store_json("E:/repos/raise_fs/shiny/shinylogs_app_bslib_text_v3.json"),  # or "json", "db"
  #   what = "input"       # this is the key part
  # )
  
  current_screen <- reactiveVal("screen1")
  shared_values <- reactiveValues(dropdown = NULL)
  shared_values <- reactiveValues(level = NULL)
  shared_values <- reactiveValues(selected_nation = NULL)
  shared_values <- reactiveValues(selected_region = NULL)
  shared_values <- reactiveValues(selected_zone = NULL)
  shared_values <- reactiveValues(selected_woreda = NULL)
  
  shared_values <- reactiveValues(resolution = NULL)
  shared_values <- reactiveValues(aggregation = NULL)
  
  shared_values <- reactiveValues(num_innovations = NULL)
  shared_values <- reactiveValues(innovation_system = NULL)
  
  
  switch_screen <- function(screen) {
    current_screen(screen)
  }
  
  output$dynamic_sidebar <- renderUI({
    if (current_screen() == "screen1") {
      bslib_screen1_module_v3_SidebarUI("screen1", shared_values)
    } else if (current_screen() == "screen2") {
      bslib_screen2_module_v3_SidebarUI("screen2", shared_values)
    } else if (current_screen() == "screen3") {
      bslib_screen3_module_v3_SidebarUI("screen3", shared_values)
    } else if (current_screen() == "screen4") {
      bslib_screen4_module_v3_SidebarUI("screen4", shared_values)
    }
  })
  
  output$dynamic_main <- renderUI({
    if (current_screen() == "screen1") {
      bslib_screen1_module_v3_MainUI("screen1")
    } else if (current_screen() == "screen2") {
      bslib_screen2_module_v3_MainUI("screen2")
    } else if (current_screen() == "screen3") {
      bslib_screen3_module_v3_MainUI("screen3")
    } else if (current_screen() == "screen4") {
      bslib_screen4_module_v3_MainUI("screen4")
    }
  })
  
  bslib_screen1_module_v3_Server("screen1", shared_values, switch_screen)
  bslib_screen2_module_v3_Server("screen2", shared_values, switch_screen)
  bslib_screen3_module_v3_Server("screen3", shared_values, switch_screen)
  bslib_screen4_module_v3_Server("screen4", shared_values, switch_screen)
}

shinyApp(ui, server)
