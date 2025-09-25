library(shiny)
library(bslib)
library(tidyverse)
library(shinylogs)
library(shinyjs)
library(data.table)
library(DT)
library(digest)
library(fontawesome)

source("E:/repos/raise_fs/shiny/modules/helpers.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen0_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen1_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen2_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen3_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen4_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen5_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen6_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen7_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen8_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen9_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen10_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen11_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen12_module_v3.R")
source("E:/repos/raise_fs/shiny/modules/bslib_screen13_module_v3.R")


ui <- fluidPage(
  
  useShinyjs(), # for disabling/enabling the actionbuttons
  
  # use_tracking(),
  theme = bs_theme(version = 5, bootswatch = "united"),
  
  # Application title
  titlePanel("IRM Setup"),
  
  sidebarLayout(
    sidebarPanel(uiOutput("dynamic_sidebar"), width = 7),
    mainPanel(uiOutput("dynamic_main"), width = 5)
  )
)

server <- function(input, output, session) {
  
  # track_usage(
  #   storage_mode =  store_json("E:/repos/raise_fs/shiny/shinylogs_app_bslib_text_v3.json"),  # or "json", "db"
  #   what = "input"       # this is the key part
  # )
  
  current_screen <- reactiveVal("screen0")

  
  # there are two sets of variables and their values that are shared:
  # (1) those that are parameters in the IRM model, these also define the case
  # (2) debugging or technical variables which are not used in the IRM model
  
  # set the IRM model parameter values
  
  shared_parameters <- reactiveValues(
    # geography details
    level = NULL,
    selected_nation = NULL,
    selected_region = NULL,
    selected_zone = NULL,
    selected_woreda = NULL, 
    # spatial resolution system details
    resolution = NULL, 
    aggregation = NULL, 
    # innovation system details
    num_innovations = NULL, 
    innovation_system = NULL, 
    # innovation #1 details
    crop_name_1 = NULL,
    ideotype_1 = NULL,
    scenario_1 = NULL,
    # innovation #2 details
    crop_name_2 = NULL,
    ideotype_2 = NULL,
    scenario_2 = NULL,
    triangulation = 0,
    services_local = 0,
    services_woreda = 0,
    services_zone = 0,
    markets_local = 0,
    markets_woreda = 0,
    markets_zone = 0,
    sos1 = 0,
    ZONCODEVAR = NULL,
    ZONCODEVAL = NULL,
    DIVCODEVAR = NULL,
    DIVCODEVAL = NULL,
    DIVNAMEVAR = NULL,
    SUBDIVNAMEVAR = NULL,
    sowdate1 = 190
  )
  
  shared_values <- reactiveValues(
    # progress
    step = 0,
    # counter used internally on screen 4
    forget = 0, 
    # code for showing whether innovation 1 is "existing", "duplicate" or "new" innovation
    inn_type_1 = "existing",
    # code for showing whether innovation 2 is "existing", "duplicate" or "new" innovation
    inn_type_2 = "existing",
    # used for creating a new requirements file based on duplicates
    crop_name_0 = NULL,
    ideotype_0 = NULL,
    scenario_0 = NULL,
    current_tree = NULL
    )
  

  switch_screen <- function(screen) {
    current_screen(screen)
  }
  
  output$dynamic_sidebar <- renderUI({
    if (current_screen() == "screen0") {
      bslib_screen0_module_v3_SidebarUI("screen0", shared_values, shared_parameters)
    } else if (current_screen() == "screen1") {
      bslib_screen1_module_v3_SidebarUI("screen1", shared_values, shared_parameters)
    } else if (current_screen() == "screen2") {
      bslib_screen2_module_v3_SidebarUI("screen2", shared_values, shared_parameters)
    } else if (current_screen() == "screen3") {
      bslib_screen3_module_v3_SidebarUI("screen3", shared_values, shared_parameters)
    } else if (current_screen() == "screen4") {
      bslib_screen4_module_v3_SidebarUI("screen4", shared_values, shared_parameters)
    } else if (current_screen() == "screen5") {
      bslib_screen5_module_v3_SidebarUI("screen5", shared_values, shared_parameters)
    } else if (current_screen() == "screen6") {
      bslib_screen6_module_v3_SidebarUI("screen6", shared_values, shared_parameters)
    } else if (current_screen() == "screen7") {
      bslib_screen7_module_v3_SidebarUI("screen7", shared_values, shared_parameters)
    } else if (current_screen() == "screen8") {
      bslib_screen8_module_v3_SidebarUI("screen8", shared_values, shared_parameters)
    } else if (current_screen() == "screen9") {
      bslib_screen9_module_v3_SidebarUI("screen9", shared_values, shared_parameters)
    } else if (current_screen() == "screen10") {
      bslib_screen10_module_v3_SidebarUI("screen10", shared_values, shared_parameters)
    } else if (current_screen() == "screen11") {
      bslib_screen11_module_v3_SidebarUI("screen11", shared_values, shared_parameters)
    } else if (current_screen() == "screen12") {
      bslib_screen12_module_v3_SidebarUI("screen12", shared_values, shared_parameters)
    } else if (current_screen() == "screen13") {
      bslib_screen13_module_v3_SidebarUI("screen13", shared_values, shared_parameters)
    }
  })
  
  output$dynamic_main <- renderUI({
    if (current_screen() == "screen0") {
      bslib_screen0_module_v3_MainUI("screen0")
    } else if (current_screen() == "screen1") {
      bslib_screen1_module_v3_MainUI("screen1")
    } else if (current_screen() == "screen2") {
      bslib_screen2_module_v3_MainUI("screen2")
    } else if (current_screen() == "screen3") {
      bslib_screen3_module_v3_MainUI("screen3")
    } else if (current_screen() == "screen4") {
      bslib_screen4_module_v3_MainUI("screen4")
    } else if (current_screen() == "screen5") {
      bslib_screen5_module_v3_MainUI("screen5")
    } else if (current_screen() == "screen6") {
      bslib_screen6_module_v3_MainUI("screen6")
    } else if (current_screen() == "screen7") {
      bslib_screen7_module_v3_MainUI("screen7")
    } else if (current_screen() == "screen8") {
      bslib_screen8_module_v3_MainUI("screen8")
    } else if (current_screen() == "screen9") {
      bslib_screen9_module_v3_MainUI("screen9")
    } else if (current_screen() == "screen10") {
      bslib_screen10_module_v3_MainUI("screen10")
    } else if (current_screen() == "screen11") {
      bslib_screen11_module_v3_MainUI("screen11")
    } else if (current_screen() == "screen12") {
      bslib_screen12_module_v3_MainUI("screen12")
    } else if (current_screen() == "screen13") {
      bslib_screen13_module_v3_MainUI("screen13")
    }
  })
  
  bslib_screen0_module_v3_Server("screen0", shared_values, shared_parameters, switch_screen)
  bslib_screen1_module_v3_Server("screen1", shared_values, shared_parameters, switch_screen)
  bslib_screen2_module_v3_Server("screen2", shared_values, shared_parameters, switch_screen)
  bslib_screen3_module_v3_Server("screen3", shared_values, shared_parameters, switch_screen)
  bslib_screen4_module_v3_Server("screen4", shared_values, shared_parameters, switch_screen)
  bslib_screen5_module_v3_Server("screen5", shared_values, shared_parameters, switch_screen)
  bslib_screen6_module_v3_Server("screen6", shared_values, shared_parameters, switch_screen)  
  bslib_screen7_module_v3_Server("screen7", shared_values, shared_parameters, switch_screen)
  bslib_screen8_module_v3_Server("screen8", shared_values, shared_parameters, switch_screen)
  bslib_screen9_module_v3_Server("screen9", shared_values, shared_parameters, switch_screen) 
  bslib_screen10_module_v3_Server("screen10", shared_values, shared_parameters, switch_screen)
  bslib_screen11_module_v3_Server("screen11", shared_values, shared_parameters, switch_screen)
  bslib_screen12_module_v3_Server("screen12", shared_values, shared_parameters, switch_screen)
  bslib_screen13_module_v3_Server("screen13", shared_values, shared_parameters, switch_screen)
}

shinyApp(ui, server)
