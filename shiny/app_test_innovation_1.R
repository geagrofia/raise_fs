library(shiny)
library(leaflet)
library(dplyr)
library(terra)

df_inn <- read.csv("E:/repos/raise_fs/shiny/data/inn.csv")


# ---- UI ----
# Shiny App
ui <- fluidPage(
  titlePanel("Select Innovation(s)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("inn_num", "Number of innovations to be modelled:",
                   choices = list("One" = "one", "Two" = "two"),
                   selected = "one"),
      
      uiOutput("one_dropdown"), # Dynamic UI for innovation 1 dropdown menu
      uiOutput("add_inn_1"), # Dynamic UI for add innovation 1 action button
      uiOutput("input_fields_inn_1"), # Dynamic UI for innovation 1 text inputs and save button
      
      uiOutput("two_dropdown"), # Dynamic UI for innovation 2 dropdown menu
      uiOutput("add_inn_2"), # Dynamic UI for add innovation 2 action button
      uiOutput("input_fields_inn_2"), # Dynamic UI for innovation 2 text inputs and save button
      uiOutput("inn_sys"), # Dynamic UI for system for two innovations
      
      actionButton("next_screen", "Go to Screen 2")
    ),
    
    mainPanel(
      
      
      # Text output for selected region/zone/woreda
      textOutput("selected_text")
    )
  )
)


server <- function(input, output, session) {
  # ---- Reactive values to track selected innovation and system ----
  rv <- reactiveValues(
    selected_inn_1  =  NULL,
    selected_inn_2 =  NULL,
    selected_sys =  NULL,
    
    table_data = df_inn,
    show_inputs  =  FALSE
  )
  
  # ---- Render dropdown for innovation one ----
  output$one_dropdown <- renderUI({
    selectInput(
      "innovation_1",
      "Select Innovation 1:",
      choices = paste0(df_inn$common_name, " (", df_inn$scientific_name, ")"),
      selected = rv$selected_inn_1
    )
    
  })
  
  # ---- Render action button for new second innovation dynamically ----
  output$add_inn_1 <- renderUI({
    if (input$inn_num == "one") {
      actionButton("add_inn_1", "Add new innovation 1")
    }
  })
  
  # ---- Prepare table for editing ----
  observeEvent(input$add_inn_1, {
    current_data <- rv$table_data
    print(current_data)
    #new_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(current_data)))
    #colnames(new_row) <- colnames(current_data)
    #updated_data <- rbind(current_data, new_row)
    #print(updated_data)
    #rv$table_data <- updated_data
  })
  
  
  # Show input fields when "Add Row" button is pressed
  observeEvent(input$add_inn_1, {
    rv$show_inputs = TRUE
  })
  
  # Render the dynamic input fields and save button
  output$input_fields_inn_1 <- renderUI({
    if (rv$show_inputs) {
      tagList(
        textInput("column2_inn_1", "Value for Column 2", ""),
        textInput("column3_inn_1", "Value for Column 3", ""),
        actionButton("save_inputs_inn_1", "Save Inputs")
      )
    }
  })
  
  # Save inputs and add a row to the table
  observeEvent(input$save_inputs_inn_1, {
    req(rv$table_data) # Ensure there's existing data
    current_data <- rv$table_data
    print(current_data)
    # Automatically generate the new ID
    new_id <- if (nrow(current_data) == 0) 1 else max(current_data[[1]], na.rm = TRUE) + 1
    print(new_id)
    # Get values from inputs for columns 2 and 3
    new_col2 <- input$column2_inn_1
    print(new_col2)
    new_col3 <- input$column3_inn_1
    print(new_col3)
    # Create a new row
    new_row <- data.frame(
      Column1 = new_id,
      Column2 = new_col2,
      Column3 = new_col3,
      stringsAsFactors = FALSE
    )
    print(new_row)
    # Ensure the new row matches the column names of the existing data
    colnames(new_row) <- colnames(current_data)
    
    # Append the new row and update the data
    updated_data <- rbind(current_data, new_row)
    rv$table_data <- updated_data
    print(updated_data)
    # Hide inputs after saving and clear the input fields
    rv$show_inputs <- FALSE
    updateTextInput(session, "column2_inn_1", value = "")
    updateTextInput(session, "column3_inn_1", value = "")
  })


  
  # ---- Update reactive values based on dropdown selections ----
  observeEvent(input$innovation_1, {
    rv$selected_inn_1 <- input$innovation_1
  })
  
  # ---- Render dropdown for innovation two dynamically ----
  output$two_dropdown <- renderUI({
    if (input$inn_num == "two" ) {
      selectInput(
        "innovation_2",
        "Select Innovation 2:",
        choices = paste0(df_inn$common_name, " (", df_inn$scientific_name, ")"),
        selected = rv$selected_inn_2
      )
    }
  })
  
  # ---- Update reactive values based on dropdown selections ----
  observeEvent(input$innovation_2, {
    rv$selected_inn_2 <- input$innovation_2
  })
  
  
  # ---- Render radio button for system dynamically ----
  output$inn_sys <- renderUI({
    if (input$inn_num == "two") {
      radioButtons(
        "inn_sys",
        "System:",
        choices = list("Intercrop / Rotation" = "intercrop", "Compare" = "compare"),
        selected = "intercrop"
      )
    }
  })
  
  # ---- Update reactive values based on dropdown selections ----
  observeEvent(input$inn_sys, {
    rv$selected_sys <- input$inn_sys
  })
  
  # ---- Render action button for new second innovation dynamically ----
  output$add_inn_2 <- renderUI({
    if (input$inn_num == "two") {
      actionButton("add_inn_2", "Add new innovation 2")
    }
  })
  
  
  # ---- Render text output for selected innovations ----
  output$selected_text <- renderText({
     if (input$inn_num == "one" && !is.null(rv$selected_inn_1)) {
      paste("Innovation:", rv$selected_inn_1)
    } else if (input$inn_num == "two" && !is.null(rv$selected_inn_1) && !is.null(rv$selected_inn_2)) {
      paste("Selected System:", rv$selected_sys, "| Innovation 1:", rv$selected_inn_1, "| Innovation 2:", rv$selected_inn_2)
    }  else {
      "No selection made."
    }
  })
  
  # ---- Navigate to Screen 2 ----
  observeEvent(input$next_screen, {
          showNotification("Ready to go the next screen.", type = "message")
      #updateTabsetPanel(session, "main_tabs", selected = "Screen 2")
    })
}
shinyApp(ui, server)