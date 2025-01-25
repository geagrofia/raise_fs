library(shiny)
library(DT)

#Load your CSV file
load_data <- function() {
  data <- read.csv("E:/repos/raise_fs/shiny/data/inn.csv")
  return(data)
}

# # Load your CSV file
# load_data <- function() {
#   data <- data.frame(
#     ID = 1:5,
#     Column2 = c("A", "B", "C", "D", "E"),
#     Column3 = c("A", "B", "C", "D", "E"),
#     Column4 = c("A", "B", "C", "D", "E"),
#     Column5 = c("X", "Y", "Z", "W", "V")
#   )
#   return(data)
# }

# Define UI
ui <- fluidPage(
  titlePanel("Shiny Table Interface - Single Screen"),
  
  # Layout with sidebar
  sidebarLayout(
    sidebarPanel(
      h3("Table"),
      DTOutput("data_table"),
      actionButton("select_row", "Select Innovation"),
      actionButton("duplicate_row", "Add New Innovation based on selection"),
      actionButton("add_row", "Add New Innovation"),
      width = 6
    ),
    mainPanel(
      h3("Details"),
      uiOutput("row_details"),
      uiOutput("edit_controls"),
      width = 6
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  # Reactive data frame to store the CSV data
  data <- reactiveVal(load_data())
  selected_row <- reactiveVal(NULL)
  editing_mode <- reactiveVal(NULL)  # Track the current action: "select", "duplicate", "add"
  
  # Render the data table
  output$data_table <- renderDT({
    datatable(data(), selection = "single", editable = FALSE)
  }, server = FALSE)
  
  # Handle row selection (Option 1: View without editing)
  observeEvent(input$select_row, {
    req(input$data_table_rows_selected)
    selected_row(data()[input$data_table_rows_selected, ])
    editing_mode("view")  # Set to "view" mode
  })
  
  # Handle duplicate row (Option 2: Edit after duplication)
  observeEvent(input$duplicate_row, {
    req(input$data_table_rows_selected)
    new_row <- data()[input$data_table_rows_selected, ]
    new_row$ID <- max(data()$ID) + 1
    selected_row(new_row)
    editing_mode("edit")  # Set to "edit" mode
  })
  
  # Handle add new row (Option 3: Add a blank row to edit)
  observeEvent(input$add_row, {
    new_row <- data.frame(
      ID = max(data()$ID) + 1,
      common_name = "",
      scientific_name = "",
      ideotype = "",
      scenario = ""
    )
    selected_row(new_row)
    editing_mode("edit")  # Set to "edit" mode
  })
  
  # Render row details (for "view" mode)
  output$row_details <- renderUI({
    req(selected_row())
    if (editing_mode() == "view") {
      tagList(
        h4("Selected Row Values"),
        verbatimTextOutput("selected_values")
      )
    }
  })
  
  # Display selected row's values (read-only)
  output$selected_values <- renderPrint({
    req(selected_row())
    selected_row()
  })
  
  # Render editable inputs (for "edit" mode)
  output$edit_controls <- renderUI({
    req(selected_row())
    if (editing_mode() == "edit") {
      row <- selected_row()
      tagList(
        h4("Edit Row"),
        textInput("edit_common_name", "Column 2", row$common_name),
        textInput("edit_scientific_name", "Column 3", row$scientific_name),
        textInput("edit_ideotype", "Column 4", row$ideotype),
        textInput("edit_scenario", "Column 5", row$scenario),
        actionButton("save_row", "Save"),
        actionButton("cancel", "Cancel")
      )
    }
  })
  
  # Save edited row and validate (for duplication or adding new rows)
  observeEvent(input$save_row, {
    edited_row <- data.frame(
      ID = selected_row()$ID,
      common_name = input$edit_common_name,
      scientific_name = input$edit_scientific_name,
      ideotype = input$edit_ideotype,
      scenario = input$edit_scenario
    )
    
    # Validation: Check for duplicates in columns 2-5
    all_data <- data()
    duplicate_check <- all_data %>%
      filter(
        common_name == edited_row$common_name &
          scientific_name == edited_row$scientific_name &
          ideotype == edited_row$ideotype &
          scenario == edited_row$scenario &
          ID != edited_row$ID
      )
    
    if (nrow(duplicate_check) > 0) {
      showModal(modalDialog(
        title = "Validation Error",
        "Duplicate values detected. Please modify the values.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      # Save the row
      all_data <- all_data[all_data$ID != edited_row$ID, ]
      all_data <- rbind(all_data, edited_row)
      data(all_data)
      editing_mode(NULL)  # Reset editing mode
      selected_row(NULL)  # Clear selection
    }
  })
  
  # Cancel editing and clear the UI
  observeEvent(input$cancel, {
    editing_mode(NULL)  # Reset editing mode
    selected_row(NULL)  # Clear selection
  })
}

# Run the application
shinyApp(ui = ui, server = server)
