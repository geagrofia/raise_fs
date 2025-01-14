# Load required libraries
library(shiny)
library(leaflet)
library(shinyTree)




# Define UI
ui <- pageWithSidebar(
  
  # Application title
  headerPanel("Simple shinyTree!"),
  
  sidebarPanel(
    helpText(HTML("A simple Shiny Tree example.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
  ),
  mainPanel(
    # Show a simple table.
    shinyTree("tree", stripes = TRUE, multiple = FALSE, animation = FALSE)
  )
)


# Define server logic
server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
        Adoption = list(
        BAL = list(
          BA = list(
            CA = list(
              PA = list(
                PT = "", 
                PM1 = "", 
                PM2 = "",
                PM3 = ""), 
              TA = list(
                TMT = "", 
                TXT = "", 
                TNT = "")), 
            SPA = "", 
            SFA ="",
            LsA =""), 
          LU = ""),
        SEF = list(leaf1 = "", 
                   leaf2 = "", 
                   leaf3="")
      )
    )
  })
}

# Run the application
shinyApp(ui, server)
