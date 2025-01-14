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
    shinyTree("tree", stripes = TRUE, multiple = FALSE, animation = FALSE, contextmenu = T, dragAndDrop = T, themeDots = T, unique = T, themeIcons = T )
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
                PT = "1",
                PM1 = "2",
                PM2 = "3",
                PM3 = "4"),
              TA = list(
                TMT = "5",
                TXT = "6",
                TNT = "7")),
            SPA = "8",
            SFA ="9",
            LsA ="10"),
          LU = "11"),
        SEF = list(leaf1 = "12",
                   leaf2 = "13",
                   leaf3="14")
      )
    )
  })
}


  
  
# Run the application
shinyApp(ui, server)
