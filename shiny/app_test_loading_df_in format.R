library(dplyr)
library(purrr)
library(shiny)
library(shinyTree)


# Load the CSV file into a data frame
df <- read.csv("E:/repos/raise_fs/shiny/data/test.csv", stringsAsFactors = FALSE)

# Recursive function to build nested list
create_nested_list <- function(df, levels) {
  if (length(levels) == 1) {
    # Base case: Return the column as a named list
    return(setNames(as.list(df[[levels]]), df[[levels]]))
  } else {
    # Group by the first level and recursively build the list
    split_data <- split(df, df[[levels[1]]])
    return(map(split_data, ~ create_nested_list(.x, levels[-1])))
  }
}

# List of levels in hierarchy (change as per your data)
levels <- c("Level1", "Level2", "Level3", "Level4")

# Create the nested list
nested_list <- create_nested_list(df, levels)

print(nested_list)



ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE)
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    testjson
  })
}

shinyApp(ui, server)
