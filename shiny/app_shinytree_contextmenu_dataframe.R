# Load required libraries

library(data.tree)
library(tidyverse)
library(jsonlite)
library(shiny)
library(leaflet)
library(shinyTree)
library(igraph)


jstree_list <- list(
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
  ))




df_f <- read.csv("E:/repos/raise_fs/tab_data/input/requirements_pearl_millet_lu_link.csv")
df_f
df_f_nested = df_f %>% nest(crit_code = crit_code)
df_f_nested
df_f_nested %>% pull(crit_code)

df_f_nested_json <- 
  df_f_nested %>%
  rowwise() %>%
  mutate(crit_code = toJSON(crit_code))
df_f_nested_json

df_f_nested_list <- 
  df_f_nested %>%
  rowwise() %>%
  mutate(crit_code = list(pull(crit_code, crit_code)))
df_f_nested_list


list_f <- split(df_f$crit_code, df_f$stack_code)



###
# FROM CHATPGPT

df <- df_f

# Function to build a nested list using criterion names
build_nested_list <- function(df, parent) {
  # Find the children of the current parent
  children <- df[df$stack_code == parent, c("crit_code", "criterion")]

  # If no children, return an empty string
  if (nrow(children) == 0) {
    return("")
  }

  # For each child, recursively call the function
  result <- setNames(lapply(children$crit_code, function(child) build_nested_list(df, child)), children$criterion)

  return(result)
}

# Identify the top-level parent(s)
top_level_parents <- setdiff(df$stack_code, df$crit_code)

# Map top-level parents to their criterion
top_level_names <- df[df$stack_code %in% top_level_parents, c("stack_code", "criterion")]
top_level_names <- unique(top_level_names) # Ensure unique rows in case of duplicates

# Build the nested list for each top-level parent
nested_list <- setNames(lapply(top_level_names$stack_code, function(parent) build_nested_list(df, parent)),
                        top_level_names$criterion)

# Print the result
print(nested_list)

###


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
  output$tree <- renderTree(nested_list)
}




# Run the application
shinyApp(ui, server)

