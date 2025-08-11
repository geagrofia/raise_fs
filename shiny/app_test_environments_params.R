ui <- fluidPage(
  
  actionButton("render_text_btn", "Run Test"),
  downloadButton("run_IRM1_btn", "Download IRM")
  
)

server <- function(input, output, session) {
  observeEvent(input$render_btn, {
    # Create a clean environment shared between helpers.R and report.Rmd
    shared_env <- new.env(parent = globalenv())
    
    # Render using that environment
    rmarkdown::render("E:/repos/raise_fs/code/rmd/test_environments.Rmd",
                      output_file = "E:/repos/raise_fs/code/rmd/test_environments.html",
                      envir = shared_env)
  })
  
  # downloadHandler run_IRM1_btn ----
  output$run_IRM1_btn  <- downloadHandler(
    filename = "E:/repos/raise_fs/code/rmd/test_environments_params.html",
    content = function(file) {
      withProgress(value = 0, message = 'Starting, this may take some time', {
        params_irm <- list(
          INT = "1",
          SYS = NA,
          Agg = 1,
          MASK = 5000,
          EXT = "Ethiopia",
          DIVCODEVAR =  'ADM3_CODE',
          DIVCODEVAL =  30801,
          DIVNAMEVAR =  "ADM3_EN",
          SUBDIVNAMEVAR =  "ADM4_EN",
          INN1 =  "pearl_millet_generic_ziquala",
          RES1 =  3,
          SOS1 =  0,
          FAOCLASS1 =  3,
          LIMITS1 =  3,
          CONCCLASS1 =  3,
          TRIAD1 =  "Adoption",
          TRIBA1 =  "Aptitude",
          TRISE1 =  "Feasible",
          INN2 = NA
        )
        
        shared_env <- new.env(parent = globalenv())
        
        rmarkdown::render(
          input = "E:/repos/raise_fs/code/rmd/report_template_shared_environment.Rmd",
          output_file = file,
          params = params_irm,
          envir = shared_env
        )
      })
    }
  )
  
  
  
  
  
}
shinyApp(ui, server)