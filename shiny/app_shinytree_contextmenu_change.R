library(shiny)
library(shinyTree)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  actionButton("run", "Change Contextmenu"),
  hr(),
  shinyTree("tree", contextmenu = TRUE, unique = TRUE, sort = TRUE)
)

server <- function(input, output, session) {
  observeEvent(input$run, {
    runjs(HTML('
      function extend(obj, src) {
  	    for (var key in src) {
  	        if (src.hasOwnProperty(key)) obj[key] = src[key];
  	    }
  	    return obj;
      }
  	
      var newctxt = {
        "openall" : {
          "separator_before"  : false,
          "separator_after"   : true,
          "_disabled"         : false, //(this.check("create_node", data.reference, {}, "last")),
          "label"             : "Open All nodes",
          "action"            : function (data) {
            var inst = $.jstree.reference(data.reference),
                obj = inst.get_node(data.reference);
            inst.open_all()
          }
        },
        "closeall" : {
          "separator_before"  : false,
          "separator_after"   : true,
          "_disabled"         : false, //(this.check("create_node", data.reference, {}, "last")),
          "label"             : "Close All nodes",
          "action"            : function (data) {
            var inst = $.jstree.reference(data.reference),
                obj = inst.get_node(data.reference);
            inst.close_all()
          }
        }
    };
    
    var defaultctxt = $.jstree.defaults.contextmenu.items();
    
    $("#tree").jstree().settings.contextmenu.items = extend(defaultctxt, newctxt)
'))  
  })
  
  output$tree <- renderTree({
    list(
      root1 = "",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      ),
      root3 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
}

shinyApp(ui, server)