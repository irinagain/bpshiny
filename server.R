#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(DT)
#library(bp)

shinyServer(function(input,output) {
  ######DATA######
  
  
  
  ######MATRIC######
  #add metric based on the parameter it takes in
  parameter_type <- reactive({
    #metric is considered as parameter type "none" if it only requires data as a parameter
    if(input$metric %in% c("arv")){
      return("none")
    }
  })
    #specify first parameter and the default values
    
    #add description of first parameter
    
    output$help_text <- renderUI ({
      parameter_type = parameter_type()
      
      if(parameter_type == "none"){
        helpText("No parameters need to be specified.")
      }
    })
    
    #specify second parameter and its default values
    
    #add description of second parameter
    
    #specify third parameter and its default value
    
    #add description on third parameter
    
    #reactive function

    metric_table <- reactive({
      parameter_type = parameter_type()
      data = transform_data()
      
      if (is.null(input$parameter)) {
        validate(
          need(!is.null(input$parameter), "Please wait - Rendering")
        )
      } else if (grepl(',', input$parameter) & !grepl("\\(", input$parameter)) {
        if (length(strsplit(input$parameter, split = ",")[[1]]) != 2) {
          validate (
            need(parameter_type %in% c("list", "none","time"), "Please wait - Rendering")
          )
        } else {
          validate(
            need(parameter_type %in% c("list", "lwrupr","lwrupr1","none","time"), "Please wait - Rendering")
          )
        }
      } else if (grepl("\\(", input$parameter)) {
        validate(
          need(parameter_type %in% c("nested", "none","time"), "Please wait - Rendering")
        )
      } else if (!grepl(',', input$parameter)) {
        print(input$parameter)
        validate(
          need(parameter_type %in% c("value","value1","value_time", "none","time"), "Please wait - Rendering")
        )
        
      }
      
      #loading bp library and using metric function
      if(is.null(input$parameter) | parameter_type == "none"){
        string = paste("iglu::", input$metric, "(data)", sep = "")
      }
      if (input$filter_sleep_wake) {
        if (parameter_type == "none") {
          out_str = paste0("iglu::calculate_sleep_wake(data, FUN = ", input$metric, ", calculate = \'", input$sleep_or_wake, "\', sleep_start = ", input$sleep_start, ", sleep_end = ", input$sleep_end, ")")
        }
        string = out_str
      }
    eval(parse(text = string))
    
   })


output$metric <- DT::renderDataTable(metric_table(), extensions = "Buttons",
                                     options = list(dom = "Btip",
                                                    buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                    scrollX = TRUE))

  ######PLOT######
  
})
  
  
  