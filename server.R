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
  observeEvent(input$submit,{
    #Creates textInput() based on what column names were selected 
    output$new1 <- renderUI({
      if(!'date' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("date", "Date")
      }
    })
    output$new2 <- renderUI({
      if(!'id' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("id", "ID")
      }
    })
    output$new3 <- renderUI({
      if(!'wake' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("wake", "Wake")
      }
    })
    output$new4 <- renderUI({
      if(!'visit' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("visit", "Visit")
      }
    })
    output$new5 <- renderUI({
      if(!'heart' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("hr", "Heart Rate")
      }
    })
    output$new6 <- renderUI({
      if(!'pp' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("pp", "Pulse Pressure")
      }
    })
    output$new7 <- renderUI({
      if(!'map' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("map", "Mean Arterial Pressure")
      }
    })
    output$new8 <- renderUI({
      if(!'rpp' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("rpp", "Rate Pulse Pressure")
      }
    })
    output$new9 <- renderUI({
      if(!'dow' %in% input$bpcolnames){
        return(NULL)
      }else{
        textInput("dow", "Day of the Week")
      }
    })
  })
  ## Displaying Data Table
  output$contents <- renderTable({
    file <- input$file1
    
    #Ensuring file extension is .csv
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    #Assigning data to variable 'bpdata1'
    bpdata1 = read.csv(file$datapath,header=T)
    
    #Transforming Variable names to usable form 
    sys = input$sys
    dias = input$dias
    date = input$date
    id = input$id
    wake = input$wake
    visit = input$visit
    hr = input$hr
    pp = input$pp
    map = input$map
    rpp = input$rpp
    dow = input$dow
    
    #Using process_data function
    bpdata1 = process_data(data = bpdata1,sbp = sys,dbp = dias,date_time = date,id = id, wake = wake, visit = visit,
                           hr=hr,pp=pp,map=map,rpp=rpp,DoW=dow)
    
    bpdata1
    
  })
  
  
  ######METRICS######
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
    data = output$contents(bpdata1)
    
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
      string = paste("bp::", input$metric, "(data)", sep = "")
    }
    if (input$filter_sleep_wake) {
      if (parameter_type == "none") {
        out_str = paste0("bp::calculate_sleep_wake(data, FUN = ", input$metric, ", calculate = \'", input$sleep_or_wake, "\', sleep_start = ", input$sleep_start, ", sleep_end = ", input$sleep_end, ")")
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
  bp_ghana <- bp::bp_ghana
  bp_hypnos <- bp::bp_hypnos
  bp_jhs <- bp::bp_jhs
  
  hypnos_proc <- process_data(bp_hypnos,
                              bp_type = 'abpm',
                              sbp = "syst",
                              dbp = "DIAST",
                              date_time = "date.time",
                              id = "id",
                              wake = "wake",
                              visit = "visit",
                              hr = "hr",
                              map = "map",
                              rpp = "rpp",
                              pp = "pp",
                              ToD_int = c(5, 13, 18, 23))
  
  jhs_proc <- process_data(bp_jhs,
                           sbp = "Sys.mmHg.",
                           dbp = "Dias.mmHg.",
                           date_time = "DateTime",
                           hr = "pulse.bpm.")
  
  
  plot.datasetInput <- reactive({
    if (input$dataSet == "bp_ghana"){
      plot.dataset <- bp_ghana
    }
    else if (input$dataSet == "bp_hypnos"){
      plot.dataset <- hypnos_proc
    }
    else if (input$dataSet == "bp_jhs"){
      plot.dataset <- jhs_proc
    }
    return(plot.dataset)
  })
  
  output$bp.scatter <- renderPlot({bp_scatter(plot.datasetInput(),
                                              plot_type = input$plotType,
                                              subj = NULL,
                                              wrap_var = NULL,
                                              group_var = {
                                                if (input$dataSet == "bp_ghana"){c(NULL,input$group_var_ghana)}
                                                else if (input$dataSet == "bp_jhs"){c(NULL,input$group_var_jhs)}
                                                else if (input$dataSet == "bp_hypnos"){c(NULL,input$group_var_hypnos)}
                                              },
                                              inc_crisis = input$inc.crisis,
                                              inc_low = input$inc.low
  )})
  
})



  
  
  
