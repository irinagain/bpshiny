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
  
  #Creates fileInput() if User Datafile is selected
  output$file_input <- renderUI({
    if(input$fileselect == 'input_data'){
      fileInput("datafile", "Choose a CSV File", multiple = FALSE, accept = ".csv")
    }
  })
  
  #Creates textInput() based on what column names were selected 
  output$dateinput <- renderUI({
    if(input$date1 == FALSE){
      return(NULL)
    }else{
      textInput("date", "Date",value=NULL)
    }
  })
  output$idinput <- renderUI({
    if(input$id1 == FALSE){
      return(NULL)
    }else{
      textInput("id", "ID",value=NULL)
    }
  })
  output$wakeinput <- renderUI({
    if(input$wake1 == FALSE){
      return(NULL)
    }else{
      textInput("wake", "Wake",value=NULL)
    }
  })
  output$visitinput <- renderUI({
    if(input$visit1 == FALSE){
      return(NULL)
    }else{
      textInput("visit", "Visit",value = NULL)
    }
  })
  output$heartinput <- renderUI({
    if(input$heart1 == FALSE){
      return(NULL)
    }else{
      textInput("hr", "Heart Rate",value = NULL)
    }
  })
  output$ppinput <- renderUI ({
    if(input$pp1 == FALSE){
      return(NULL)
    }else{
      textInput('pp', 'Pulse Pressure',value = NULL)
    }
  })
  output$mapinput <- renderUI({
    if(input$map1 == FALSE){
      return(NULL)
    }else{
      textInput("map", "Mean Arterial Pressure",value = NULL)
    }
  })
  output$rppinput <- renderUI({
    if(input$rpp1 == FALSE){
      return(NULL)
    }else{
      textInput("rpp", "Rate Pulse Pressure",value = NULL)
    }
  })
  output$dowinput <- renderUI({
    if(input$dow1 == FALSE){
      return(NULL)
    }else{
      textInput("dow", "Day of the Week",value = NULL)
    }
  })
  
  #Reactive Expression if user inputs their own data
  input_data <- reactive({
    file <- input$datafile
    
    #Ensuring uploaded file is .csv format
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    #Assigning data to variable 'bpdata'
    bpdata = read.csv(file$datapath, header=T)
    
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
    
    #Displays original dataframe until submit button is pushed and creates new processed data frame with variable name 'bpdata.final'
    if(input$submit == FALSE){
      bpdata
    }else{
      bpdata_final = process_data(data = bpdata, sbp = input$sys, dbp = input$dias,date_time = date, id = id, wake = wake, visit = visit,
                                  hr=hr, pp=pp, map=map,rpp=rpp, DoW=dow)
      bpdata_final
    }
  })
  
  #Reactive Expression if users selects hypnos_data
  hypnos_data <- reactive({
    bp_hypnos <- bp::bp_hypnos
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
    hypnos_proc
  })
  
  #Reactive Expression if users selects jhs_data
  jhs_data <- reactive ({
    bp_jhs <- bp::bp_jhs
    jhs_proc <- process_data(bp_jhs,
                             sbp = "Sys.mmHg.",
                             dbp = "Dias.mmHg.",
                             date_time = "DateTime",
                             hr = "pulse.bpm.")
    jhs_proc
  })
  
  #Reactive Expression if users selects ghana_data
  ghana_data <- reactive ({
    bp_ghana <- bp::bp_ghana
    ghana_proc <- process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP')
    ghana_proc
  })
  
  #switch() function that will output table according to selected dataset 
  user_data <- reactive ({
    datachoice = input$fileselect
    switch(datachoice,'ghana_data' = ghana_data(),'hypnos_data' = hypnos_data(), 'jhsproc_data' = jhs_data(), 'input_data' = input_data())
  })
  
  
  output$contents <- renderTable({
    user_data()
  })
  
  
  ######METRICS######
  #add metric based on the parameter it takes in
  parameter_type <- reactive({
    #metric is considered as parameter type "none" if it only requires data as a parameter
    if(input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", 'bp_stats')){
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
    data = user_data()
    
    #loading bp library and using metric function
    if(is.null(input$parameter) | parameter_type == "none"){
      string = paste("bp::", input$metric, "(data)", sep = "")
    }
    
    eval(parse(text = string))
    
  })
  
  
  output$metric <- DT::renderDataTable(metric_table(), extensions = "Buttons",
                                       options = list(dom = "Btip",
                                                      buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                      scrollX = TRUE))
  ######PLOT######
  
  output$plotName <- renderText(input$fileselect)
  
  output$bp.scatter <- renderPlot({bp_scatter(user_data(),
                                              plot_type = input$plotType,
                                              subj = {
                                                if (input$fileselect == "ghana_data"){input$subj_ghana}
                                                else if (input$fileselect == "hypnos_data"){input$subj_hypnos}
                                                else if(input$fileselect == "input_data" && length(factor(input$subj_input_data)) >1){input$subj_input_data}
                                                else (subj <- NULL)
                                              }
                                              ,
                                              wrap_var = {
                                                if(input$fileselect == "input_data"){
                                                  if(!is.null(input$wrap_vars_input_data)){
                                                    if(nchar(input$wrap_vars_input_data)>1){
                                                      input$wrap_vars_input_data
                                                    }
                                                  }
                                                }
                                              
                                                else if (input$fileselect == "ghana_data"){
                                                  if(!is.null(input$wrap_vars_ghana)){
                                                    if(nchar(input$wrap_vars_ghana)>1){
                                                      input$wrap_vars_ghana
                                                    }
                                                  }
                                                }
                                                else if (input$fileselect == "jhsproc_data"){
                                                  if(!is.null(input$wrap_vars_jhs)){
                                                    if(nchar(input$wrap_vars_jhs)>1){
                                                      input$wrap_vars_jhs
                                                    }
                                                  }}
                                                else if (input$fileselect == "hypnos_data"){
                                                  if(!is.null(input$wrap_vars_hypnos)){
                                                    if(nchar(input$wrap_vars_hypnos)>1){
                                                      input$wrap_vars_hypnos
                                                    }
                                                  }
                                                }
                                              },
                                              group_var = {
                                                if(input$fileselect == "input_data"){
                                                  if(!is.null(input$group_var_input_data)){
                                                    if(nchar(input$group_var_input_data)>1){
                                                      input$group_var_input_data
                                                    }
                                                  }
                                                }
                                                
                                               else if (input$fileselect == "ghana_data"){
                                                  if(!is.null(input$group_var_ghana)){
                                                    if(nchar(input$group_var_ghana)>1){
                                                      input$group_var_ghana
                                                    }
                                                  }
                                                }
                                                else if (input$fileselect == "jhsproc_data"){
                                                  if(!is.null(input$group_var_jhs)){
                                                    if(nchar(input$group_var_jhs)>1){
                                                      input$group_var_jhs
                                                    }
                                                  }
                                                }
                                                else if (input$fileselect == "hypnos_data"){
                                                  if(!is.null(input$group_var_hypnos)){
                                                    if(nchar(input$group_var_hypnos)>1){
                                                      input$group_var_hypnos
                                                    }
                                                  }
                                                }
                                              },
                                              inc_crisis = input$inc.crisis,
                                              inc_low = input$inc.low
  )})
  
})
