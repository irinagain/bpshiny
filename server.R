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


shinyServer(function(input,output,session) {
  ######DATA######
  
  #Creates fileInput() if User Datafile is selected
  output$file_input <- renderUI({
    if(input$fileselect == 'input_data'){
      fileInput("datafile", "Choose a CSV File", multiple = FALSE, accept = ".csv")
    }
  })  

  #Reactive function to read datafile
  dataset <- reactive({
    inFile <- input$datafile
    if(is.null(inFile)){
      return(NULL)
    }
    read.csv(inFile$datapath, header = T)
  })
  #Updates selectInput() value 
  observe({
    updateSelectInput(session,'sys', choices = names(dataset()))
    updateSelectInput(session, 'dias', choices = names(dataset()))
  })
  
  
  #Creates Systolic/Diastolic text boxes if user datafile is selected
  output$sys_input <- renderUI({
    if(input$fileselect == 'input_data'){
      selectInput('sys', 'Systolic', '')
    }
  })
  
  output$dias_input <- renderUI({
    if(input$fileselect == 'input_data'){
      selectInput('dias', 'Diastolic', '')
    }
  })
  
  
  #Creates checkbox based on if User Datafile is selected
  output$date_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('date1', 'Date/Time')
    }
  })
  
  output$id_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('id1', 'ID')
    }
  })
  
  output$wake_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('wake1', 'Wake')
    }
  })
  
  output$visit_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('visit1', 'Visit')
    }
  })
  
  output$hr_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('heart1', 'Heart Rate')
    }
  })
  
  output$pp_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('pp1','Pulse Pressure')
    }
  })
  
  output$map_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('map1','Mean Arterial Pressure')
    }
  })
  
  output$rpp_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('rpp1','Rate Pulse Pressure')
    }
  })
  
  output$dow_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('dow1','Day of the Week')
    }
  })
  
  
  #Creates textInput() based on what column names were selected 
  output$dateinput <- renderUI({
    req(input$date1)
    selectInput('date', 'Date', names(dataset()))
  })
  output$idinput <- renderUI({
    req(input$id1)
    if(input$id1 == FALSE){
      return(NULL)
    }else{
      selectInput('id', 'ID', names(dataset()))
    }
  })
  output$wakeinput <- renderUI({
    req(input$wake1)
    if(input$wake1 == FALSE){
      return(NULL)
    }else{
      selectInput('wake', 'Wake', names(dataset()))
    }
  })
  output$visitinput <- renderUI({
    req(input$visit1)
    if(input$visit1 == FALSE){
      return(NULL)
    }else{
      selectInput('visit', 'Visit', names(dataset()))
    }
  })
  output$heartinput <- renderUI({
    req(input$heart1)
    if(input$heart1 == FALSE){
      return(NULL)
    }else{
      selectInput('hr', 'Heart Rate', names(dataset()))
    }
  })
  output$ppinput <- renderUI ({
    req(input$pp1)
    if(input$pp1 == FALSE){
      return(NULL)
    }else{
      selectInput('pp', 'Pulse Pressure', names(dataset()))
    }
  })
  output$mapinput <- renderUI({
    req(input$map1)
    if(input$map1 == FALSE){
      return(NULL)
    }else{
      selectInput('map', 'Mean Arterial Pressure', names(dataset()))
    }
  })
  output$rppinput <- renderUI({
    req(input$rpp1)
    if(input$rpp1 == FALSE){
      return(NULL)
    }else{
      selectInput('rpp', 'Rate Pulse Pressure', names(dataset()))
    }
  })
  output$dowinput <- renderUI({
    req(input$dow1)
    if(input$dow1 == FALSE){
      return(NULL)
    }else{
      selectInput('dow', 'Day of the Week', names(dataset()))
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
  
  #Reactive Expression if user selects bp_children
  children_data <- reactive({
    bp_children <- bp::bp_children
    children_proc <- process_data(bp_children, 
                                  sbp = 'sbp', dbp = 'dbp',
                                  id = 'id', visit = 'visit')
    children_proc
  })
  
  #Reactive Expression if user selects bp_preg
  preg_data <- reactive({
    bp_preg <- bp::bp_preg
    bppreg_proc <- process_data(bp_preg, sbp = 'SBP', dbp = 'DBP',
                                id = 'ID')
    bppreg_proc
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
  
    #switch() function that will output table according to selected dataset 
  user_data <- reactive ({
    datachoice = input$fileselect
    switch(datachoice,'ghana_data' = bp::bp_ghana, 'hypnos_data' = hypnos_data(), 'jhsproc_data' = jhs_data(), 'bpchildren_data' = children_data(),
           'bppreg_data' = preg_data(), 'input_data' = input_data())
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
  
  
  
  output$input_data_subj <- renderUI({
    selectInput(inputId = "subj_input_data", label = "Subject", choices = c("", as.character(factor(user_data()$ID))), selected = NULL, multiple = T)
  })
  
  output$input_data_group_var <- renderUI({
    selectInput(inputId = "group_var_input_data", label = "Grouping Variable (1):", choices = c("", names(user_data()[,1:ncol(user_data())])),selected = NULL, multiple = T)
  })
  
  output$input_data_wrap_vars <- renderUI({
    selectInput(inputId = "wrap_vars_input_data", label = "Wrapping Variable (1):", choices = c("", names(user_data()[,1:ncol(user_data())])), selected = NULL, multiple = T)
  })
  
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
