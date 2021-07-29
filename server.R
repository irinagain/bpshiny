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
  
  
  #Creates dropdown based on what column names were selected 
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
  
  #Select for bp_type argument
  output$bp_type_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('bptype_check', 'Blood Presure Type Argument')
    }
  })
  
  output$bp_type_arg <- renderUI({
    req(input$bptype_check)
    if(input$bptype_check == FALSE){
      return(NULL)
    }else{
      selectInput('bptype_arg', 'Blood Pressure Type', c('HBPM' ='hbpm', 'ABPM' = 'abpm', 'AP' = 'ap'))
    }
  })
  
  bptype_value <- reactive({
    if(is.null(input$bptype_arg) | isFALSE(input$bptype_check)){
      return('hbpm')
    }
    req(input$bptype_arg)
    if(input$bptype_arg == 'hbpm'){
      return('hbpm')
    }else if (input$bptype_arg == 'abpm'){
      return('abpm')
    }else{
      return('ap')
    }
  })
  
  #Toggle for data_screen argument 
  output$data_screen_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('datascreen_check', 'Data Screen Argument')
    }
  })
  
  output$data_screen_arg <- renderUI({
    req(input$datascreen_check)
    if(input$datascreen_check == FALSE){
      return(NULL)
    }else{
      selectInput('datascreen_arg', 'Screen For Extreme Values', c('True' = 't', 'False' = 'f'))
    }
  })
  
  datascreen_value <- reactive ({
    if(is.null(input$datascreen_arg) | isFALSE(input$datascreen_check)){
      return(TRUE)
    }
    req(input$datascreen_arg)
    if(input$datascreen_arg == 'f'){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  #Toggle for inc_low argument
  output$inc_low_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('inclow_check', 'Include Low Argument')
    }
  })
  
  output$inc_low_arg <- renderUI({
    req(input$inclow_check)
    if(input$inclow_check == FALSE){
      return(NULL)
    }else{
      selectInput('inclow_arg', 'Include Low Category', c('True' = 't', 'False' = 'f'))
    }
  })
  
  inclow_value <- reactive({
    if(is.null(input$inclow_arg) | isFALSE(input$inclow_check)){
      return(TRUE)
    }
    req(input$inclow_arg)
    if(input$inclow_arg == 'f'){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  #Toggle for the inc_crisis argument
  output$inc_crisis_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('inccrisis_check', 'Include Crisis Argument')
    }
  })
  
  output$inc_crisis_arg <- renderUI({
    req(input$inccrisis_check)
    if(input$inccrisis_check == FALSE){
      return(NULL)
    }else{
      selectInput('inccrisis_arg', 'Include Crisis Category', c('True' = 't', 'False' = 'f'))
    }
  })
  
  inccrisis_value <- reactive({
    if(is.null(input$inccrisis_arg) | isFALSE(input$inccrisis_check)){
      return(TRUE)
    }
    req(input$inccrisis_arg)
    if(input$inccrisis_arg == 'f'){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  #Input for tod_int argument
  output$tod_int_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('todint_check', 'Include Time of Day Argument')
    }
  })
  
  output$tod_int_arg <- renderUI({
    req(input$todint_check)
    if(input$todint_check == FALSE){
      return(NULL)
    }else{
      textInput('todint_arg', 'Time of Day Argument') 
    }
  })
  
  todint_value <- reactive({
    if(is.null(input$todint_arg) | isFALSE(input$todint_check)){
      return(c(6,12,18,0))
    }
    req(input$todint_arg)
    if(!is.null(input$todint_arg)){
      inp <- as.numeric(unlist(strsplit(input$todint_arg,",")))
      req(length(inp) >= 4)
      return(inp)
    }
  })
  
  #Input for eod argument
  output$eod_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('eodcheck', 'Include EOD Argument')
    }
  })
  
  output$eod_arg <- renderUI({
    req(input$eodcheck)
    if(input$eodcheck == FALSE){
      return(NULL)
    }else{
      textInput('eodarg', 'EOD Argument')
    }
  })
  
  eod_value <- reactive({
    if(is.null(input$eodarg) | isFALSE(input$eodcheck)){
      return(NULL)
    }
    req(input$eodarg)
    if(!is.null(input$eodarg)){
      inp1 <- as.numeric(input$eodarg)
      req(nchar(input$eodarg) == 4)
      return(inp1)
    }
  })
  
  #Input for agg argument 
  output$agg_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('aggcheck', 'Include AGG Argument')
    }
  })
  
  output$agg_arg <- renderUI({
    req(input$aggcheck)
    if(input$aggcheck == FALSE){
      return(NULL)
    }else{
      selectInput('aggarg', 'AGG Argument', c('True' = 't', 'False' = 'f'))
    }
  })
  
  agg_value <- reactive ({
    if(is.null(input$aggarg) | isFALSE(input$aggcheck)){
      return(FALSE)
    }
    req(input$aggarg)
    if(input$aggarg == 'f'){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  #Input for agg_thresh argument
  output$agg_thresh_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('aggthresh_check', 'Include AGG Thresh Argument')
    }
  })
  
  output$agg_thresh_arg <- renderUI({
    req(input$aggthresh_check)
    if(input$aggthresh_check == FALSE){
      return(NULL)
    }else{
      textInput('aggthresh_arg', 'AGG Thresh Argument', '3')
    }
  })
  
  aggthresh_value <- reactive ({
    if(is.null(input$aggthresh_arg) | isFALSE(input$aggthresh_check | isTRUE(agg_value()))){
      return(NULL)
    }
    req(input$aggthresh_arg)
    if(!is.null(input$aggthresh_arg)){
      inp2 <- as.numeric(input$aggthresh_arg)
      return(inp2)
    }
  })
  
  #Input for collapse_df argument
  output$collapse_df_check <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('collapsedf_check', 'Include Collapse df Argument')
    }
  })
  
  output$collapse_df_arg <- renderUI({
    req(input$collapsedf_check)
    if(input$collapsedf_check == FALSE){
      return(NULL)
    }else{
      selectInput('collapsedf_arg', 'Collapse df Argument', c('True' = 't', 'False' = 'f'))
    }
  })
  
  collapse_value <- reactive ({
    if(is.null(input$collapsedf_arg) | isFALSE(input$collapsedf_check)){
      return(FALSE)
    }
    req(input$collapsedf_arg)
    if(input$collapsedf_arg == 'f'){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  
  #Toggle between original and processed data
  output$dataviewer <- renderUI(
    radioButtons('dataview', label = 'View Data', choices = c('Original Data' = 'unproc_data', 'Processed Data' = 'proc_data'), selected = 'unproc_data')
  )
  
  #Reactive Expression if users selects hypnos_data
  hypnos_data <- reactive({
    bp_hypnos <- bp::bp_hypnos
    hypnos_proc <- process_data(bp_hypnos,
                                bp_type = 'abpm',
                                sbp = "syst",
                                dbp = "DIAST",
                                date_time = "DATE.TIME",
                                id = "id",
                                wake = "wake",
                                visit = "visit",
                                hr = "hr",
                                map = "map",
                                rpp = "rpp",
                                pp = "pp")
    if(input$dataview == 'proc_data'){
      hypnos_proc
    }else{
      bp_hypnos
    }
  })
  
  #Reactive Expression if users selects jhs_data
  jhs_data <- reactive ({
    bp_jhs <- bp::bp_jhs
    jhs_proc <- process_data(bp_jhs,
                             sbp = "Sys.mmHg.",
                             dbp = "Dias.mmHg.",
                             date_time = "DateTime",
                             hr = "pulse.bpm.")
    if(input$dataview == 'proc_data'){
      jhs_proc
    }else{
      bp_jhs
    }
  })
  
  #Reactive Expression if user selects bp_children
  children_data <- reactive({
    bp_children <- bp::bp_children
    children_proc <- process_data(bp_children, 
                                  sbp = 'sbp', dbp = 'dbp',
                                  id = 'id', visit = 'visit')
    if(input$dataview == 'proc_data'){
      children_proc
    }else{
      bp_children
    }
  })
  
  #Reactive Expression if user selects bp_preg
  preg_data <- reactive({
    bp_preg <- bp::bp_preg
    bppreg_proc <- process_data(bp_preg, sbp = 'SBP', dbp = 'DBP',
                                id = 'ID')
    if(input$dataview == 'proc_data'){
      bppreg_proc
    }else{
      bp_preg
    }
  })
  
  ghana_data <- reactive({
    bp_ghana <- bp::bp_ghana
    bpghana_proc <- process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP', id = 'ID')
    if(input$dataview == 'proc_data'){
      bpghana_proc
    }else{
      bp_ghana
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
    if(input$date1 == FALSE){date = NULL}
    id = input$id
    if(input$id1 == FALSE){id = NULL}
    wake = input$wake
    if(input$wake1 == FALSE){wake = NULL}
    visit = input$visit
    if(input$visit1 == FALSE){visit = NULL}
    hr = input$hr
    if(input$heart1 == FALSE){hr = NULL}
    pp = input$pp
    if(input$pp1 == FALSE){pp = NULL}
    map = input$map
    if(input$map1 == FALSE){map = NULL}
    rpp = input$rpp
    if(input$rpp1 == FALSE){rpp = NULL}
    dow = input$dow
    if(input$dow1 == FALSE){dow = NULL}
    
    #Displays original dataframe until submit button is pushed and creates new processed data frame with variable name 'bpdata.final'
    if(input$dataview == 'proc_data'){
      bpdata_final = process_data(data = bpdata, sbp = input$sys, dbp = input$dias,date_time = date, id = id, wake = wake, visit = visit,
                                  hr=hr, pp=pp, map=map,rpp=rpp, DoW=dow, data_screen = datascreen_value(),
                                  bp_type = bptype_value(), inc_low = inclow_value(), inc_crisis = inccrisis_value(),
                                  ToD_int = todint_value(), eod = eod_value(), agg = agg_value(),
                                  agg_thresh = aggthresh_value(), collapse_df = collapse_value())
      bpdata_final
    }else{
      bpdata
    }
    
  })
  
  #switch() function that will output table according to selected dataset 
  user_data <- reactive ({
    datachoice = input$fileselect
    switch(datachoice,'ghana_data' = ghana_data(), 'hypnos_data' = hypnos_data(), 'jhsproc_data' = jhs_data(), 'bpchildren_data' = children_data(),
           'bppreg_data' = preg_data(), 'input_data' = input_data())
  })
  
  original_data <- reactive({
    datachoice = input$fileselect
    proc_hypnos <- process_data(bp_hypnos,
                                bp_type = 'abpm',
                                sbp = "syst",
                                dbp = "DIAST",
                                date_time = "DATE.TIME",
                                id = "id",
                                wake = "wake",
                                visit = "visit",
                                hr = "hr",
                                map = "map",
                                rpp = "rpp",
                                pp = "pp")
    proc_jhs <- process_data(bp_jhs,
                             sbp = "Sys.mmHg.",
                             dbp = "Dias.mmHg.",
                             date_time = "DateTime",
                             hr = "pulse.bpm.")
    proc_children <- process_data(bp_children, 
                                  sbp = 'sbp', dbp = 'dbp',
                                  id = 'id', visit = 'visit')
    
    proc_preg <- process_data(bp_preg, sbp = 'SBP', dbp = 'DBP', id = 'ID')
    proc_ghana <- process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP', id = 'ID')
    
    sys = input$sys
    dias = input$dias
    date = input$date
    if(input$date1 == FALSE){date = NULL}
    id = input$id
    if(input$id1 == FALSE){id = NULL}
    wake = input$wake
    if(input$wake1 == FALSE){wake = NULL}
    visit = input$visit
    if(input$visit1 == FALSE){visit = NULL}
    hr = input$hr
    if(input$heart1 == FALSE){hr = NULL}
    pp = input$pp
    if(input$pp1 == FALSE){pp = NULL}
    map = input$map
    if(input$map1 == FALSE){map = NULL}
    rpp = input$rpp
    if(input$rpp1 == FALSE){rpp = NULL}
    dow = input$dow
    if(input$dow1 == FALSE){dow = NULL}
    
    proc_inputdata = process_data(data = input_data(), sbp = input$sys, dbp = input$dias,date_time = date, id = id, wake = wake, visit = visit,
                                  hr=hr, pp=pp, map=map,rpp=rpp, DoW=dow, data_screen = datascreen_value(),
                                  bp_type = bptype_value(), inc_low = inclow_value(), inc_crisis = inccrisis_value(),
                                  ToD_int = todint_value(), eod = eod_value(), agg = agg_value(),
                                  agg_thresh = aggthresh_value(), collapse_df = collapse_value())
    
    switch(datachoice,'ghana_data' = proc_ghana, 'hypnos_data' = proc_hypnos, 'jhsproc_data' = proc_jhs, 'bpchildren_data' = proc_children,
           'bppreg_data' = proc_preg, 'input_data' = proc_inputdata)
  })
  
  output$contents <- renderTable({
    user_data()
  })
  
  
  
  ############################# METRIC SECTION ######################################################
  
  
  #add metric based on the parameter it takes in
  parameter_type <- reactive({
    #metric is considered as parameter type "none" if it only requires data as a parameter
    if(input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", "bp_stats", "bp_tables", "cv", "sv")){
      return("none")
    }
    if(input$metric %in% c("dip_calc")){
      return("dip_calc")
    }
  })
  
  output_type <- reactive({
    if(input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", "bp_stats", "cv", "sv")){
      return("none")
    }
    if(input$metric == "bp_tables"){
      return("tables") 
    }
    if(input$metric == "dip_calc"){
      return("dip_calc")
    }
    
  })
  #specify first parameter and the default values
  output$select_dip_parameter <- renderUI({
    if(input$metric == "dip_calc"){
      numericInput("parameter", "Specify dipping threshold",value = 0.1, step = 0.05)
    }
  })
  
  output$select_ext_parameter <- renderUI({
    if(input$metric == "dip_calc"){
      numericInput("parameter2", "Specify extreme dipping threshold",value = 0.2, step = 0.05)
    }
  })
  #add warning message if dip_thres >= ext_thres
  
  #add description of first parameter
  
  output$help_text <- renderUI ({
    parameter_type = parameter_type()
    
    if(parameter_type == "none"){
      helpText("No parameters need to be specified.")
    }
    else if(parameter_type == "dip_calc"){
      helpText("Enter the dip and extreme thresholds separated by comma. ")
      if (input$parameter >= input$parameter2){
        helpText("Enter a dipping threshold less than extreme dipping threshold. ")
      }
    }
  })
  
  # output$select_parameter <- renderUI({
  #   parameter_type = parameter_type()
  #   if(parameter_type == "dip_calc"){
  #       textInput("Dip Threshold", value = "0.10")
  #       textInput("Extreme Threshold", value= "0.20")
  #   }
  # })
  
  #reactive and output functions based on the user's choice
  # outputting one table
  # observeEvent(req(input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", "bp_stages", 'bp_stats', 'cv', 'sv', 'dip_calc')), {
  #   metric_table <- reactive({
  #     parameter_type = parameter_type()
  #     output_type = output_type()
  #     data = user_data()
  # 
  #     #loading bp library and using metric function
  #     if(is.null(input$parameter) | (parameter_type == "none" & output_type == "none")){
  #       #
  #       string = paste("bp::", input$metric, "(data)", sep = "")
  #     }
  # 
  #     eval(parse(text = string))
  #   })
  # 
  #   output$metric_table <- DT::renderDataTable(metric_table(), extensions = "Buttons",
  #                                        options = list(dom = "Btip",
  #                                                       buttons = c("copy", "csv", "excel", "pdf", "print"),
  #                                                       scrollX = TRUE))
  # 
  # })
  
  # #outputting several tables (when 'bp_tables' is chosen as the metric)
  # observeEvent((req(input$metric == "bp_tables")), {
  #     metric_bp_tables <- reactive({
  #     #parameter_type = parameter_type()
  #     #output_type = output_type()
  #       data = user_data()
  #     #if(output_type == "tables"){
  #       tables_output = bp::bp_tables(data)
  #     #}
  #     # for (i in sequence(length(tables_output))){
  #     #   final_output <- as.data.frame(tables_output[i])
  #     # }
  #     # final_output
  #       tables_output$SBP_Counts_by_Stage
  #     })
  # 
  # 
  #   # bp_tables_output = metric_bp_tables()
  #   # for (i in sequence(16)){
  #   output$metric_bp_tables <- DT::renderDataTable(metric_bp_tables(), extensions = "Buttons",
  #                                           options = list(dom = "Btip",
  #                                                          buttons = c("copy", "csv", "excel", "pdf", "print"),
  #                                                          scrollX = TRUE))
  #   # }
  # })
  # output$metric <- DT::renderDataTable(final_table, extensions = "Buttons",
  #                                      options = list(dom = "Btip",
  #                                                     buttons = c("copy", "csv", "excel", "pdf", "print"),
  #                                                     scrollX = TRUE))
  
  
  metric_table <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    #If/else statement that decides whether to use sample data that is processed in data tab
    #Or if original data is still selected, it will use the pre-proccessed sample data
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    ## validate(need) argument, eliminates error popping up when changing parameter type
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "none", "output type incorrect")
    )
    
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "none")){
      string = paste("bp::", input$metric, "(data)", sep = "")
    }
    eval(parse(text = string))
  })
  output$metric_table <- DT::renderDataTable(metric_table(), extensions = "Buttons",
                                             options = list(dom = "Btip",
                                                            buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                            scrollX = TRUE))
  metric_bp_table_1 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_Counts_by_Stage
  })
  output$text_1 <- renderText({"Table 1. SBP Counts by Stage"})
  
  metric_bp_table_2 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_Counts_by_Stage
  })
  output$text_2 <- renderText({"\n Table 2. DBP Counts by Stage"})
  
  metric_bp_table_3 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Counts
  })
  output$text_3 <- renderText({"\n Table 3. CLASS Counts"})
  
  metric_bp_table_4 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$All_BP_Stage_Combinations
  })
  output$text_4 <- renderText({"\n Table 4. All BP Stage Combinations"})
  
  metric_bp_table_5 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$BP_contingency_count
  })
  output$text_5 <- renderText({"\n Table 5. BP_contingency_count"})
  
  metric_bp_table_6 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$BP_contingency_percent
  })
  output$text_6 <- renderText({"\n Table 6. BP_contingency_percent"})
  
  metric_bp_table_7 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_Day_of_Week
  })
  output$text_7 <- renderText({"\n Table 7. SBP_by_Day_of_Week"})
  
  metric_bp_table_8 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_Day_of_Week
  })
  output$text_8 <- renderText({"\n Table 8. DBP_by_Day_of_Week"})
  
  metric_bp_table_9 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Day_of_Week
  })
  output$text_9 <- renderText({"\n Table 9. CLASS_Day_of_Week"})
  
  metric_bp_table_10 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_Time_of_Day
  })
  output$text_10 <- renderText({"\n Table 10. SBP_by_Time_of_Day"})
  
  metric_bp_table_11 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_Time_of_Day
  })
  output$text_11 <- renderText({"\n Table 11. DBP_by_Time_of_Day"})
  
  metric_bp_table_12 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Time_of_Day
  })
  output$text_12 <- renderText({"\n Table 12. CLASS_Time_of_Day"})
  
  metric_bp_table_13 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    ## need wake to calculate sbp_by_wake_status(metric_bp_table_13), dbp_by_wake_status(14), sbp_by_wake_perc(15), and dbp_by_wake_perc(16), details seen in bp::bp_tables function
    validate(
      need("WAKE" %in% names(data), "N/A - WAKE column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_WAKE_status
  })
  output$text_13 <- renderText({"\n Table 13. SBP_by_WAKE_status \n"})
  
  metric_bp_table_14 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    validate(
      need("WAKE" %in% names(data), "N/A - WAKE column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_WAKE_status
  })
  output$text_14 <- renderText(paste("Table 14. DBP_by_WAKE_status", "", sep = "\n"))
  
  metric_bp_table_15 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    validate(
      need("WAKE" %in% names(data), "N/A - WAKE column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_WAKE_perc
  })
  output$text_15 <- renderText({"\n Table 15. SBP_by_WAKE_perc \n"})
  
  metric_bp_table_16 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    validate(
      need("WAKE" %in% names(data), "N/A - WAKE column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_WAKE_perc
  })
  output$text_16 <- renderText({"\n Table 16. DBP_by_WAKE_perc"})
  
  
  output$metric_bp_table_1 <- DT::renderDataTable(metric_bp_table_1(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_2 <- DT::renderDataTable(metric_bp_table_2(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_3 <- DT::renderDataTable(metric_bp_table_3(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_4 <- DT::renderDataTable(metric_bp_table_4(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_5 <- DT::renderDataTable(metric_bp_table_5(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_6 <- DT::renderDataTable(metric_bp_table_6(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_7 <- DT::renderDataTable(metric_bp_table_7(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_8 <- DT::renderDataTable(metric_bp_table_8(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_9 <- DT::renderDataTable(metric_bp_table_9(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_bp_table_10 <- DT::renderDataTable(metric_bp_table_10(), extensions = "Buttons",
                                                   options = list(dom = "Btip",
                                                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                  scrollX = TRUE))
  output$metric_bp_table_11 <- DT::renderDataTable(metric_bp_table_11(), extensions = "Buttons",
                                                   options = list(dom = "Btip",
                                                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                  scrollX = TRUE))
  output$metric_bp_table_12 <- DT::renderDataTable(metric_bp_table_12(), extensions = "Buttons",
                                                   options = list(dom = "Btip",
                                                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                  scrollX = TRUE))
  output$metric_bp_table_13 <- DT::renderDataTable(metric_bp_table_13(), extensions = "Buttons",
                                                   options = list(dom = "Btip",
                                                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                  scrollX = TRUE))
  output$metric_bp_table_14 <- DT::renderDataTable(metric_bp_table_14(), extensions = "Buttons",
                                                   options = list(dom = "Btip",
                                                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                  scrollX = TRUE))
  output$metric_bp_table_15 <- DT::renderDataTable(metric_bp_table_15(), extensions = "Buttons",
                                                   options = list(dom = "Btip",
                                                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                  scrollX = TRUE))
  output$metric_bp_table_16 <- DT::renderDataTable(metric_bp_table_16(), extensions = "Buttons",
                                                   options = list(dom = "Btip",
                                                                  buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                  scrollX = TRUE))
  
  ## dip_calc
  metric_dip_calc_1 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "dip_calc", "parameter type incorrect"),
      need(output_type == "dip_calc", "output type incorrect")
    )
    # if (is.null(input$parameter)) {
    #   validate(
    #     need(!is.null(input$parameter), "Please wait - Rendering")
    #   )
    # } else if (grepl(',', input$parameter) & !grepl("\\(", input$parameter)) {
    #   if (length(strsplit(input$parameter, split = ",")[[1]]) != 2) {
    #     validate (
    #       need(parameter_type == "dip_calc", "Please wait - Rendering")
    #     )
    #   } else {
    #     validate(
    #       need(parameter_type == "dip_calc", "Please wait - Rendering")
    #     )
    #   }
    # }
    if(is.null(input$parameter) | (parameter_type == "dip_calc" & output_type == "dip_calc")){
      dip_calc_output = bp::dip_calc(data, dip_thresh = input$parameter, extreme_thresh = input$parameter2)
    }
    return(data.frame(dip_calc_output[1]))
  })
  
  metric_dip_calc_2 <- eventReactive(req(input$metric_update),{
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "dip_calc", "parameter type incorrect"),
      need(output_type == "dip_calc", "output type incorrect")
    )
    if(is.null(input$parameter) | (parameter_type == "dip_calc" & output_type == "dip_calc")){
      dip_calc_output = bp::dip_calc(data, dip_thresh = input$parameter, extreme_thresh = input$parameter2)
    }
    return(data.frame(dip_calc_output[2]))
  })
  
  output$metric_dip_calc_1 <- DT::renderDataTable(metric_dip_calc_1(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_dip_calc_2 <- DT::renderDataTable(metric_dip_calc_2(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  
  output$one_table <- eventReactive(req(input$metric_update),{
    input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", "bp_stats", "cv", "sv")
  })
  outputOptions(output, 'one_table', suspendWhenHidden = FALSE)
  
  output$bp_tables_tables <- eventReactive(req(input$metric_update),{
    input$metric == "bp_tables"
  })
  outputOptions(output, 'bp_tables_tables', suspendWhenHidden = FALSE)
  
  output$dip_calc_tables <- eventReactive(req(input$metric_update),{
    input$metric == "dip_calc"
  })
  outputOptions(output, 'dip_calc_tables', suspendWhenHidden = FALSE)
  
  ######PLOT######
  
  #Get name of dataset
  plotName <- eventReactive(input$plot_update, {input$fileselect})
  
  output$plotName <- plotName
  
  #Get the type of plot the user wants to render
  plottype <- reactive({  # wrap plottype input in a reactive for rendering UI and Plot
    if(input$plottype == "bp_scatter"){
      return("bp_scatter")
    }
    else if(input$plottype == "bp_hist"){
      return("bp_hist")
    }
    else if(input$plottype == "bp_report"){
      return("bp_report")
    }
    else if(input$plottype == "dow_tod_plots"){
      return("dow_tod_plots")
    }
    else if(input$plottype == "bp_ts_plots"){
      return("bp_ts_plots")
    }
  })
  
  #get the name of the type of plot the user wants to render
  plot_type_text <- eventReactive(input$plot_update, {
    if(input$plottype == "bp_scatter"){
      return("bp_scatter")
    }
    else if(input$plottype == "bp_hist"){
      return("bp_hist")
    }
    else if(input$plottype == "bp_report"){
      return("bp_report")
    }
    else if(input$plottype == "dow_tod_plots"){
      return("dow_tod_plots")
    }
    else if(input$plottype == "bp_ts_plots"){
      return("bp_ts_plots")
    }
  })
  output$plot_type_text <- renderText(plot_type_text())
  ### Get subj argument used in all the plots
  
  #Get the subject arguments that is used in all plot types 
  output$subj_for_plots<- renderUI({
    plottype = plottype()
    
    if((plottype == "bp_scatter") | (plottype == "bp_hist") | (plottype == "bp_report") | (plottype == "dow_tod_plots") | (plottype == "bp_ts_plots")){
      selectizeInput(inputId = "subj_for_plots", label = "Subject", choices = c("", as.character(levels(factor(user_data()$ID)))), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  ### Get group_var argument for bp_scatter & bp_report
  output$group_var_for_scatter_and_report <- renderUI({
    plottype = plottype()
    
    if((plottype == "bp_scatter") | (plottype == "bp_report") ){
      selectInput(inputId = "group_var_for_scatter_and_report", label = "Grouping Variable (1):", choices = c("", names(user_data()[,which(user_data() %>% summarise_all(n_distinct) <= 10)])),selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  ### Get wrap_var argument for bp_scattter & bp_ts_plots
  output$wrap_var_for_scatter <- renderUI({
    plottype = plottype()
    
    if((plottype == "bp_scatter")){
      selectInput(inputId = "wrap_var_for_scatter", label = "Wrapping Variable (1):", choices = c("", names(user_data()[,which(user_data() %>% summarise_all(n_distinct) <= 10)])), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  #Get the user to choose between AHA or Stages 2020 plot type used exclusively in the bp_scatter function
  output$plot_type_for_scatter <- renderUI({
    plottype = plottype()
    if (plottype == "bp_scatter"){
      radioButtons(inputId = "plot_type_for_scatter", label = "Plot Type", choices = c("stages2020", "AHA"), selected = "stages2020")
    }
    else{NULL}
  })
  
  
  #If the user selects stages2020, they have the option to render the crisis category
  output$include_crisis_stages2020 <- renderUI({
    plottype = plottype()
    plot_type_for_scatter = input$plot_type_for_scatter
    
    if ((plottype == "bp_scatter" & plot_type_for_scatter == "stages2020") | (plottype == "bp_report")) {
      checkboxInput(inputId = "inc_crisis_T_or_F",label = "Include Hypersensitive Crisis", value = T)
    }
  })
  
  #if the user selects stages2020, they have the option to render the low category
  output$include_low_stages2020 <- renderUI({
    plottype = plottype()
    plot_type_for_scatter = input$plot_type_for_scatter
    
    if ((plottype == "bp_scatter" & plot_type_for_scatter == "stages2020") | (plottype == "bp_report")){
      checkboxInput(inputId = "inc_low_T_or_F",label = "Include Low Hypotension", value = T)
    }
  })
  
  #Get argument "Save Report" used in the bp_report() function
  #output$save_report_for_report <- renderUI({
    #plottype = plottype()
    #if (plottype == "bp_report"){
      #checkboxInput(inputId = "save_report_for_report", label = "Save Report", value = F)
    #}
    #else{NULL}
  #})
  
  #Get the argument "units" used in the bp_report() function
  #output$units_for_report <- renderUI({
   # plottype = plottype()
    #if (plottype == "bp_report"){
     # selectInput(inputId = "units_for_report", label = "Units", choices = c(`Inches (in)` = "in", `Centimeters (cm)` = "cm", `Millimeters (mm)` = "mm"), selected = "in")
    #}
  #})
  
  ### Arguments for bp_ts_plots
  output$wrap_var_for_ts <- renderUI({
    plottype = plottype()
    
    if(plottype == "bp_ts_plots"){
      selectInput(inputId = "wrap_var_for_ts", label = "Wrapping Variable (1):", choices = c("", names(user_data())), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  output$wrap_rowcol_for_ts <- renderUI({
    plottype = plottype()
    
    if(plottype == "bp_ts_plots"){
      
        column(4, numericInput(inputId = "wrap_row_for_ts", 
                               label = "# Rows",value = 1, min = 1, max = 9, step = 1, width = '50%'))
        column(4, numericInput(inputId = "wrap_col_for_ts", 
                               label = "# Columns", value = 1, min = 1, max = 9, ste1, width = '50%'))
      
    }
  })
  output$index_for_ts <- renderUI({
    plottype = plottype()
    
    if(plottype == "bp_ts_plots"){
      selectInput(inputId = "index_for_ts", label = "Index:", choices = c("", names(user_data())), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  output$first_hour_for_ts <- renderUI({
    plottype = plottype()
    
    if(plottype == "bp_ts_plots"){
      numericInput(inputId = "first_hour_for_ts", label = "First Hour (0-23):", value = 0, min = 0, max = 23)
    }
    else{NULL}
  })
  
  output$rotate_xlab_for_ts <- renderUI({
    plottype = plottype()
    
    if (plottype == "bp_ts_plots"){
      checkboxInput(inputId = "rotate_xlab_for_ts", label = "Rotate x-axis labels", value = F)
    }
    else{NULL}
  })
  
  ### Render Plot
  
  #Make a event reactive object that will update whenever the user interacts with the action button "Update" in the plot section.
  #plotFunc is now an object that can be fed into a renderPlot element in the output list
  plotFunc <- eventReactive(input$plot_update,{
    
    plottype = plottype() # bring reactive input variable into this renderPlot call
    library(bp)
    
    #React to plottype call
    
    #If the user wants to render bp_hist
    if(plottype == "bp_hist"){
      
      #if the user wants to do bp_hist on data that isn't unprocessed jhs/hypnos/ghana/children/preg
      if(!(input$fileselect == "jhsproc_data") && !(input$fileselect == "hypnos_data") && !(input$fileselect == "ghana_data") && !(input$fileselect == "bpchildren_data") && !(input$fileselect == "bppreg_data")) {
        bp_hist(data = user_data(), subj = input$subj_for_plots)
      }
      #if the user wants to do bp_hist on unprocessed jhs data
      else if (input$fileselect == "jhsproc_data") {
        bp_hist(data = {process_data(bp_jhs,
                                     sbp = "Sys.mmHg.",
                                     dbp = "Dias.mmHg.",
                                     date_time = "DateTime",
                                     hr = "pulse.bpm.")},
                subj = input$subj_for_plots)
      }
      
      #if the user wants to do bp_hist on unprocessed hypnos data
      else if (input$fileselect == "hypnos_data"){
        bp_hist(data = {process_data(bp_hypnos,
                                     bp_type = 'abpm',
                                     sbp = "syst",
                                     dbp = "DIAST",
                                     date_time = "DATE.TIME",
                                     id = "id",
                                     wake = "wake",
                                     visit = "visit",
                                     hr = "hr",
                                     map = "map",
                                     rpp = "rpp",
                                     pp = "pp")},
                subj = input$subj_for_plots)
      }
      
      #if the user wants to do bp_hist on unprocessed ghana data
      else if (input$fileselect == "ghana_data"){
       bp_hist(data = {process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP', id = 'ID')}, subj = input$subj_for_plots) 
      }
      
      else if (input$fileselect == "bpchildren_data"){
        bp_hist(data = {
          process_data(bp_children, 
                       sbp = 'sbp', dbp = 'dbp',
                       id = 'id', visit = 'visit')
        }, subj = input$subj_for_plots)
      }
      
      else if (input$fileselect == "bppreg_data"){
        bp_hist(data = {
          process_data(bp_preg, sbp = 'SBP', dbp = 'DBP', id = 'ID')
        }, subj = input$subj_for_plots)
      }
    }
    
    #If the user wants to render bp_scatter
    else if(plottype == "bp_scatter"){
      
      #if the user wants to bp_scatter() data that isn't unprocessed jhs or unprocessed hypnos
      if(!(input$fileselect == "jhsproc_data") && !(input$fileselect == "hypnos_data")) {
      bp_scatter(data = user_data(), plot_type = input$plot_type_for_scatter,
                 subj = input$subj_for_plots,
                 group_var = input$group_var_for_scatter_and_report,
                 wrap_var = input$wrap_var_for_scatter,
                 inc_crisis = input$inc_crisis_T_or_F, 
                 inc_low = input$inc_low_T_or_F)
      }
      #if the user wants to use bp_scatter on unprocessed jhs data
      else if (input$fileselect == "jhsproc_data") {
        bp_scatter(data = {process_data(bp_jhs,
                                        sbp = "Sys.mmHg.",
                                        dbp = "Dias.mmHg.",
                                        date_time = "DateTime",
                                        hr = "pulse.bpm.")}, 
                   plot_type = input$plot_type_for_scatter,
                   subj = input$subj_for_plots,
                   group_var = input$group_var_for_scatter_and_report,
                   wrap_var = input$wrap_var_for_scatter,
                   inc_crisis = input$inc_crisis_T_or_F, 
                   inc_low = input$inc_low_T_or_F)
      }
      #if the user wants to use bp_scatter on the unprocessed hypnos data
      else if (input$fileselect == "hypnos_data"){
        bp_scatter(data = {process_data(bp_hypnos,
                                        bp_type = 'abpm',
                                        sbp = "syst",
                                        dbp = "DIAST",
                                        date_time = "DATE.TIME",
                                        id = "id",
                                        wake = "wake",
                                        visit = "visit",
                                        hr = "hr",
                                        map = "map",
                                        rpp = "rpp",
                                        pp = "pp")},
                   plot_type = input$plot_type_for_scatter,
                   subj = input$subj_for_plots,
                   group_var = input$group_var_for_scatter_and_report,
                   wrap_var = input$wrap_var_for_scatter,
                   inc_crisis = input$inc_crisis_T_or_F, 
                   inc_low = input$inc_low_T_or_F)
      }
    }
    
    #If the user wants to render bp_report
    else if(plottype == "bp_report"){
      #If the user wants to user wants to use bp_report for data that isn't unprocessed jhs or unprocessed hypnos
      if(!(input$fileselect == "jhsproc_data") && !(input$fileselect == "hypnos_data")) {
      bp_report(data = user_data(),
                subj = input$subj_for_plots,
                inc_low = input$inc_low_T_or_F,
                inc_crisis = input$inc_crisis_T_or_F,
                group_var = input$group_var_for_scatter_and_report,
                #save_report = input$save_report_for_report,
                path = NULL,
                filename = "bp_report",
                width = 12,
                height = 8.53,
                filetype = "pdf",
                units = input$units_for_report,
                scale = 1)
      }
      #if the user wants to use bp_report on unprocessed jhs data
      else if (input$fileselect == "jhsproc_data") {
        bp_report(data = {process_data(bp_jhs,
                                       sbp = "Sys.mmHg.",
                                       dbp = "Dias.mmHg.",
                                       date_time = "DateTime",
                                       hr = "pulse.bpm.")},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  #save_report = input$save_report_for_report,
                  path = NULL,
                  filename = "bp_report",
                  width = 12,
                  height = 8.53,
                  filetype = "pdf",
                  units = input$units_for_report,
                  scale = 1)
      }
      #if the user wants to use bp_report on unprocessed hypnos data
      else if (input$fileselect == "hypnos_data"){
        bp_report(data = {process_data(bp_hypnos,
                                       bp_type = 'abpm',
                                       sbp = "syst",
                                       dbp = "DIAST",
                                       date_time = "DATE.TIME",
                                       id = "id",
                                       wake = "wake",
                                       visit = "visit",
                                       hr = "hr",
                                       map = "map",
                                       rpp = "rpp",
                                       pp = "pp")},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  #save_report = input$save_report_for_report,
                  path = NULL,
                  filename = "bp_report",
                  width = 12,
                  height = 8.53,
                  filetype = "pdf",
                  units = input$units_for_report,
                  scale = 1)
      }
    }
    
    #if the user wants to render the dow_tod_plots() 
    else if(plottype == "dow_tod_plots"){
      #if the user wants to dow_tod_plots a dataset that isn't unprocessedd hypnos or unprocessed jhs
      if(!(input$fileselect == "jhsproc_data") && !(input$fileselect == "hypnos_data")) {
      dow_tod_plots_out <- dow_tod_plots(data = user_data(),
                                         subj = input$subj_for_plots)
      }
      #if the user wants to dow_tod_plots the jhs data
      else if (input$fileselect == "jhsproc_data") {
        dow_tod_plots_out <- dow_tod_plots(data = {process_data(bp_jhs,
                                                                sbp = "Sys.mmHg.",
                                                                dbp = "Dias.mmHg.",
                                                                date_time = "DateTime",
                                                                hr = "pulse.bpm.")},
                                           subj = input$subj_for_plots)
      }
      #if the user wants to dow_tod_plots the hypnos dataset
      else if (input$fileselect == "hypnos_data"){
        dow_tod_plots_out <- dow_tod_plots(data = {process_data(bp_hypnos,
                                                                bp_type = 'abpm',
                                                                sbp = "syst",
                                                                dbp = "DIAST",
                                                                date_time = "DATE.TIME",
                                                                id = "id",
                                                                wake = "wake",
                                                                visit = "visit",
                                                                hr = "hr",
                                                                map = "map",
                                                                rpp = "rpp",
                                                                pp = "pp")},
                                           subj = input$subj_for_plots)
      }
      
      #use grid & gridExtra package to arrange the list of plots created by the dow_tod_plots() function
      grid::grid.draw(
        gridExtra::grid.arrange(dow_tod_plots_out[[1]], dow_tod_plots_out[[2]], ncol = 2)
      )
    }
    
    #If the user wants to render the bp_ts_plots
    else if(plottype == "bp_ts_plots"){
      bp_ts_plots(data = user_data(),
                  subj = input$subj_for_plots,
                  wrap_var = input$wrap_var_for_ts, 
                  index = input$index_for_ts, 
                  first_hour = input$first_hour_for_ts,
                  rotate_xlab = input$rotate_xlab_for_ts
  )
    }
    
  })
  
  #download handler
  output$downloadPlot <- downloadHandler(
    filename = function() {paste(input$fileselect, '.png', sep = '')},
    content = function(file) {
      ggplot2::ggsave(file, plotFunc())
    }
  )
  #output the plot
  output$plot <- renderPlot({plotFunc()})
})
