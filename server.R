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
  #Toggle for data_screen argument 
  output$data_screen_arg <- renderUI({
    if(input$fileselect %in% c('input_data', 'ghana_data', 'hypnos_data', 'jhsproc_data', 'bpchildren_data', 'bppreg_data')){
      selectInput('datascreen_arg', 'Screen for Extreme Values', c('TRUE' = 't', 'FALSE' = 'f'))
    }
  })
  datascreen_tf_value <- reactive ({
    req(input$datascreen_arg)
    if(input$datascreen_arg == 'f'){
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
                                pp = "pp",
                                ToD_int = c(5, 13, 18, 23),
                                data_screen = datascreen_tf_value())
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
                             hr = "pulse.bpm.", data_screen = datascreen_tf_value())
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
                                  id = 'id', visit = 'visit',
                                  data_screen = datascreen_tf_value())
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
                                id = 'ID', data_screen = datascreen_tf_value())
    if(input$dataview == 'proc_data'){
      bppreg_proc
    }else{
      bp_preg
    }
  })
  
  ghana_data <- reactive({
    bp_ghana <- bp::bp_ghana
    bpghana_proc <- process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP', id = 'ID', data_screen = datascreen_tf_value())
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
                                  hr=hr, pp=pp, map=map,rpp=rpp, DoW=dow, data_screen = datascreen_tf_value())
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
  
  
  output$contents <- renderTable({
    user_data()
  })
  
  
  
  ################################################################################################################################  
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
      numericInput("parameter", "Specify dip_thresh Parameter",value = 0.1, step = 0.05)
    }
  })
  
  output$select_ext_parameter <- renderUI({
    if(input$metric == "dip_calc"){
      numericInput("parameter2", "Specify extreme_thresh Parameter",value = 0.2, step = 0.05)
    }
  })
  
  #add description of first parameter
  
  output$help_text <- renderUI ({
    parameter_type = parameter_type()
    
    if(parameter_type == "none"){
      helpText("No parameters need to be specified.")
    }
    else if(parameter_type == "dip_calc"){
      helpText("Enter the dip and extreme thresholds separated by comma.")
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
  
  
  metric_table <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "none")){
      string = paste("bp::", input$metric, "(data)", sep = "")
    }
    eval(parse(text = string))
  })
  output$metric_table <- DT::renderDataTable(metric_table(), extensions = "Buttons",
                                             options = list(dom = "Btip",
                                                            buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                            scrollX = TRUE))
  metric_bp_table_1 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_Counts_by_Stage
  })
  output$text_1 <- renderText({"Table 1. SBP Counts by Stage"})
  
  metric_bp_table_2 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_Counts_by_Stage
  })
  output$text_2 <- renderText({"\n Table 2. DBP Counts by Stage"})
  
  metric_bp_table_3 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Counts
  })
  output$text_3 <- renderText({"\n Table 3. CLASS Counts"})
  
  metric_bp_table_4 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$All_BP_Stage_Combinations
  })
  output$text_4 <- renderText({"\n Table 4. All BP Stage Combinations"})
  
  metric_bp_table_5 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$BP_contingency_count
  })
  output$text_5 <- renderText({"\n Table 5. BP_contingency_count"})
  
  metric_bp_table_6 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$BP_contingency_percent
  })
  output$text_6 <- renderText({"\n Table 6. BP_contingency_percent"})
  
  metric_bp_table_7 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_Day_of_Week
  })
  output$text_7 <- renderText({"\n Table 7. SBP_by_Day_of_Week"})
  
  metric_bp_table_8 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_Day_of_Week
  })
  output$text_8 <- renderText({"\n Table 8. DBP_by_Day_of_Week"})
  
  metric_bp_table_9 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Day_of_Week
  })
  output$text_9 <- renderText({"\n Table 9. CLASS_Day_of_Week"})
  
  metric_bp_table_10 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_Time_of_Day
  })
  output$text_10 <- renderText({"\n Table 10. SBP_by_Time_of_Day"})
  
  metric_bp_table_11 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_Time_of_Day
  })
  output$text_11 <- renderText({"\n Table 11. DBP_by_Time_of_Day"})
  
  metric_bp_table_12 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Time_of_Day
  })
  output$text_12 <- renderText({"\n Table 12. CLASS_Time_of_Day"})
  
  metric_bp_table_13 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_WAKE_status
  })
  output$text_13 <- renderText({"\n Table 13. SBP_by_WAKE_status"})
  
  metric_bp_table_14 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_WAKE_status
  })
  output$text_14 <- renderText({"\n Table 14. DBP_by_WAKE_status"})
  
  metric_bp_table_15 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_WAKE_perc
  })
  output$text_15 <- renderText({"\n Table 15. SBP_by_WAKE_perc"})
  
  metric_bp_table_16 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
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
  metric_dip_calc_1 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
    if (is.null(input$parameter)) {
      validate(
        need(!is.null(input$parameter), "Please wait - Rendering")
      )
    } else if (grepl(',', input$parameter) & !grepl("\\(", input$parameter)) {
      if (length(strsplit(input$parameter, split = ",")[[1]]) != 2) {
        validate (
          need(parameter_type == "dip_calc", "Please wait - Rendering")
        )
      } else {
        validate(
          need(parameter_type == "dip_calc", "Please wait - Rendering")
        )
      }
    }
    if(is.null(input$parameter) | (parameter_type == "dip_calc" & output_type == "dip_calc")){
      dip_calc_output = bp::dip_calc(data, dip_thresh = input$parameter, extreme_thresh = input$parameter2)
    }
    return(data.frame(dip_calc_output[1]))
  })
  
  metric_dip_calc_2 <- reactive({
    parameter_type = parameter_type()
    data = user_data()
    output_type = output_type()
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
  
  output$one_table <- reactive({
    input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", "bp_stats", "cv", "sv")
  })
  outputOptions(output, 'one_table', suspendWhenHidden = FALSE)
  
  output$bp_tables_tables <- reactive({
    input$metric == "bp_tables"
  })
  outputOptions(output, 'bp_tables_tables', suspendWhenHidden = FALSE)
  
  output$dip_calc_tables <- reactive({
    input$metric == "dip_calc"
  })
  outputOptions(output, 'dip_calc_tables', suspendWhenHidden = FALSE)
  
  ######PLOT######
  output$plotName <- renderText(input$fileselect)
  
  plottype <- reactive({  # wrap plottype input in a reactive for rendering UI and Plot
    if(input$plottype == "bp_scatter"){
      return("bp_scatter")
    }
    else if(input$plottype == "bp_hist"){
      return("bp_hist")
    }
  })
  
  
  ### Get subj argument for bp_scatter and bp_hist
  
  output$subj_for_scatter_and_hist <- renderUI({
    plottype = plottype()
    
    if((plottype == "bp_scatter") | (plottype == "bp_hist")){
      selectizeInput(inputId = "subj_for_scatter_and_hist", label = "Subject", choices = c("", as.character(levels(factor(user_data()$ID)))), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  ### Get group_var argument for bp_scatter
  output$group_var_for_scatter <- renderUI({
    plottype = plottype()
    
    if(plottype == "bp_scatter"){
      selectInput(inputId = "group_var_for_scatter", label = "Grouping Variable (1):", choices = c("", names(user_data()[,which(user_data() %>% summarise_all(n_distinct) <= 10)])),selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  ### Get wrap_var argument for bp_scattter
  output$wrap_var_for_scatter <- renderUI({
    plottype = plottype()
    
    if(plottype == "bp_scatter"){
      selectInput(inputId = "wrap_var_for_scatter", label = "Wrapping Variable (1):", choices = c("", names(user_data()[,which(user_data() %>% summarise_all(n_distinct) <= 10)])), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  output$plot_type_for_scatter <- renderUI({
    plottype = plottype()
    if (plottype == "bp_scatter"){
      radioButtons(inputId = "plot_type_for_scatter", label = "Plot Type", choices = c("stages2020", "AHA"), selected = "stages2020")
    }
    else{NULL}
  })
  
  
  output$include_crisis_stages2020 <- renderUI({
    plottype = plottype()
    plot_type_for_scatter = input$plot_type_for_scatter
    
    if (plottype == "bp_scatter" & plot_type_for_scatter == "stages2020"){
      checkboxInput(inputId = "inc_crisis_T_or_F",label = "Include Hypersensitive Crisis?", value = T)
    }
  })
  
  output$include_low_stages2020 <- renderUI({
    plottype = plottype()
    plot_type_for_scatter = input$plot_type_for_scatter
    
    if (plottype == "bp_scatter" & plot_type_for_scatter == "stages2020"){
      checkboxInput(inputId = "inc_low_T_or_F",label = "Include Low Hypotension?", value = T)
    }
  })
  
  
  ### Render Plot
  
  plotFunc <- reactive({
    
    plottype = plottype() # bring reactive input variable into this renderPlot call
    library(bp)
    
    if(plottype == "bp_hist"){
      
      
      if(input$fileselect == "ghana_data"){
        bp_hist(data = bp_ghana, subj = input$subj_for_scatter_and_hist)
      }
      else{
        bp_hist(data = user_data(), subj = input$subj_for_scatter_and_hist)
      }
    }
    else if(plottype == "bp_scatter"){
      bp_scatter(data = user_data(), plot_type = input$plot_type_for_scatter,
                 subj = input$subj_for_scatter_and_hist,
                 group_var = input$group_var_for_scatter,
                 wrap_var = input$wrap_var_for_scatter,
                 inc_crisis = input$inc_crisis_T_or_F, 
                 inc_low = input$inc_low_T_or_F)
    }
  })
  
  output$plot <- renderPlot({
    plotFunc()
  })
})
