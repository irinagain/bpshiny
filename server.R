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
  
  ####DATA INPUT/STORAGE####
  library(lubridate)
  library(dplyr)
  #Creates fileInput() which will accept a .csv file if User Datafile is selected
  output$file_input <- renderUI({
    if(input$fileselect == 'input_data'){
      fileInput("datafile", "Choose a CSV File", multiple = FALSE, accept = ".csv")
    }
  })  
  
  #Reactive function that will read data file and store User Inputted data
  dataset <- reactive({
    inFile <- input$datafile
    if(is.null(inFile)){
      return(NULL)
    }
    read.csv(inFile$datapath, header = T)
  })
  
  
  ####INPUT FOR COLUMN NAMES OF REQUIRED VARIABLES: SYSTOLIC,DIASTOLIC,BP_TYPE####
  
  #Creates selectInput() of all column names in User Inputted Data, and will store the selected column name corresponding to Systolic  to 'input$sys'
  output$sys_input <- renderUI({
    if(input$fileselect == 'input_data'){
      selectInput('sys', 'Systolic', '')
    }
  })
  
  #Creates selectInput() of all column names in User Inputted Data, and will store the selected column name corresponding to Diastolic  to 'input$dias'
  output$dias_input <- renderUI({
    if(input$fileselect == 'input_data'){
      selectInput('dias', 'Diastolic', '')
    }
  })
  
  #Updates Systolic and Diastolic selectInput()'s to the corresponding column names of the User Inputted data 
  observe({
    updateSelectInput(session,'sys', choices = names(dataset()))
    updateSelectInput(session, 'dias', choices = names(dataset()))
  })
  
  #Creates selectInput() of all column names in User Inputted Data, and will store the selected column name corresponding to Blood Pressure Type  to 'input$bptype_arg'
  output$bptype_input <- renderUI({
    if(input$fileselect == 'input_data'){
      selectInput('bptype_arg', 'Blood Pressure Type. Default is set to HBPM', c('HBPM' ='hbpm', 'ABPM' = 'abpm', 'AP' = 'ap'))
    }
  })
  
  #Stores Blood Pressure Type value into a reactive function for data processing
  #If sample data  was chosen, this value wil default to 'HBPM' otherwise it will take the value of the bp_type selectInput()
  bptype_value <- reactive({
    if(input$fileselect != 'input_data'){
      return('HBPM')
    }
    req(input$bptype_arg)
    if(input$bptype_arg == 'hbpm'){
      return('HBPM')
    }else if (input$bptype_arg == 'abpm'){
      return('ABPM')
    }else{
      return('AP')
    }
  })
  
  
  ####INPUTS FOR ALL OTHER COLUMN NAMES OF OPTIONAL VARIABLES####
  
  ##If user inputted data is selected, ccheckboxInput()s for all optional variables will be made##
  
  #Creates a checkboxInput() if user wants to assign a column name for the DATE/TIME variable 
  output$date_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('date1', 'Date/Time')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the DATE variable 
  output$dateonly_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('dateonly', 'Date')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the ID variable 
  output$id_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('id1', 'ID')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the WAKE variable 
  output$wake_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('wake1', 'Wake')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the VISIT variable 
  output$visit_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('visit1', 'Visit')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the HR variable 
  output$hr_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('heart1', 'Heart Rate')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the PP variable 
  output$pp_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('pp1','Pulse Pressure')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the mAP variable 
  output$map_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('map1','Mean Arterial Pressure')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the RPP variable 
  output$rpp_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('rpp1','Rate Pulse Pressure')
    }
  })
  
  #Creates a checkboxInput() if user wants to assign a column name for the DayOfWeek variable 
  output$dow_checkbox <- renderUI({
    if(input$fileselect == 'input_data'){
      checkboxInput('dow1','Day of the Week')
    }
  })
  
  ###If checkboxInput() for a given variable is TRUE, a selectInput() will appear below with column names of inputted dataset##
  
  
  #Telling User to input both Date and Date/Timme
  output$datetimelabel <- renderUI({
    if(input$fileselect %in% c('input_data')){
      h6('If Date/Time and Date are both included in dataset, please specify both below with their corresponding format. Can take any format specified by the lubridate package.')
    }
  })
  
  #If input for the DATE/TIME checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$dateinput <- renderUI({
    req(input$date1)
    if(input$date1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('datetime', 'Date/Time', names(dataset()))
    }
  })
  
  #If input for DATE/TIME checkbox is true, a textbox() will appear with dt_format() options
  output$date_adjust <- renderUI({
    req(input$date1)
    if(input$date1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      textInput('date_adj', "Specify the date/time format. Default set to 'ymd HMS'", value = 'ymd HMS')
    }
  })
  
  #Value for dt_format() argument
  date_format <- reactive({
    if(input$date1 == FALSE | is.null(input$date_adj)){
      return('ymd HMS')
    }else{
      adj = input$date_adj
      adj = as.character(adj)
      return(adj)
    }
  })
  
  #If input for DATE checkbox is true, a selctInput() will appear below with column names of inputted dataset
  output$dateonlyinput <- renderUI({
    req(input$dateonly)
    if(input$dateonly == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('dateonly_input', 'Date', names(dataset()))
    }
  })
  
  #If input for DATE checkbox is true, a textbox() will appear with dt_format() options
  output$dateonly_adjust <- renderUI({
    req(input$dateonly)
    if(input$dateonly == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      textInput('dateonly_adj', "Specify the date format. Default set to 'ymd'", value = 'ymd')
    }
  })
  
  #Value for format of Date 
  dateonly_format <- reactive({
    if(input$dateonly== FALSE | is.null(input$dateonly_adj)){
      return('ymd')
    }else{
      adj1 = input$dateonly_adj
      adj1 = as.character(adj1)
      return(adj1)
    }
  })
  
  
  #If input for the ID checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$idinput <- renderUI({
    req(input$id1)
    if(input$id1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('id', 'ID', names(dataset()))
    }
  })
  
  #If input for the WAKE checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$wakeinput <- renderUI({
    req(input$wake1)
    if(input$wake1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('wake', 'Wake', names(dataset()))
    }
  })
  
  #If input for the VISIT checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$visitinput <- renderUI({
    req(input$visit1)
    if(input$visit1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('visit', 'Visit', names(dataset()))
    }
  })
  
  #If input for the HR checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$heartinput <- renderUI({
    req(input$heart1)
    if(input$heart1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('hr', 'Heart Rate', names(dataset()))
    }
  })
  
  #If input for the PP checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$ppinput <- renderUI ({
    req(input$pp1)
    if(input$pp1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('pp', 'Pulse Pressure', names(dataset()))
    }
  })
  
  #If input for the MAP checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$mapinput <- renderUI({
    req(input$map1)
    if(input$map1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('map', 'Mean Arterial Pressure', names(dataset()))
    }
  })
  
  #If input for the RPP checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$rppinput <- renderUI({
    req(input$rpp1)
    if(input$rpp1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('rpp', 'Rate Pulse Pressure', names(dataset()))
    }
  })
  
  #If input for the DOW checkbox is true, a selectInput() will appear below with column names of inputted dataset
  output$dowinput <- renderUI({
    req(input$dow1)
    if(input$dow1 == FALSE || input$fileselect != 'input_data'){
      return(NULL)
    }else{
      selectInput('dow', 'Day of the Week', names(dataset()))
    }
  })
  
  
  ####INPUTS FOR ALL OPTIONAL ARGUMENTS####
  
  ##DISPLAY FOR OPTIONAL ARGUMENTS##
  
  #If User Inputted data is selected, will display a checkbox with option to add optional arguments 
  output$optional_arguments <- renderUI({
    if(input$fileselect %in% c('input_data')){
      checkboxInput('optional_arg', 'Display Optional Arguments')
    }
  })
  
  #Label to differentiate variable name and optional argument checkboxes
  output$optionallabel <- renderUI({
    if(input$fileselect %in% c('input_data')){
      h5('Optional Arguments')
    }
  })
  
  ##DATA_SCREEN ARGUMENT##
  
  
  #checkboxInput() for datascreen will appear if optional argument checkbox (input$optional_arg) is True 
  output$data_screen_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg)){
      checkboxInput('datascreen_check', 'Screen for extreme values in data for both SBP and DBP. Default is True', value = TRUE)
    }
  })
  
  #Reactive expression that stores data_screen() argument value of either TRUE/FALSE
  datascreen_value <- reactive ({
    if(isFALSE(input$optional_arg)){
      return(TRUE)
    }else{
      if(isTRUE(input$datascreen_check)){
        return(TRUE)
      }else{return(FALSE)}
    }
  })
  
  ##SYSTOLIC,DIASTOLIC,AND HR UPPER/LOWER BOUNDARIES ARGUMENTS##
  
  ##SYSTOLIC UPPER/LOWER LIMIT ARGUMENT##
  
  #Checkbox for SUL/SLL argument 
  output$SBP_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check)){
      checkboxInput('SBPcheck', 'Input Systolic Upper/Lower Limits to exclude SBP values from Data', value = FALSE)
    }
  })
  #Input for SUL argument
  output$SUL_input <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check) & isTRUE(input$SBPcheck)){
      textInput('SULinput', 'Input Systolic Upper Limit to exclude any values that exceed this threshold ', value = '240')
    }
  })
  #Reactive Expression that holds the SUL value
  SUL_value <- reactive({
    if(is.null(input$SULinput) | isFALSE(input$SBPcheck) | isFALSE(input$optional_arg) | isFALSE(input$datascreen_check)){
      return(240)
    }else{
      sul = as.numeric(input$SULinput)
      return(sul)
    }
    
  })
  #Input for SLL argument
  output$SLL_input <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check) & isTRUE(input$SBPcheck)){
      textInput('SLLinput', 'Input Systolic Lower Limit to exclude any values that fall below this threshold ', value = '50')
    }
  })
  #Reactive Expression that holds the SLL value
  SLL_value <- reactive({
    if(is.null(input$SLLinput) | isFALSE(input$SBPcheck) | isFALSE(input$optional_arg) | isFALSE(input$datascreen_check)){
      return(50)
    }else{
      sll = as.numeric(input$SLLinput)
      return(sll)
    }
    
  })
  ##DIASTOLIC UPPER/LOWER LIMIT ARGUMENT##
  
  #Check box for DUL/DLL argument
  output$DBP_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check)){
      checkboxInput('DBPcheck', 'Input Diastolic Upper/Lower Limits to exclude DBP values from Data', value = FALSE)
    }
  })
  #Input for DUL argument
  output$DUL_input <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check) & isTRUE(input$DBPcheck)){
      textInput('DULinput', 'Input Diastolic Upper Limit to exclude any values that exceed this threshold ', value = '140')
    }
  })
  #Reactive Expresion that holds the DUL value
  DUL_value <- reactive({
    if(is.null(input$DULinput) | isFALSE(input$DBPcheck) | isFALSE(input$optional_arg) | isFALSE(input$datascreen_check)){
      return(140)
    }else{
      dul = as.numeric(input$DULinput)
      return(dul)
    }
    
  })
  #Input for DLL argument
  output$DLL_input <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check) & isTRUE(input$DBPcheck)){
      textInput('DLLinput', 'Input Diastolic Lower Limit to exclude any values that fall below this threshold ', value = '50')
    }
  })
  #Reactive Expression that holds the DLL value
  DLL_value <- reactive({
    if(is.null(input$DLLinput) | isFALSE(input$DBPcheck) | isFALSE(input$optional_arg) | isFALSE(input$datascreen_check)){
      return(50)
    }else{
      dll = as.numeric(input$DLLinput)
      return(dll)
    }
    
  })
  
  ##HR UPPER/LOWER LIMIT ARGUMENT##
  
  #Checkbox for HRUL/HRLL ARGUMENT
  output$HR_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check)){
      checkboxInput('HRcheck', 'Input HR Upper/Lower Limits to exclude HR values from Data', value = FALSE)
    }
  })
  #Input for HRUL argument
  output$HRUL_input <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check) & isTRUE(input$HRcheck)){
      textInput('HRULinput', 'Input HR Upper Limit to exclude any values that exceed this threshold ', value = '220')
    }
  })
  #Reactive Expression that holds HRUL argument
  HRUL_value <- reactive({
    if(is.null(input$HRULinput) | isFALSE(input$HRcheck) | isFALSE(input$optional_arg) | isFALSE(input$datascreen_check)){
      return(220)
    }else{
      hrul = as.numeric(input$HRULinput)
      return(hrul)
    }
  })
  #Input for HRLL argument
  output$HRLL_input <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$datascreen_check) & isTRUE(input$HRcheck)){
      textInput('HRLLinput', 'Input HR Lower Limit to exclude any values that fall below this threshold ', value = '27')
    }
  })
  #Reactive Expression that holds HRLL argument
  HRLL_value <- reactive({
    if(is.null(input$HRLLinput) | isFALSE(input$HRcheck) | isFALSE(input$optional_arg) | isFALSE(input$datascreen_check)){
      return(27)
    }else{
      hrll = as.numeric(input$HRLLinput)
      return(hrll)
    }
  })
  ##INC_LOW ARGUMENT##
  
  #checkboxInput() for INC_LOW will appear if optional argument checkbox (input$optional_arg) is True 
  output$inc_low_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg)){
      checkboxInput('inclow_check', 'Include low category for BP classification column. Default is True', value = TRUE)
    }
  })
  
  #Reactive expression that stores inc_low() argument value of either TRUE/FALSE
  inclow_value <- reactive({
    if(isFALSE(input$optional_arg)){
      return(TRUE)
    }else{
      if(isTRUE(input$inclow_check)){
        return(TRUE)
      }else{return(FALSE)}
    }
  })
  
  ##INC_CRISIS ARGUMENT##
  
  #checkboxInput() for INC_CRISIS will appear if optional argument checkbox (input$optional_arg) is True 
  output$inc_crisis_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg)){
      checkboxInput('inccrisis_check', 'Include crisis category for BP classification column. Default is True', value = TRUE)
    }
  })
  
  #Reactive expression that stores inc_crisis() argument value as either TRUE/FALSE
  inccrisis_value <- reactive({
    if(isFALSE(input$optional_arg)){
      return(TRUE)
    }else{
      if(isTRUE(input$inccrisis_check)){
        return(TRUE)
      }else{return(FALSE)}
    }
  })
  
  ##TOD_INT ARGUMENT##
  
  #checkboxInput() for TOD_INT will appear if optional argument checkbox (input$optional_arg) is True 
  output$tod_int_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg)){
      checkboxInput('todint_check', 'Override the default interval for the Time-of-Day periods')
    }
  })
  
  
  #Will create a textInput() for TOD_INT argument if input of the tod_int checkbox is true
  output$tod_int_arg <- renderUI({
    req(input$todint_check)
    if(input$todint_check == FALSE | isFALSE(input$optional_arg)){
      return(NULL)
    }else{
      textInput('todint_arg', 'Input a vector of four numbers between 0 and 23 corresponding to the desired times. By default, the Morning, Afternoon, Evening, and Night 
                periods are set at 6,12,18,0 respectively, where 0 corresponds to the 24th hour of the day', value = '6,12,18,0') 
    }
  })
  
  #Reactive expression that stores TOD_INT() argument value AS a vector of four numbers
  todint_value <- reactive({
    if(is.null(input$todint_arg) | isFALSE(input$todint_check)){
      return(c(6,12,18,0))
    }
    req(input$todint_arg)
    if(!is.null(input$todint_arg)){
      inp <- as.numeric(unlist(strsplit(input$todint_arg,",")))
      validate(
        need(length(inp) == 4, 'Time of Day interval must be atleast 4 digits and lie within the 0-24 hour interval'),
        need(length(inp[inp >= 24]) == 0 && length(inp[inp < 0]) == 0 , 'Time of Day intervals must lie within the 0-24 hour interval')
      )
      return(inp)
    }
  })
  
  ##EOD ARGUMENT##
  
  #checkboxInput() for EOD will appear if optional argument checkbox (input$optional_arg) is True 
  output$eod_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg)){
      checkboxInput('eodcheck', 'Adjust the delineation of the end of day')
    }
  })
  
  #Will create a textInput() for eod argument if input of the eod checkbox is true
  output$eod_arg <- renderUI({
    req(input$eodcheck)
    if(input$eodcheck == FALSE | isFALSE(input$optional_arg)){
      return(NULL)
    }else{
      textInput('eodarg', "Input four digits that correspond to 24-hour time to adjust the end of day so that any readings
                in the early morning are not grouped with the next day's readings where 0000 denotes midnight", value = NULL)
    }
  })
  
  #Reactive expression that stores EOD argument value AS a four digit numeric
  eod_value <- reactive({
    if(is.null(input$eodarg) | isFALSE(input$optional_arg) | isFALSE(input$eodcheck)){
      return(NULL)
    }
    req(input$eodarg)
    if(!is.null(input$eodarg) | isFALSE(input$optional_arg)){
      inp1 <- as.numeric(input$eodarg)
      min <- inp1 %% 100
      max <- as.numeric(substr(inp1,1,2))
      validate(
        need(nchar(input$eodarg) == 4, "EOD Argument must be atleast four digits that and lie within 0-24 hour interval"),
        need(min >= 0 && min <= 59, 'EOD minute argument cannot be less than 0 or exceed 59' ),
        need(max >= 0 && max <=23, 'EOD hour argument cannot be greater than 23 or less than 00')
      )
      return(input$eodarg)
    }
  })
  
  ##AGGREGATE/COLLAPSE_DF ARGUMENTS###
  
  #checkboxInput() for AGG will appear if optional argument checkbox (input$optional_arg) is True 
  output$agg_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg)){
      checkboxInput('aggcheck', 'Aggregate the data based on the amount of time between observations. Default is False', value = FALSE)
    }
  })
  
  #Reactive expression that stores agg argument value AS a TRUE/FALSE
  agg_value <- reactive({
    if(isFALSE(input$optional_arg)){
      return(FALSE)
    }else{
      if(isTRUE(input$aggcheck)){
        return(TRUE)
      }else{return(FALSE)}
    }
  })
  
  #Will create a textInput() for agg_adj argument if input of the agg checkbox is true
  output$agg_thresh_arg <- renderUI({
    req(input$aggcheck)
    if(isFALSE(input$aggcheck) | isFALSE(input$optional_arg)){
      return(NULL)
    }else{
      textInput('aggthresh_arg', 'Specify the threshold of how many minutes can pass between readings and still be considered part of the same sitting', '3')
    }
  })
  
  #Reactive expression that stores agg_adj argument value AS a numeric
  aggthresh_value <- reactive ({
    if(isFALSE(input$aggcheck) | isFALSE(input$optional_arg)){
      return(NULL)
    }
    if(!is.null(input$aggthresh_arg)){
      inp2 <- as.numeric(input$aggthresh_arg)
      return(inp2)
    }
  })
  
  #Will create a checkboxInput() for collapse_df argument if input of the agg checkbox is true
  output$collapse_df_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg) & isTRUE(input$aggcheck)){
      checkboxInput('collapsedf_check', 'Argument that collapses data to eliminate repeating rows after aggregation. Default is False.', value = FALSE)
    }
  })
  
  #Reactive expression that stores collapse_df argument value AS a TRUE/FALSE
  collapse_value <- reactive ({
    if(isFALSE(input$optional_arg) | isFALSE(input$aggcheck)){
      return(FALSE)
    }else{
      if(isTRUE(input$collapsedf_check)){
        return(TRUE)
      }else{return(FALSE)}
    }
  })
  
  #checkboxInput() for chron_order() will appear if optional argument checkbox (input$optional_arg) is True
  output$chronorder_check <- renderUI({
    if(input$fileselect %in% c('input_data') & isTRUE(input$optional_arg)){
      checkboxInput('chron_order_check', 'Specify whether to to order the data in chronological order or reverse chronological order. Default is False which
                     corresponds to reverse chronological order', value = FALSE)
    }
  })
  
  #Reactive expression that stores chron_order argument value AS a TRUE/FALSE
  chronorder_value <- reactive({
    if(isFALSE(input$optional_arg)){
      return(FALSE)
    }else{
      if(isTRUE(input$chron_order_check)){
        return(TRUE)
      }else{return(FALSE)}
    }
  })
  
  
  #Radio button that will give the option of the data to display the original data or processed data. 
  output$dataviewer <- renderUI(
    radioButtons('dataview', label = "", choices = c('Original Data' = 'unproc_data', 'Process Data' = 'proc_data'), selected = 'unproc_data', inline = T)
  )
  
  ##SAMPLE DATASETS PROCESSING##
  
  #Reactive Expression for processed/orginial hypnos data
  #Will contain original or processed data depending on what the input$dataviewer is 
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
    hypnos_proc$DATE_TIME <- as.POSIXct(hypnos_proc$DATE_TIME)
    hypnos_proc$DATE_TIME <- format(hypnos_proc$DATE_TIME, "%m-%d-%Y %H:%M:%S")
    hypnos_proc$DATE <- format(hypnos_proc$DATE, "%m-%d-%Y")
    if(input$dataview == 'proc_data'){
      hypnos_proc
    }else{
      bp_hypnos
    }
  })
  
  #Reactive Expression for processed/orginial jhs data
  #Will contain original or processed data depending on what the input$dataviewer is 
  jhs_data <- reactive ({
    bp_jhs <- bp::bp_jhs
    date11 = as.character(bp_jhs$DateTime)
    bp_jhs <- bp_jhs%>%
      select(-DateTime)%>%
      mutate(DateTime = date11, .before = Month)
    jhs_proc <- process_data(bp_jhs,
                             sbp = "Sys.mmHg.",
                             dbp = "Dias.mmHg.",
                             date_time = "DateTime",
                             hr = "pulse.bpm.")
    jhs_proc$DATE_TIME <- as.POSIXct(jhs_proc$DATE_TIME)
    jhs_proc$DATE_TIME <- format(jhs_proc$DATE_TIME, "%m-%d-%Y %H:%M:%S")
    jhs_proc$DATE <- format(jhs_proc$DATE, "%m/%d/%Y")
    jhs_proc <- jhs_proc%>%mutate(ID = '1')
    if(input$dataview == 'proc_data'){
      jhs_proc
    }else{
      bp_jhs
    }
  })
  
  #Reactive Expression for processed/orginial children data
  #Will contain original or processed data depending on what the input$dataviewer is 
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
  
  #Reactive Expression for processed/orginial bp_preg data
  #Will contain original or processed data depending on what the input$dataviewer is 
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
  
  #Reactive Expression for processed/orginial ghana data
  #Will contain original or processed data depending on what the input$dataviewer is 
  ghana_data <- reactive({
    bp_ghana <- bp::bp_ghana
    bpghana_proc <- process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP', id = 'ID')
    if(input$dataview == 'proc_data'){
      bpghana_proc
    }else{
      bp_ghana
    }
  })
  
  ##USER INPUTTED DATA PROCESSING##
  
  
  #Reactive Expression that contains user-inputted data
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
    
    #DATE_TIME VALUE
    datetime1 = input$datetime
    if(input$date1 == FALSE){
      datetime1 = NULL
    }
    
    #DATE VALUE
    date = input$dateonly_input
    if(input$dateonly == FALSE){
      date = NULL
    }
    
    id = input$id
    #If ID checkbox is not selected, a column will be generated called 'ID' consisiting of the number 1
    #Resolves Issue with Plots Tab
    if(input$id1 == FALSE){
      id = NULL
      bpdata <- bpdata%>%mutate(ID = '1')
    }
    
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
    
    #Will either display original or processed data depending on what the input$dataviewer is 
    if(input$dataview == 'proc_data'){
      bpdata1 <- bpdata
      if(input$dateonly == TRUE){
        date_rev <- subset(bpdata1, select = input$dateonly_input)[,1]
        date_rev <- parse_date_time(date_rev, orders = dateonly_format())
        bpdata1$date <- date_rev
      }
      if(input$date1 == TRUE){
        datetime_rev <- subset(bpdata1, select = input$datetime)[,1]
        datetime_rev <- parse_date_time(datetime_rev, orders = date_format())
        bpdata1$datetime1 <- datetime_rev
      }
      bpdata_final = process_data(data = bpdata1, sbp = input$sys, dbp = input$dias,date_time = datetime1, id = id, wake = wake, visit = visit,
                                  hr=hr, pp=pp, map=map,rpp=rpp, DoW=dow, data_screen = datascreen_value(),
                                  SUL = SUL_value(), SLL = SLL_value(),DUL = DUL_value(), DLL = DLL_value(), HRUL = HRUL_value(), HRLL = HRLL_value(),
                                  bp_type = bptype_value(), inc_low = inclow_value(), inc_crisis = inccrisis_value(),
                                  ToD_int = todint_value(), eod = eod_value(),
                                  agg = agg_value(), agg_thresh = aggthresh_value(), collapse_df = collapse_value(), 
                                  chron_order = chronorder_value(), dt_fmt = date_format())
      if(isFALSE(input$date1)){
        bpdata_final
      }else{
        bpdata_final$DATE_TIME <- as.POSIXct(bpdata_final$DATE_TIME)
        bpdata_final$DATE_TIME <- format(bpdata_final$DATE_TIME, "%Y-%m-%d %H:%M:%S")
        bpdata_final$DATE <- format(bpdata_final$DATE, "%Y/%m/%d")
        bpdata_final
      }
    }else{
      bpdata
    }
    
  })
  
  
  ##DATA TO BE USED WHEN PROCESSED DATA IS NOT SELECTED##
  ##VERY SIMILAR TO PROCESSING ABOVE, EXCEPT DOES NOT DEPEND ON WHETHER THE USER HAS CHOSEN ORIGINAL/PROCESSED DATA##
  
  #Expression containg processed jhs data
  jhs_data1 <- reactive({
    bp_jhs <- bp::bp_jhs
    jhs_proc <- process_data(bp_jhs,
                             sbp = "Sys.mmHg.",
                             dbp = "Dias.mmHg.",
                             date_time = "DateTime",
                             hr = "pulse.bpm.")
    jhs_proc
  })
  
  #Expression containing processed hypnos data
  hypnos_data1 <- reactive({
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
    hypnos_proc
    
  })
  
  #Expression containing processed ghana data
  ghana_data1 <- reactive({
    bp_ghana <- bp::bp_ghana
    bpghana_proc <- process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP', id = 'ID')
    bpghana_proc
  })
  
  #Expression containing processed preg data
  preg_data1 <- reactive({
    bp_preg <- bp::bp_preg
    bppreg_proc <- process_data(bp_preg, sbp = 'SBP', dbp = 'DBP',
                                id = 'ID')
    bppreg_proc
  })
  
  #Expression containing processed children data
  children_data1 <- reactive({
    bp_children <- bp::bp_children
    children_proc <- process_data(bp_children, 
                                  sbp = 'sbp', dbp = 'dbp',
                                  id = 'id', visit = 'visit')
    children_proc
    
  })
  
  #Expression containing processed input data
  input_data1 <- reactive({
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
    
    #DATE_TIME VALUE
    datetime1 = input$datetime
    if(input$date1 == FALSE){
      datetime1 = NULL
    }
    
    #DATE VALUE
    date = input$dateonly_input
    if(input$dateonly == FALSE){
      date = NULL
    }
    
    id = input$id
    #If ID checkbox is not selected, a column will be generated called 'ID' consisiting of the number 1
    #Resolves Issue with Plots Tab
    if(input$id1 == FALSE){
      id = NULL
      bpdata <- bpdata%>%mutate(ID = '1')
    }
    
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
    
    #Will either display original or processed data depending on what the input$dataviewer is 
    bpdata1 <- bpdata
    if(input$dateonly == TRUE){
      date_rev <- subset(bpdata1, select = input$dateonly_input)[,1]
      date_rev <- parse_date_time(date_rev, orders = dateonly_format())
      bpdata1$date <- date_rev
      }
    if(input$date1 == TRUE){
      datetime_rev <- subset(bpdata1, select = input$datetime)[,1]
      datetime_rev <- parse_date_time(datetime_rev, orders = date_format())
      bpdata1$datetime1 <- datetime_rev
    }

    bpdata_final = process_data(data = bpdata1, sbp = input$sys, dbp = input$dias,date_time = datetime1, id = id, wake = wake, visit = visit,
                                hr=hr, pp=pp, map=map,rpp=rpp, DoW=dow, data_screen = datascreen_value(),
                                SUL = SUL_value(), SLL = SLL_value(),DUL = DUL_value(), DLL = DLL_value(), HRUL = HRUL_value(), HRLL = HRLL_value(),
                                bp_type = bptype_value(), inc_low = inclow_value(), inc_crisis = inccrisis_value(),
                                ToD_int = todint_value(), eod = eod_value(),
                                agg = agg_value(), agg_thresh = aggthresh_value(), collapse_df = collapse_value(), 
                                chron_order = chronorder_value(), dt_fmt = date_format())
      if(isFALSE(input$date1)){
        bpdata_final
      }else{
        bpdata_final$DATE_TIME <- as.POSIXct(bpdata_final$DATE_TIME)
        bpdata_final$DATE_TIME <- format(bpdata_final$DATE_TIME, "%Y-%m-%d %H:%M:%S")
        bpdata_final$DATE <- format(bpdata_final$DATE, "%Y/%m/%d")
        bpdata_final
      }
  })
  
  #Reactive Expression that contains processed data. Can be used regardless input$dataview
  original_data <- reactive({
    datachoice = input$fileselect
    switch(datachoice,'ghana_data' = ghana_data1(), 'hypnos_data' = hypnos_data1(), 'jhsproc_data' = jhs_data1(), 'bpchildren_data' = children_data1(),
           'bppreg_data' = preg_data1(), 'input_data' = input_data1())
  })
  
  
  
  ##FINAL DATA DISPLAY##
  
  #Reactive Expression that will either display the sample dataset, or the user inputted data
  #Depends on what dataset was selected
  user_data <- reactive ({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Uploaded Data will be seen here")
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information"),
        need(expr = input$dias != '', message = "Enter Diastolic Information")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic data is entered properly")
      )
    }
    
    datachoice = input$fileselect
    switch(datachoice,'ghana_data' = ghana_data(), 'hypnos_data' = hypnos_data(), 'jhsproc_data' = jhs_data(), 'bpchildren_data' = children_data(),
           'bppreg_data' = preg_data(), 'input_data' = input_data())
  })
  
  output$contents <- renderTable({
    user_data()
  })
  
  
  
  ############################# METRIC SECTION ######################################################
  
  
  #add metric based on the parameter it takes in
  parameter_type <- reactive({
    #metric is considered as parameter type "none" if it only requires data as a parameter (no additional user input)
    if(input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", "bp_stats", "bp_tables", "cv", "sv")){
      return("none")
    }
    #dip_calc function requires user to input 2 additional parameters (dipping threshold and extreme threshold)
    if(input$metric %in% c("dip_calc")){
      return("dip_calc")
    }
  })
  
  output_type <- reactive({
    #these functions output 1 table
    if(input$metric %in% c("arv", "bp_center", "bp_mag", "bp_range", "bp_stats", "cv", "sv")){
      return("none")
    }
    #bp_tables function needs to render 16 tables separately
    if(input$metric == "bp_tables"){
      return("tables") 
    }
    #dip_calc function needs to render 2 tables separately
    if(input$metric == "dip_calc"){
      return("dip_calc")
    }
    
  })
  #specify dip_calc parameters and the default values
  output$select_dip_parameter <- renderUI({
    if(input$metric == "dip_calc"){
      #dipping threshold, default is  0.1
      numericInput("parameter1", "Specify dipping threshold",value = 0.1, step = 0.05, min = 0, max = .95)
    }
  })
  
  output$select_ext_parameter <- renderUI({
    if(input$metric == "dip_calc"){
      #extreme threshold, default if 0.2
      numericInput("parameter2", "Specify extreme dipping threshold",value = 0.2, step = 0.05, min = input$parameter1 + .05, max = 1)
    }
  })
  
  
  #add description of parameters (help text)
  output$help_text <- renderUI ({
    parameter_type = parameter_type()
    
    if(parameter_type == "none"){
      #no parameter
      helpText("No parameters need to be specified.")
    }
    else if(parameter_type == "dip_calc"){
      #parameters for dip_calc
      helpText("Enter the dipping and extreme dipping thresholds between 0 and 1. ")
    }
  })
  
  ### metrics other than bp_tables and dip_calc
  #parameter_type==none, output_type==none (functions that do not need additional parameters and will only output 1 table)
  metric_table <- reactive({
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    parameter_type = parameter_type()
    #If/else statement that decides whether to use sample data that is processed in data tab
    #Or if original data is still selected, it will use the pre-proccessed sample data
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    #validate/need argument, eliminates error popping up when changing parameter type
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "none", "output type incorrect")
    )
    #apply bp functions 
    if(parameter_type == "none" & output_type == "none"){
      string = paste("bp::", input$metric, "(data)", sep = "")
    }
    eval(parse(text = string))
  })
  #render output from metric_table (1 table)
  output$metric_table <- DT::renderDataTable(metric_table(), extensions = "Buttons",
                                             options = list(dom = "Btip",
                                                            buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                            scrollX = TRUE))
  
  ### bp_tables
  #separate the output(a list of 16 tables) into 16 reactive outputs and render them separately
  
  #table 1 (SBP_Counts_by_Stage)
  metric_bp_table_1 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    #validate/need argument, eliminates error popping up when changing parameter type
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    #does not need additional parameters
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_Counts_by_Stage
  })
  output$text_1 <- renderText({"Table 1. SBP Counts by Stage"}) #act as caption for table 1
  
  metric_bp_table_2 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  metric_bp_table_3 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  metric_bp_table_4 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  metric_bp_table_5 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  metric_bp_table_6 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  metric_bp_table_7 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    ## need DAY_OF_WEEK column to calculate SBP_by_DAY_OF_WEEK(metric_bp_table_7), DBP_by_DAY_OF_WEEK(8), and CLASS_by_DAY_OF_WEEK(9), details seen in bp::bp_tables function
    validate(
      need("DAY_OF_WEEK" %in% names(data), "N/A - DAY_OF_WEEK column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_Day_of_Week
  })
  output$text_7 <- renderText({"\n Table 7. SBP_by_Day_of_Week"})
  
  metric_bp_table_8 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
      need("DAY_OF_WEEK" %in% names(data), "N/A - DAY_OF_WEEK column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_Day_of_Week
  })
  output$text_8 <- renderText({"\n Table 8. DBP_by_Day_of_Week"})
  
  metric_bp_table_9 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
      need("DAY_OF_WEEK" %in% names(data), "N/A - DAY_OF_WEEK column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Day_of_Week
  })
  output$text_9 <- renderText({"\n Table 9. CLASS_Day_of_Week"})
  
  metric_bp_table_10 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    ## need TIME_OF_DAY column to calculate SBP_by_Time_of_Day(metric_bp_table_10), DBP_by_Time_of_Day(11), and CLASS_by_Time_of_Day(12), details seen in bp::bp_tables function
    validate(
      need("TIME_OF_DAY" %in% names(data), "N/A - TIME_OF_DAY column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_Time_of_Day
  })
  output$text_10 <- renderText({"\n Table 10. SBP_by_Time_of_Day"})
  
  metric_bp_table_11 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
      need("TIME_OF_DAY" %in% names(data), "N/A - TIME_OF_DAY column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$DBP_by_Time_of_Day
  })
  output$text_11 <- renderText({"\n Table 11. DBP_by_Time_of_Day"})
  
  metric_bp_table_12 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
      need("TIME_OF_DAY" %in% names(data), "N/A - TIME_OF_DAY column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$CLASS_Time_of_Day
  })
  output$text_12 <- renderText({"\n Table 12. CLASS_Time_of_Day"})
  
  metric_bp_table_13 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "none", "parameter type incorrect"),
      need(output_type == "tables", "output type incorrect")
    )
    ## need wake column to calculate sbp_by_wake_status(metric_bp_table_13), dbp_by_wake_status(14), sbp_by_wake_perc(15), and dbp_by_wake_perc(16), details seen in bp::bp_tables function
    validate(
      need("WAKE" %in% names(data), "N/A - WAKE column not available")
    )
    if(is.null(input$parameter) | (parameter_type == "none" & output_type == "tables")){
      tables_output = bp::bp_tables(data)
    }
    tables_output$SBP_by_WAKE_status
  })
  output$text_13 <- renderText({"\n Table 13. SBP_by_WAKE_status \n"})
  
  metric_bp_table_14 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  metric_bp_table_15 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  metric_bp_table_16 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
    }
    
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
  
  #rendering the list of 16 tables one by one
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
  
  ### dip_calc
  metric_dip_calc_1 <- reactive({
    req(input$parameter1, input$parameter2) #make sure dip_thresh(input$parameter1) and extreme_thresh(input$parameter2) are not null
    parameter_type = parameter_type()
    
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    #validate/need argument, eliminates error popping up when changing parameter type
    validate (
      need(parameter_type == "dip_calc", "parameter type incorrect"),
      need(output_type == "dip_calc", "output type incorrect")
    )
    
    validate(
      need(input$parameter1 < input$parameter2, message = "Ensure Dipping Threshold is less than Extreme Dipping Threshold")
    )
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
      
      validate(
        need(expr = any(c("DATE_TIME", "WAKE") %in% names(data)), message = "DATE_TIME or WAKE column is needed for Noctural Blood Pressure Dipping Calculation. A processed data set may be required."  )
      )
    }
    
    
    if(parameter_type == "dip_calc" & output_type == "dip_calc"){
      dip_calc_output = bp::dip_calc(data, sleep_start_end = NULL, dip_thresh = input$parameter1, extreme_thresh = input$parameter2, 
                                     inc_date = FALSE, subj = NULL)
    }
    return(data.frame(dip_calc_output[1]))
  })
  
  metric_dip_calc_2 <- reactive({
    
    #This if statement prevents error message that occurs when data isn't properly entered in Data Tab
    if(input$fileselect == "input_data") {
      validate(
        need(expr = !is.null(input$datafile), message = "Ensure Data has been uploaded in Data Tab"  )
      )
      validate(
        need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
        need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
      )
      validate(
        need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
      )
      
      validate(
        need(expr = any(c("DATE_TIME", "WAKE") %in% names(data)), message = "DATE_TIME or WAKE column is needed for Noctural Blood Pressure Dipping Calculation. A processed data set may be required."  )
      )
    
    }
    
    req(input$parameter1, input$parameter2)
    parameter_type = parameter_type()
    if(input$dataview == 'proc_data'){
      data = user_data()
    }else{data = original_data()}
    output_type = output_type()
    validate (
      need(parameter_type == "dip_calc", "parameter type incorrect"),
      need(output_type == "dip_calc", "output type incorrect")
    )
    
    validate(
      need(input$parameter1 < input$parameter2, message = "Ensure Dipping Threshold is less than Extreme Dipping Threshold")
    )
    
    if(parameter_type == "dip_calc" & output_type == "dip_calc"){
      dip_calc_output = bp::dip_calc(data, sleep_start_end = NULL, dip_thresh = input$parameter1, extreme_thresh = input$parameter2, 
                                     inc_date = FALSE, subj = NULL)
    }
    return(data.frame(dip_calc_output[2]))
  })
  #render 2 tables for dip_calc separately
  output$metric_dip_calc_1 <- DT::renderDataTable(metric_dip_calc_1(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  output$metric_dip_calc_2 <- DT::renderDataTable(metric_dip_calc_2(), extensions = "Buttons",
                                                  options = list(dom = "Btip",
                                                                 buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                                 scrollX = TRUE))
  ### 3 output conditions linked with conditionalpanels in UI. 
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
  
  #Get name of dataset store name in output$plotName
  plotName <- eventReactive(input$plot_update, {input$fileselect})
  output$plotName <- plotName
  
  #Get the type of plot the user wants to render
  #Wrap plottype input in a reactive for rendering UI and Plot
  plottype <- reactive({
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
  
  #get the name of the type of plot the user wants to render & store it in output$plot_type_text
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
  
 
  #Get the subject arguments that is used in all plot types 
  output$subj_for_plots<- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
      
        validate(
          need(input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
        )
      
      validate(
        need(expr = (!is.null(input_data1()$ID) || !is.null(input_data()$ID)), message = "No subject 'ID' column is found, select Processed Data on 'Data' Tab if necessary")
      )
      if (!is.null(input_data1()$ID)){
        selectizeInput(inputId = "subj_for_plots", label = "Subject:", choices = c("", as.character(levels(factor(input_data1()$ID)))), selected = NULL, multiple = T)
      }
      else if (!is.null(input_data()$ID)){
        selectizeInput(inputId = "subj_for_plots", label = "Subject:", choices = c("", as.character(levels(factor(input_data()$ID)))), selected = NULL, multiple = T)
      }
      
    }
    
    if(input$fileselect != "bpchildren_data"){
      if((plottype == "bp_scatter") | (plottype == "bp_hist") | (plottype == "bp_report") | (plottype == "dow_tod_plots") | (plottype == "bp_ts_plots")){
        selectizeInput(inputId = "subj_for_plots", label = "Subject:", choices = c("", as.character(levels(factor(user_data()$ID)))), selected = NULL, multiple = T)
      }
      else{NULL}
    }
    
    else if(input$fileselect == "bpchildren_data"){
      if((plottype == "bp_scatter") | (plottype == "bp_hist") | (plottype == "bp_report") | (plottype == "dow_tod_plots") | (plottype == "bp_ts_plots")){
        selectizeInput(inputId = "subj_for_plots", label = "Subject:", choices = c("", as.numeric(levels(factor(children_data1()$ID)))), selected = NULL, multiple = T)
      }
      else{NULL}
    }
    else{NULL}
  })
  
  ### Get group_var argument for bp_scatter & bp_report
  output$group_var_for_scatter_and_report <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    if((plottype == "bp_scatter") | (plottype == "bp_report") ){
      selectInput(inputId = "group_var_for_scatter_and_report", label = "Grouping Variable:", choices = c("", names(user_data()[,which(user_data() %>% summarise_all(n_distinct) <= 10)])),selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  ### Get wrap_var argument for bp_scattter 
  output$wrap_var_for_scatter <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    if((plottype == "bp_scatter")){
      selectInput(inputId = "wrap_var_for_scatter", label = "Wrapping Variable:", choices = c("", names(user_data()[,which(user_data() %>% summarise_all(n_distinct) <= 10)])), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  #Get the user to choose between AHA or Stages 2020 plot type used exclusively in the bp_scatter function
  output$plot_type_for_scatter <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    if (plottype == "bp_scatter"){
      radioButtons(inputId = "plot_type_for_scatter", label = "Plot Type", choices = c("stages2020", "AHA"), selected = "stages2020")
    }
    else{NULL}
  })
  
  
  #If the user selects stages2020, they have the option to render the crisis category
  output$include_crisis_stages2020 <- renderUI({
    plottype = plottype()
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    if (!(is.null(input$plot_type_for_scatter))){
      
      
      
      plot_type_for_scatter = input$plot_type_for_scatter
      
      if ((plottype == "bp_scatter" & plot_type_for_scatter == "stages2020") | (plottype == "bp_report")) {
        checkboxInput(inputId = "inc_crisis_T_or_F",label = "Include Hypersensitive Crisis", value = T)
      }
      else{NULL}
    }
    else{NULL}
  })
  
  #if the user selects stages2020, they have the option to render the low category
  output$include_low_stages2020 <- renderUI({
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    plottype = plottype()
    if (!(is.null(input$plot_type_for_scatter))){
      
      
      plot_type_for_scatter = input$plot_type_for_scatter
      
      if ((plottype == "bp_scatter" & plot_type_for_scatter == "stages2020") | (plottype == "bp_report")){
        checkboxInput(inputId = "inc_low_T_or_F",label = "Include Low Hypotension", value = T)
      }
      else{NULL}
    }
    else{NULL}
  })

  
  ### Arguments for bp_ts_plots
  output$wrap_var_for_ts <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    if(plottype == "bp_ts_plots"){
      selectInput(inputId = "wrap_var_for_ts", label = "Wrapping Variable:", choices = c("", names(user_data())), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  ### If user wants to have wrapping argument in time series plot, additional arguments need to be provided by user
  output$wrap_rowcol_for_ts <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    wrap_var_for_ts = input$wrap_var_for_ts
    
    if ((plottype == "bp_ts_plots") && !is.null(wrap_var_for_ts)){
      wellPanel(
        fluidRow(
          column(width = 4, 
                 numericInput(inputId = "wrap_col_for_ts", 
                              label = "# Columns", 
                              value = 1, min = 1, max = 9, step = 1)
          ),
          column(width = 4,
                 numericInput(inputId = "wrap_row_for_ts", 
                              label = "# Rows",value = 1, min = 1, max = 9, step = 1)
          )
        ))
      
    }
    else{NULL}
  })
  output$index_for_ts <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    if(plottype == "bp_ts_plots"){
      selectInput(inputId = "index_for_ts", label = "Index:", choices = c("", names(user_data())), selected = NULL, multiple = T)
    }
    else{NULL}
  })
  
  #First hour argument for time series plot
  output$first_hour_for_ts <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    
    if(plottype == "bp_ts_plots"){
      numericInput(inputId = "first_hour_for_ts", label = "First Hour (0-23):", value = 0, min = 0, max = 23)
    }
    else{NULL}
  })
  
  #rotate x axis label argument for time series plot
  output$rotate_xlab_for_ts <- renderUI({
    plottype = plottype()
    req(input$fileselect)
    
    if (input$fileselect == "input_data"){
      req(input$datafile)
    }
    
    
    if (plottype == "bp_ts_plots"){
      checkboxInput(inputId = "rotate_xlab_for_ts", label = "Rotate x-axis labels", value = F)
    }
    else{NULL}
  })
  
 
  
  #there are multiple plots to view with bp_hist, this lets user choose which plot to view
  output$bp_hist_view <- renderUI(
    if (plottype() == "bp_hist"){
      selectInput(inputId = "bp_hist_view", label = NULL, choices = c(`# of Readings per BP Classification` = "1", 
                                                                      `Frequency of SBP Readings` = "2", 
                                                                      `Frequency of DBP Readings` = "3"),
                  selected = 1, size = 1, selectize = F)
    }
    else{NULL}
  )
  
  ### Render Plot
  
  #Make a event reactive object that will update whenever the user interacts with the action button "Render" in the plot section.
  #plotFunc is now an object that can be fed into a renderPlot element in the output list
  plotFunc <- eventReactive(input$plot_update,{
    
    plottype = plottype() # bring reactive input variable into this renderPlot call
    library(bp)
    
    #React to plottype call
    
    #If the user wants to render bp_hist
    if(plottype == "bp_hist"){
      #Makes sure a data set has been uploaded/selected
      validate(
        need(expr = input$fileselect != '', message = "Please upload/select a data set")
      )
      
      
      
      #if the user uploads their own data, this makes sure they've entered systolic and diastolic information, and then casts plotFunc() properly
      if (input$fileselect == "input_data"){
        validate(
          need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
          need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
        )
        validate(
          need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
        )
        
        bp_hist(data = input_data1(), 
                subj = input$subj_for_plots)[as.numeric(input$bp_hist_view)]
      } 
      #if the user wants to do bp_hist on unprocessed jhs data
      else if (input$fileselect == "jhsproc_data") {
        bp_hist(data = {jhs_data1()},
                subj = input$subj_for_plots)[as.numeric(input$bp_hist_view)]
      }
      
      #if the user wants to do bp_hist on unprocessed hypnos data
      else if (input$fileselect == "hypnos_data"){
        bp_hist(data = {hypnos_data1()},
                subj = input$subj_for_plots)[as.numeric(input$bp_hist_view)]
      }
      
      #if the user wants to do bp_hist on unprocessed ghana data
      else if (input$fileselect == "ghana_data"){
        bp_hist(data = {ghana_data1()}, subj = input$subj_for_plots)[as.numeric(input$bp_hist_view)]
      }
      
      else if (input$fileselect == "bpchildren_data"){
        bp_hist(data = {
          children_data1()
        }, subj = input$subj_for_plots)[as.numeric(input$bp_hist_view)]
      }
      
      else if (input$fileselect == "bppreg_data"){
        bp_hist(data = {
          preg_data1()
        }, subj = input$subj_for_plots)[as.numeric(input$bp_hist_view)]
      }
    }
    
    #If the user wants to render bp_scatter
    else if(plottype == "bp_scatter"){
      #Makes sure a data set has been uploaded/selected
      validate(
        need(expr = input$fileselect != '', message = "Please upload/select a data set")
      )
      
      #Makes sure only 1 grouping variable is chosen
      validate(
        need(expr = length(input$group_var_for_scatter_and_report) <= 1, message = "Ensure there is only one (1) entry for Grouping Variable")
      )
      
      #Makes sure only 1 wrapping variable is chosen
      validate(
        need(expr = length(input$wrap_var_for_scatter) <= 1, message = "Ensure there is only one (1) entry for Wrapping Variable")
      )
      #if the user uploads their own data, this makes sure they've entered systolic and diastolic information, and then casts plotFunc() properly
      if (input$fileselect == "input_data"){
        validate(
          need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
          need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
        )
        validate(
          need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
        )
        bp_scatter(data = original_data(),
                   plot_type = input$plot_type_for_scatter,
                   subj = input$subj_for_plots,
                   group_var = input$group_var_for_scatter_and_report,
                   wrap_var = input$wrap_var_for_scatter,
                   inc_crisis = input$inc_crisis_T_or_F, 
                   inc_low = input$inc_low_T_or_F)
        
      }
      
      
      #if the user wants to use bp_scatter on unprocessed jhs data
      else if (input$fileselect == "jhsproc_data") {
        bp_scatter(data = {jhs_data1()}, 
                   plot_type = input$plot_type_for_scatter,
                   subj = input$subj_for_plots,
                   group_var = input$group_var_for_scatter_and_report,
                   wrap_var = input$wrap_var_for_scatter,
                   inc_crisis = input$inc_crisis_T_or_F, 
                   inc_low = input$inc_low_T_or_F)
      }
      #if the user wants to use bp_scatter on the unprocessed hypnos data
      else if (input$fileselect == "hypnos_data"){
        bp_scatter(data = {hypnos_data1()},
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
      #Makes sure a data set has been uploaded/selected
      validate(
        need(expr = input$fileselect != '', message = "Please upload/select a data set")
      )
      #Makes sure only 1 grouping variable is chosen
      validate(
        need(expr = length(input$group_var_for_scatter_and_report) <= 1, message = "Ensure there is only one (1) entry for Grouping Variable")
      )
      #If the user wants to user wants to use bp_report for data that isn't unprocessed jhs or unprocessed hypnos
      if(input$fileselect == "input_data") {
        validate(
          need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
          need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
        )
        validate(
          need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
        )
        # 
        bp_report(data = {tibble::as_data_frame(input_data1())},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  scale = .75)
      }
      
      else if (input$fileselect == "ghana_data"){
        bp_report(data = {process_data(data = bp::bp_ghana, sbp = "SBP", dbp = "DBP")},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  scale = .75)
      }
      
      else if(input$fileselect == "bpchildren_data"){
        bp_report(data = {children_data1()},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  scale = .75)
      }
      
      else if(input$fileselect == "bppreg_data"){
        bp_report(data = {preg_data1()},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  scale = .75)
      }
      #if the user wants to use bp_report on unprocessed jhs data
      else if (input$fileselect == "jhsproc_data") {
        bp_report(data = {jhs_data1()},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  scale = .75)
      }
      #if the user wants to use bp_report on unprocessed hypnos data
      else if (input$fileselect == "hypnos_data"){
        bp_report(data = {hypnos_data1()},
                  subj = input$subj_for_plots,
                  inc_low = input$inc_low_T_or_F,
                  inc_crisis = input$inc_crisis_T_or_F,
                  group_var = input$group_var_for_scatter_and_report,
                  scale = .75)
      }
    }
    
    #if the user wants to render the dow_tod_plots() 
    else if(plottype == "dow_tod_plots"){
      
      #Makes sure a data set has been uploaded/selected
      validate(
        need(expr = input$fileselect != '', message = "Please upload/select a data set")
      )
      
     
      
      
      
      #if the user wants to dow_tod_plots an uploaded dataset
      if(input$fileselect == "input_data") {
        validate(
          need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
          need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
        )
        validate(
          need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
        )
        
        validate(
          need(expr = (input$dow != '' || input$datetime != ''), message = "Please specify Day of Week and/or Date/Time Column in Data Tab")
        )
       
        
        dow_tod_plots_out <- dow_tod_plots(data = input_data1(),
                                           subj = input$subj_for_plots)
      }
      
      else if(input$fileselect == "ghana_data"){
        dow_tod_plots_out <- dow_tod_plots(data = {ghana_data1()},
                                           subj = input$subj_for_plots)
      }
      
      else if(input$fileselect == "bppreg_data"){
        dow_tod_plots_out <- dow_tod_plots(data = {preg_data1()},
                                           subj = input$subj_for_plots)
      }
      
      else if(input$fileselect == "bpchildren_data"){
        dow_tod_plots_out <- dow_tod_plots(data = {preg_data1()},
                                           subj = input$subj_for_plots)
      }
      
      #if the user wants to dow_tod_plots the jhs data
      else if (input$fileselect == "jhsproc_data") {
        dow_tod_plots_out <- dow_tod_plots(data = {jhs_data1()},
                                           subj = input$subj_for_plots)
      }
      #if the user wants to dow_tod_plots the hypnos dataset
      else if (input$fileselect == "hypnos_data"){
        dow_tod_plots_out <- dow_tod_plots(data = {hypnos_data1()},
                                           subj = input$subj_for_plots)
      }
      
      #use grid & gridExtra package to arrange the list of plots created by the dow_tod_plots() function
      grid::grid.draw(
        gridExtra::grid.arrange(dow_tod_plots_out[[1]], dow_tod_plots_out[[2]], ncol = 2)
      )
    }
    
    #If the user wants to render the bp_ts_plots
    else if(plottype == "bp_ts_plots"){
      
      #Makes sure a data set has been uploaded/selected
      validate(
        need(expr = input$fileselect != '', message = "Please upload/select a data set")
      ) 
      
      validate(
        need(expr = length(input$subj_for_plots) == 1, message = "Please specifty 1 subject, if necessary process the data in the 'Data' tab")
      )
      
      if(input$fileselect == "hypnos_data"){
        validate(
          need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
        )
        
        validate(
          need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
        )
        bp_ts_plots(data = {hypnos_data1()},
                    subj = input$subj_for_plots,
                    wrap_var = input$wrap_var_for_ts, 
                    index = input$index_for_ts, 
                    first_hour = input$first_hour_for_ts,
                    rotate_xlab = input$rotate_xlab_for_ts,
                    wrap_row = input$wrap_row_for_ts, 
                    wrap_col = input$wrap_col_for_ts)[1]
      }
      else if(input$fileselect == "ghana_data"){
        validate(
          need(expr = length(input$index_for_ts) == 1, message = "Please specify Index")
        )
        validate(
          need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
        )
        
        validate(
          need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
        )
        bp_ts_plots(data = {ghana_data1()},
                    subj = input$subj_for_plots,
                    wrap_var = input$wrap_var_for_ts, 
                    index = input$index_for_ts, 
                    first_hour = input$first_hour_for_ts,
                    rotate_xlab = input$rotate_xlab_for_ts,
                    wrap_row = input$wrap_row_for_ts, 
                    wrap_col = input$wrap_col_for_ts)[1]
      }
      else if(input$fileselect == "jhsproc_data"){
        validate(
          need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
        )
        
        validate(
          need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
        )
        bp_ts_plots(data = {jhs_data1()},
                    subj = input$subj_for_plots,
                    wrap_var = input$wrap_var_for_ts, 
                    index = input$index_for_ts, 
                    first_hour = input$first_hour_for_ts,
                    rotate_xlab = input$rotate_xlab_for_ts,
                    wrap_row = input$wrap_row_for_ts, 
                    wrap_col = input$wrap_col_for_ts)[1]
      }
      else if(input$fileselect == "bpchildren_data"){
        validate(
          need(expr = length(input$index_for_ts) == 1, message = "Please specify Index")
        )
        validate(
          need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
        )
        
        validate(
          need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
        )
        bp_ts_plots(data = {children_data1()},
                    subj = input$subj_for_plots,
                    wrap_var = input$wrap_var_for_ts, 
                    index = input$index_for_ts, 
                    first_hour = input$first_hour_for_ts,
                    rotate_xlab = input$rotate_xlab_for_ts,
                    wrap_row = input$wrap_row_for_ts, 
                    wrap_col = input$wrap_col_for_ts)[1]
      }
      else if(input$fileselect == "bppreg_data"){
        validate(
          need(expr = length(input$index_for_ts) == 1, message = "Please specify Index")
        )
        validate(
          need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
        )
        
        validate(
          need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
        )
        bp_ts_plots(data = {preg_data1()},
                    subj = input$subj_for_plots,
                    wrap_var = input$wrap_var_for_ts, 
                    index = input$index_for_ts, 
                    first_hour = input$first_hour_for_ts,
                    rotate_xlab = input$rotate_xlab_for_ts,
                    wrap_row = input$wrap_row_for_ts, 
                    wrap_col = input$wrap_col_for_ts)[1]
        
      }
      else if(input$fileselect == "input_data"){
        validate(
          need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
          need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
        )
        validate(
          need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
        )
        
        validate(
          need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
        )
        
        if((!is.null(input$date)) || (!is.null(input$datetime)) ){
          validate(
            need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
          )
          bp_ts_plots(data = {input_data1()},
                      subj = input$subj_for_plots,
                      wrap_var = input$wrap_var_for_ts, 
                      index = input$index_for_ts, 
                      first_hour = input$first_hour_for_ts,
                      rotate_xlab = input$rotate_xlab_for_ts,
                      wrap_row = input$wrap_row_for_ts, 
                      wrap_col = input$wrap_col_for_ts)[1]
        }
        else{
          validate(
            need(expr = length(input$index_for_ts) == 1, message = "Uploaded data sets require specified Index for Time Series plotting")
          )
          validate(
            need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
          )
          
          bp_ts_plots(data = {input_data1()},
                      subj = input$subj_for_plots,
                      wrap_var = input$wrap_var_for_ts, 
                      index = input$index_for_ts, 
                      first_hour = input$first_hour_for_ts,
                      rotate_xlab = input$rotate_xlab_for_ts,
                      wrap_row = input$wrap_row_for_ts, 
                      wrap_col = input$wrap_col_for_ts)[1]
        }
      }
    }
    
    
  })
  
 second_plot_for_ts <- eventReactive(input$plot_update, {
  
   if(input$plottype == "bp_ts_plots"){
    #Makes sure a data set has been uploaded/selected
   validate(
     need(expr = input$fileselect != '', message = "Please upload/select a data set")
   ) 
   
   validate(
     need(expr = length(input$subj_for_plots) == 1, message = "Please specifty 1 subject, if necessary process the data in the 'Data' tab")
   )
   
   if(input$fileselect == "hypnos_data"){
     validate(
       need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
     )
     
     validate(
       need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
     )
     bp_ts_plots(data = {hypnos_data1()},
                 subj = input$subj_for_plots,
                 wrap_var = input$wrap_var_for_ts, 
                 index = input$index_for_ts, 
                 first_hour = input$first_hour_for_ts,
                 rotate_xlab = input$rotate_xlab_for_ts,
                 wrap_row = input$wrap_row_for_ts, 
                 wrap_col = input$wrap_col_for_ts)[2]
   }
   else if(input$fileselect == "ghana_data"){
     validate(
       need(expr = length(input$index_for_ts) == 1, message = "Please specify Index")
     )
     validate(
       need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
     )
     
     validate(
       need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
     )
     bp_ts_plots(data = {ghana_data1()},
                 subj = input$subj_for_plots,
                 wrap_var = input$wrap_var_for_ts, 
                 index = input$index_for_ts, 
                 first_hour = input$first_hour_for_ts,
                 rotate_xlab = input$rotate_xlab_for_ts,
                 wrap_row = input$wrap_row_for_ts, 
                 wrap_col = input$wrap_col_for_ts)[2]
   }
   else if(input$fileselect == "jhsproc_data"){
     validate(
       need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
     )
     
     validate(
       need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
     )
     bp_ts_plots(data = {jhs_data1()},
                 subj = input$subj_for_plots,
                 wrap_var = input$wrap_var_for_ts, 
                 index = input$index_for_ts, 
                 first_hour = input$first_hour_for_ts,
                 rotate_xlab = input$rotate_xlab_for_ts,
                 wrap_row = input$wrap_row_for_ts, 
                 wrap_col = input$wrap_col_for_ts)[2]
   }
   else if(input$fileselect == "bpchildren_data"){
     validate(
       need(expr = length(input$index_for_ts) == 1, message = "Please specify Index")
     )
     validate(
       need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
     )
     
     validate(
       need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
     )
     bp_ts_plots(data = {children_data1()},
                 subj = input$subj_for_plots,
                 wrap_var = input$wrap_var_for_ts, 
                 index = input$index_for_ts, 
                 first_hour = input$first_hour_for_ts,
                 rotate_xlab = input$rotate_xlab_for_ts,
                 wrap_row = input$wrap_row_for_ts, 
                 wrap_col = input$wrap_col_for_ts)[2]
   }
   else if(input$fileselect == "bppreg_data"){
     validate(
       need(expr = length(input$index_for_ts) == 1, message = "Please specify Index")
     )
     validate(
       need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
     )
     
     validate(
       need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
     )
     bp_ts_plots(data = {preg_data1()},
                 subj = input$subj_for_plots,
                 wrap_var = input$wrap_var_for_ts, 
                 index = input$index_for_ts, 
                 first_hour = input$first_hour_for_ts,
                 rotate_xlab = input$rotate_xlab_for_ts,
                 wrap_row = input$wrap_row_for_ts, 
                 wrap_col = input$wrap_col_for_ts)[2]
     
   }
   else if(input$fileselect == "input_data"){
     validate(
       need(expr = input$sys != '', message = "Enter Systolic Information in Data Tab"),
       need(expr = input$dias != '', message = "Enter Diastolic Information in Data Tab")
     )
     validate(
       need(expr = input$sys != input$dias, message = "Ensure Systolic and Diastolic information provided in the 'Data' tab are different")
     )
     
     validate(
       need(expr = length(input$wrap_rowcol_for_ts) < 2, message = "Ensure only 1 Wrapping Variable is given")
     )
     if((!is.null(input$date)) || (!is.null(input$datetime)) ){
       validate(
         need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
       )
       bp_ts_plots(data = {input_data1()},
                   subj = input$subj_for_plots,
                   wrap_var = input$wrap_var_for_ts, 
                   index = input$index_for_ts, 
                   first_hour = input$first_hour_for_ts,
                   rotate_xlab = input$rotate_xlab_for_ts,
                   wrap_row = input$wrap_row_for_ts, 
                   wrap_col = input$wrap_col_for_ts)[2]
     }
     else{
       validate(
         need(expr = length(input$index_for_ts) == 1, message = "Uploaded data sets require specified Index for Time Series plotting")
       )
       validate(
         need(expr = length(input$index_for_ts) < 2, message = "Ensure only 1 Index value is given")
       )
       
       bp_ts_plots(data = {input_data1()},
                   subj = input$subj_for_plots,
                   wrap_var = input$wrap_var_for_ts, 
                   index = input$index_for_ts, 
                   first_hour = input$first_hour_for_ts,
                   rotate_xlab = input$rotate_xlab_for_ts,
                   wrap_row = input$wrap_row_for_ts, 
                   wrap_col = input$wrap_col_for_ts)[2]
     }
   }
   }
   else{NULL}
 })
 
 
  output$plot_length <- renderText(length(plotFunc()))
  
  
  #output the plot(s)
  output$plot <- renderPlot({
    plotFunc()
  })
  
  output$second_plot_for_ts <- renderPlot({
    second_plot_for_ts()
  })
 
})