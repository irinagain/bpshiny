library(shiny)
library(DT)
library(bp)

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

ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny BP"),
  
# Create tabset for Panel layout of Data, Metrics, and Plots
    
    tabsetPanel(
      tabPanel("Data", fluid = T, 
               sidebarLayout(
                 sidebarPanel(
                   selectInput('fileselect', label = 'Select Dataset',
                               choices = list('','bp_ghana' = 'ghana_data', 'bp_hypnos' = 'hypnos_data','jhs_proc' = 'jhsproc_data','User Datafile' = 'input_data' )),
                   uiOutput('file_input'),
                   textInput('sys', 'Systolic'),
                   textInput('dias', 'Diastolic'),
                   checkboxInput('date1', 'Date'),
                   uiOutput('dateinput'),
                   checkboxInput('id1', 'ID'),
                   uiOutput('idinput'),
                   checkboxInput('wake1', 'Wake'),
                   uiOutput('wakeinput'),
                   checkboxInput('visit1', 'Visit'),
                   uiOutput('visitinput'),
                   checkboxInput('heart1', 'Heart Rate'),
                   uiOutput('heartinput'),
                   checkboxInput('pp1','Pulse Pressure'),
                   uiOutput('ppinput'),
                   checkboxInput('map1','Mean Arterial Pressure'),
                   uiOutput('mapinput'),
                   checkboxInput('rpp1','Rate Pulse Pressure'),
                   uiOutput('rppinput'),
                   checkboxInput('dow1','Day of the Week'),
                   uiOutput('dowinput'),
                   actionButton('submit','Submit')
                 ),
                 mainPanel(tableOutput("contents"))
                 
                 
                 
    )),
    tabPanel("Metrics", fluid = T, 
             sidebarLayout(
               sidebarPanel(selectInput('metric', 'Choose Metric', choices = c('Average Real Variability (ARV)' = 'arv'
               )),
               uiOutput("select_parameter"),
               uiOutput("help_text"),
               uiOutput("select_second_parameter"),
               uiOutput("second_parameter_helptext"),
               uiOutput("select_third_parameter"),
               uiOutput("third_parameter_helptext"),
               ),
               mainPanel(DT::dataTableOutput("metric")))
    ),
    tabPanel("Plots", fluid = T,
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 #Select Data Set from BP Ghana, BP Hypnos, or BP JHS
                 selectInput(inputId = "dataSet", label = "Data Set", choices = c("BP Hypnos" = "bp_hypnos", "BP JHS" = "bp_jhs", "BP Ghana" = "bp_ghana")),
                 
                 #If BP Ghana is selected, customize bp_scatter() plot accordingly
                 conditionalPanel(condition = "input.dataSet == 'bp_ghana'",
                                  
                                  #Subject customization
                                  selectInput(inputId = "subj_ghana", label = "Subject", choices = c("", as.character(factor(bp_ghana$ID))), selected = NULL, multiple = T),
                                  
                                  #Group_var customization, c(2,6,7,..., 19) are the columns that can be applied to group_var (levels must be less than 10)
                                  selectInput(inputId = "group_var_ghana", label = "Grouping Variable (1):", choices = c("", names(bp_ghana[,c(2,6,7,8,9,10,11,13,15,17,18,19)])), selected = NULL, multiple = T),
                                  
                                  #Wrap_var customization
                                  selectInput(inputId = "wrap_vars_ghana",label = "Wrapping Variable (1):", choices = c("", names(bp_ghana[,c(2,6,7,8,9,10,11,13,15,17,18,19)])), selected = NULL, multiple = T) 
                 ), 
                 
                 #if BP JHS is selected, customize bp_scatter() plot accordingly
                 conditionalPanel(condition = "input.dataSet == 'bp_jhs'", 
                                  
                                  #No Subject customization necessary
                                  
                                  #Group_var customization
                                  selectInput(inputId = "group_var_jhs", label = "Grouping Variable (1):", choices =c("", names(jhs_proc[,c(6,10,12,13,15,17,18,19,20,21,22,23)])), selected = NULL, multiple = T),
                                  
                                  #Wrap_var customization
                                  selectInput(inputId = "wrap_vars_jhs", label = "Wrapping Variable (1)", choices = c("", names(jhs_proc[,c(6,10,12,13,15,17,18,19,20,21,22,23)])), selected = NULL, multiple = T)),
                 
                 #if BP Hypnos is selected, customize bp_scatter() plot accordingly
                 conditionalPanel(condition = "input.dataSet == 'bp_hypnos'",
                                  
                                  #Subject customization
                                  selectInput(inputId = "subj_hypnos", label = "Subject", choices = c("", as.character(factor(bp_hypnos$ID))), selected = NULL, multiple = T),
                                  
                                  #group_var customization
                                  selectInput(inputId = "group_var_hypnos", label = "Grouping Variable (1)", choices = c("", as.character(names(hypnos_proc[,c(4,5,6,10,15,16,17,18,19,20)]))), selected = NULL, multiple = T),
                                  
                                  #wrap_var customization
                                  selectInput(inputId = "wrap_vars_hypnos", label = "Wrapping Variable (1)", choices = c("", as.character(names(hypnos_proc[,c(4,5,6,10,15,16,17,18,19,20)]))), selected = NULL, multiple = T)),
                 
                 #Select Plot Type from Stages 2020 or AHA
                 radioButtons(inputId = "plotType", label = "Plot Type", choices = c("stages2020", "AHA"), selected = "stages2020"),
                 
                 #Further customization can be made if stages 2020 plot type is chosen
                 conditionalPanel(condition = "input.plotType == 'stages2020'",
                                  checkboxInput(inputId = "inc.crisis", label = "Include Hypersensitive Crisis?", value = T), #include Crisis category
                                  checkboxInput(inputId = "inc.low", label = "Include Low Hypotension?", value = T) #include "low" category
                 ),
                 
                 
               ),
               #draw the scatter plot
               mainPanel = mainPanel(plotOutput(outputId = "bp.scatter"))
             ))
  )
)
