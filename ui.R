library(shiny)
library(DT)
library(bp)


ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny BP"),
  
  # Create tabset for Panel layout of Data, Metrics, and Plots
  
  tabsetPanel(
    tabPanel("Data", fluid = T, 
             sidebarLayout(
               sidebarPanel(
                 selectInput('fileselect', label = 'Select Dataset',
                             choices = list('','bp_ghana' = 'ghana_data', 'bp_hypnos' = 'hypnos_data','bp_jhs' = 'jhsproc_data','User Datafile' = 'input_data' )),
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
               sidebarPanel(selectInput('metric', 'Choose Metric', choices = c('Average Real Variability (ARV)' = 'arv',
                                                                               'Measures of Center' = 'bp_center', 
                                                                               'Blood Pressure Magnitude (Peak and Trough)' = 'bp_mag',
                                                                               'Blood Pressure Range' = 'bp_range', 
                                                                               'Aggregated BP Summary Statistics' = 'bp_stats'
                                                                               
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
                 
                uiOutput("input_data_subj"),
                uiOutput("input_data_group_var"),
                uiOutput("input_data_wrap_vars"),

                 
                 #Select Plot Type from Stages 2020 or AHA
                 radioButtons(inputId = "plotType", label = "Plot Type", choices = c("stages2020", "AHA"), selected = "stages2020"),
                 
                 #Further customization can be made if stages 2020 plot type is chosen
                 conditionalPanel(condition = "input.plotType == 'stages2020'",
                                  checkboxInput(inputId = "inc.crisis", label = "Include Hypersensitive Crisis?", value = T), #include Crisis category
                                  checkboxInput(inputId = "inc.low", label = "Include Low Hypotension?", value = T) #include "low" category
                 ),
                 
                 
               ),
               #draw the scatter plot
               mainPanel = mainPanel(plotOutput(outputId = "bp.scatter"),
                                     textOutput(outputId = "plotName"))
             ))
  )
)
