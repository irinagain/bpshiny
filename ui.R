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
                             choices = list('','bp_ghana' = 'ghana_data', 'bp_hypnos' = 'hypnos_data','bp_jhs' = 'jhsproc_data',
                                            'bp_children' = 'bpchildren_data','bp_preg' = 'bppreg_data', 'User Datafile' = 'input_data' )),
                 uiOutput('file_input'),
                 uiOutput('sys_input'),
                 uiOutput('dias_input'),
                 uiOutput('date_checkbox'),
                 uiOutput('dateinput'),
                 uiOutput('id_checkbox'),
                 uiOutput('idinput'),
                 uiOutput('wake_checkbox'),
                 uiOutput('wakeinput'),
                 uiOutput('visit_checkbox'),
                 uiOutput('visitinput'),
                 uiOutput('hr_checkbox'),
                 uiOutput('heartinput'),
                 uiOutput('pp_checkbox'),
                 uiOutput('ppinput'),
                 uiOutput('map_checkbox'),
                 uiOutput('mapinput'),
                 uiOutput('rpp_checkbox'),
                 uiOutput('rppinput'),
                 uiOutput('dow_checkbox'),
                 uiOutput('dowinput'),
                 uiOutput('dataviewer')
               ),
               mainPanel(tableOutput("contents"))
               
               
               
             )),
    tabPanel("Metrics", fluid = T, 
             sidebarLayout(
               sidebarPanel(selectInput('metric', 'Choose Metric', choices = c('Average Real Variability (ARV)' = 'arv',
                                                                               'Measures of Center' = 'bp_center', 
                                                                               'Blood Pressure Magnitude (Peak and Trough)' = 'bp_mag',
                                                                               'Blood Pressure Range' = 'bp_range', 
                                                                               'Aggregated BP Summary Statistics' = 'bp_stats', 
                                                                               'Blood Pressure Tables' = 'bp_tables', 
                                                                               'Coefficient of Variation (CV)' = 'cv',
                                                                               'Successive Variation (SV)' = 'sv', 
                                                                               'Nocturnal Blood Pressure Dipping Calculation' = 'dip_calc'
                                                                               
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
        radioButtons("plottype",  "Plot Type",
                       choices = c(`Scatter Plot` = 'bp_scatter',
                                   `Histogram` = 'bp_hist'
                                    )
                       ),
        uiOutput("subj_for_scatter_and_hist"),
        uiOutput("group_var_for_scatter"),
        uiOutput("wrap_var_for_scatter"),
        uiOutput("plot_type_for_scatter"),
        uiOutput("include_crisis_stages2020"),
        uiOutput("include_low_stages2020")),
        mainPanel = 
          mainPanel(plotOutput("plot"),
                    hr(),
                    textOutput(outputId = "plotName"))
        
      )
    )
  )
)
