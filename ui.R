
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
                             choices = list('', 'User Datafile' = 'input_data', 'bp_ghana' = 'ghana_data', 'bp_hypnos' = 'hypnos_data','bp_jhs' = 'jhsproc_data',
                                            'bp_children' = 'bpchildren_data','bp_preg' = 'bppreg_data')),
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
                 uiOutput('data_screen_check'),
                 uiOutput('data_screen_arg'),
                 uiOutput('bp_type_check'),
                 uiOutput('bp_type_arg'),
                 uiOutput('inc_low_check'),
                 uiOutput('inc_low_arg'),
                 uiOutput('inc_crisis_check'),
                 uiOutput('inc_crisis_arg'),
                 uiOutput('tod_int_check'),
                 uiOutput('tod_int_arg'),
                 uiOutput('eod_check'),
                 uiOutput('eod_arg'),
                 uiOutput('agg_check'),
                 uiOutput('agg_arg'),
                 uiOutput('agg_thresh_check'),
                 uiOutput('agg_thresh_arg'),
                 uiOutput('collapse_df_check'),
                 uiOutput('collapse_df_arg'),
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
                                                                               'Nocturnal Blood Pressure Dipping Calculation' = 'dip_calc', 
                                                                               "Morning Blood Pressure Surge" = "mbps", 
                                                                               "Trough: Peak Ratio" = "tp_ratio", 
                                                                               "Smoothness Index" = "si", 
                                                                               "Weighted Standard Deviation" = "w_sd", 
                                                                               "Morningness-Eveningness Average" = "me-avg", 
                                                                               "Morningness-Eveningness Difference" = "me_diff"
                                                                               
               )),
               uiOutput("select_dip_parameter"),
               uiOutput('select_ext_parameter'),
               uiOutput("help_text"),
               # uiOutput("select_second_parameter"),
               # uiOutput("second_parameter_helptext"),
               # uiOutput("select_third_parameter"),
               # uiOutput("third_parameter_helptext"),
               ),
               mainPanel(conditionalPanel(condition = "output.one_table",
                                          DT::dataTableOutput("metric_table")),
                         conditionalPanel(condition = "output.bp_tables_tables",
                                          textOutput("text_1"), DT::dataTableOutput("metric_bp_table_1"),
                                          textOutput("text_2"), DT::dataTableOutput("metric_bp_table_2"), 
                                          textOutput("text_3"), DT::dataTableOutput("metric_bp_table_3"),
                                          textOutput("text_4"), DT::dataTableOutput("metric_bp_table_4"),
                                          textOutput("text_5"), DT::dataTableOutput("metric_bp_table_5"),
                                          textOutput("text_6"), DT::dataTableOutput("metric_bp_table_6"),
                                          textOutput("text_7"), DT::dataTableOutput("metric_bp_table_7"),
                                          textOutput("text_8"), DT::dataTableOutput("metric_bp_table_8"),
                                          textOutput("text_9"), DT::dataTableOutput("metric_bp_table_9"),
                                          textOutput("text_10"), DT::dataTableOutput("metric_bp_table_10"),
                                          textOutput("text_11"), DT::dataTableOutput("metric_bp_table_11"),
                                          textOutput("text_12"), DT::dataTableOutput("metric_bp_table_12"),
                                          textOutput("text_13"), DT::dataTableOutput("metric_bp_table_13"),
                                          textOutput("text_14"), DT::dataTableOutput("metric_bp_table_14"),
                                          textOutput("text_15"), DT::dataTableOutput("metric_bp_table_15"),
                                          textOutput("text_16"), DT::dataTableOutput("metric_bp_table_16")
                         ), 
                         conditionalPanel(condition = "output.dip_calc_tables", 
                                          DT::dataTableOutput("metric_dip_calc_1"), 
                                          DT::dataTableOutput("metric_dip_calc_2"))
               )
               
               #mainPanel(DT::dataTableOutput("metric")))
             )),
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
