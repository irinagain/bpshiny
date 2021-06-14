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
                 fileInput("file1", "Choose CSV File", multiple = FALSE, accept = ".csv"),
                 checkboxGroupInput('bpcolnames',label = "Please select all column names included in the data",
                                    choices = list('Systolic' = 'syst', 'Diastolic' = 'diast',"Date/Time" = 'date',"ID" = 'id', 'Wake' = 'wake', 
                                                   'Visit' = 'visit', 'Heart Rate' = 'heart', 'Pulse Pressure' = 'pp', 'Mean Arterial Pressure' = 'map',
                                                   'Rate Pulse Pressure' = 'rpp', 'Day of the Week' = 'dow'), selected = c('syst','diast')),
                 actionButton('submit','Submit'),
                 textInput('sys','Systolic'),
                 textInput('dias','Diastolic'),
                 uiOutput('new1'),
                 uiOutput('new2'),
                 uiOutput('new3'),
                 uiOutput('new4'),
                 uiOutput('new5'),
                 uiOutput('new6'),
                 uiOutput('new7'),
                 uiOutput('new8'),
                 uiOutput('new9')
               ),
               mainPanel(tableOutput("contents"))
             ),
    ),
    tabPanel("Metrics", fluid = T, 
             sidebarLayout(
               sidebarPanel(selectInput('matric', 'Choose Matric', choices = c('Average Real Variability (ARV)' = 'arv'
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
             "hello world")
  )
)


