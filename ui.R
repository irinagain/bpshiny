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
                 textInput('syst',label = "Enter column name corresponding to systolic values"),
                 textInput('diast',label = "Enter column name corresponding to diastolic values"),
                 textInput('date',label = "Enter column name corresponding to date/time"),
                 textInput('hr',label = "Enter column name corresponding to Heart Rate")
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


