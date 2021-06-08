library(shiny)
library(DT)
library(bp)

ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny BP"),
  
  # Create tabset for Panel layout of Data, Metrics, and Plots
  
  tabsetPanel(
    tabPanel("Data", fluid = T, 
             "hello world"),
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
               checkboxInput("filter_sleep_wake", "Calculate metric for sleeping/waking hours?", value = FALSE, width = NULL),
               conditionalPanel(
                 condition = "input.filter_sleep_wake",
                 numericInput("sleep_start", "Sleep start time", 0, min = 0, max = 24),
                 numericInput("sleep_end", "Sleep end time", 6, min = 0, max = 24),
                 uiOutput("sleep_wake_help"),
                 selectInput("sleep_or_wake", "Calculate for sleep, wake, or both?", choices = c("Sleep" = "sleep", "Wake" = "wake", "Both" = "both"), selected = "Sleep"))
               ),
               mainPanel(DT::dataTableOutput("metric")))
    ),
    
    tabPanel("Plots", fluid = T,
             "hello world")
  )
)


