library(shiny)
library(bp)

ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny BP"),
  
  # Create tabset for Panel layout of Data, Metrics, and Plots
  
  tabsetPanel(
    tabPanel("Data", fluid = T, 
             "hello world"),
    tabPanel("Metrics", fluid = T, 
             "hello world"),
    tabPanel("Plots", fluid = T,
             "hello world")
  )
)


