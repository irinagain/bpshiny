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
               ),
               mainPanel(DT::dataTableOutput("metric")))
    ),
    
  tabPanel("Plots", fluid = T,
             sidebarLayout(
               sidebarPanel = sidebarPanel(
                 #Select Data Set from BP Ghana, BP Hypnos, or BP JHS
                 selectInput(inputId = "dataSet", label = "Data Set", choices = c("BP Ghana" = "bp_ghana", "BP Hypnos" = "bp_hypnos", "BP JHS" = "bp_jhs")),
                 
                 #If BP Ghana is selected, customize bp_scatter() plot accordingly
                 conditionalPanel(condition = "input.dataSet == 'bp_ghana'",
                                  
                                  #Group_var customization, c(2,6,7,..., 19) are the columns that can be applied to group_var (levels must be less than 10)
                                  selectInput(inputId = "group_var_ghana", label = "Group Var", choices = names(bp_ghana[,c(2,6,7,8,9,10,11,13,15,17,18,19)]), ),
                                  
                                  #Wrap_var customization
                                  selectInput(inputId = "wrap_vars_ghana",label = "Wrap Var", choices = names(bp_ghana[,c(1:21)]), multiple = T) #It doesn't seem to do anything
                                  ), 
                 
                 #if BP JHS is selected, customize bp_scatter() plot accordingly
                 conditionalPanel(condition = "input.dataSet == 'bp_jhs'", 
                                  
                                  #Group_var customization
                                  selectInput(inputId = "group_var_jhs", label = "Group Var", choices = names(jhs_proc[,c(6,10,12,13,15,17,18,19,20,21,22,23)]))),
                 
                 #if BP Hypnos is selected, customize bp_scatter() plot accordingly
                 conditionalPanel(condition = "input.dataSet == 'bp_hypnos'",
                                  
                                  #group_var customization
                                  selectInput(inputId = "group_var_hypnos", label = "Group Var", choices = names(hypnos_proc[,c(4,5,6,10,15,16,17,18,19,20)]))),
                 
                 #Select Plot Type from Stages 2020 or AHA
                 radioButtons(inputId = "plotType", label = "Plot Type", choices = c("AHA", "stages2020"), selected = "AHA"),
                 
                 #Further customization can be made if stages 2020 plot type is chosen
                 conditionalPanel(condition = "input.plotType == 'stages2020'",
                  checkboxInput(inputId = "inc.crisis", label = "Include Hypersensitive Crisis?", value = T), #include Crisis category
                  checkboxInput(inputId = "inc.low", label = "Include Low Hypotension?", value = T) #include "low" category
                 )
                 
                 
                 ),
               #draw the scatter plot
               mainPanel = mainPanel(plotOutput(outputId = "bp.scatter"))
             ))
  )
)


