

require(shiny)
if(!require("shinydashboard")) {install.packages("shinydashboard")} else {library(shinydashboard)}
if (!require("shinyjs")) {install.packages("shinyjs")} else {library(shinyjs)}
if (!require("rhandsontable")) {install.packages("rhandsontable")} else {library(rhandsontable)}
if (!require("plotly")) {install.packages("plotly")} else {library(plotly)}
options(scipen=500)


shinyUI(
  dashboardPage(
    title="ISPOR course example",
    dashboardHeader(title="ISPOR course example", titleWidth=240),
    dashboardSidebar(
      collapsed=FALSE,
      # width=240,
      sidebarMenu(
        menuItem("Introduction", icon=icon("home"), tabName="home"),
        menuItem("Settings", icon=icon("th"), tabName="Model"),
        menuItem("Data Input", icon=icon("th"), tabName="Data", 
                 menuItem("Clinical Inputs", icon=icon("th"), tabName="Clinical"),
                 menuItem("Cost Inputs", icon=icon("th"), tabName="Treatment_Cost"),
                 menuItem("Quality Of Life Inputs", icon=icon("th"), tabName="Quality_of_Life"),
                 menuItem("Life-Tables", icon=icon("th"), tabName="Life_Tables")
        ),
        menuItem("Results", icon=icon("th"), tabName="Results"),
        menuItem("Sensitivity", icon=icon("th"), tabName="Sensitivity", 
                 menuItem("Heatmap", icon=icon("th"), tabName="Heatmap")
        )
      ),
      
      tags$br(),
      div(
        style = "width:100%;text-align: center;",
        actionButton(
          "goButton",
          label = "Click to Simulate results",
          icon = icon("refresh")
        )
      )
      
    ),
    
    dashboardBody(
      tags$head(tags$style(HTML('* {font-family: "Bahnschrift"};'))), #Change all font to Bahnschrift. 
      
      tabItems(
        tabItem("home",
                div(fluidRow(
                  column(
                    width = 12,
                    box(
                      title = "Microsimulation model",
                      collapsible = FALSE,
                      width = 12,
                      status = "primary",
                      fluidRow(column(
                        width = 12,
                        p("This course is designed to teach you how to replicate an Excel-based micro-simulation model using R Shiny. "),
                        p(
                          "Through hands-on sessions, you will learn to translate the functionalities and calculations of your Excel models into a dynamic and interactive R Shiny application."
                        ),
                        p(
                          "The course covers the fundamental concepts of micro-simulation modeling, the basics of R Shiny, and step-by-step guidance on how to recreate your models with a user-friendly interface. By the end of the course, you will have the skills to efficiently build, test, and deploy sophisticated simulation models in R Shiny, enhancing the accessibility and usability of your analytical tools."
                        )
                      )),
                      fluidRow(column(
                        width = 8,
                        tags$br(),
                        h4("Model structure"),
                        tags$img(src = "modelstructure.png", style = "display: block; margin: auto; width: 80%;"),
                        tags$br()
                      ))
                    )
                  )
                ))),
        
        tabItem(tabName="Model",
                fluidRow(
                  #------------------------------------------------------------------
                  box(
                    title="Settings", width=6, solidHeader=T, status="primary", 
                    h5(div("Country settings:", style="color:blue;font-weight: bold;padding-left:1px")),
                    fluidRow(
                      column(4, selectInput("country", label="Country", selected='United States', 
                                            choices=c('United States') )), 
                      column(4, selectInput("currency", label="Currency", selected='$', 
                                            choices=c('$') )), 
                      column(4, selectInput("perspective", label="Perspective", selected='Payer', 
                                            choices=c('Payer') ))
                    ),
                    
                    h5(div("Population settings:", style="color:blue;font-weight: bold;padding-left:1px")),
                    fluidRow(
                      column(4, numericInput("Cohort_size", label="Cohort size", 
                                             value=50, min=1)),
                      column(4, numericInput("cycle_length_weeks", label="Cycle length (weeks)", 
                                             value=52, min=1)),
                      column(4, numericInput("Time_horizon_years", label="Time horizon (years)", 
                                             value=74, min=1))
                    ),
                    fluidRow(
                      column(4, numericInput("Number_of_cycles_per_year", label="Number of cycles per year", 
                                             value=1, min=1)), 
                      column(4, numericInput("age_at_baseline", label="Age at baseline (years)", 
                                             value=35, min=1)), 
                      column(4, numericInput("percent_male", label="Percentage male at baseline", 
                                             value=.6, min=0, max=1))
                    ),
                    br(),
                    fluidRow(
                      column(3, textInput("Session", label="Session Name", value="run1")) #for output file
                    )
                  ) 
                  #------------------------------------------------------------------ 
                )
        ),
        
        ### Data input: 
        tabItem(tabName="Clinical",
                fluidRow(
                  useShinyjs(),
                  #------------------------------------------------------------------
                  box(title="Input", width=10, solidHeader=T, status="primary", 
                      h5(div("Clinical input.csv:", style="color:blue;font-weight:bold;padding-left:1px")),
                      rHandsontableOutput("clinical_input_csv", width="100%"), 
                      br(), br()
                  ),   #------------------------------------------------------------------
                )
        ),
        tabItem(tabName="Treatment_Cost",
                fluidRow(
                  #------------------------------------------------------------------
                  box(title="Input", width=10, solidHeader=T, status="primary", 
                      h5(div("Health State Costs:", style="color:blue;font-weight:bold;padding-left:1px")),
                      fluidRow(
                        column(3, numericInput("HSC_Active", label="Active", value=12000, min=0)),
                        column(3, numericInput("HSC_Remission", label="Remission", value=6000, min=0)),
                        column(3, numericInput("HSC_Hospitalized", label="Hospitalized", value=10000, min=0)),
                        column(3, numericInput("HSC_Death", label="Death", value=0, min=0))
                      ),
                      h5(div("Treatment Costs:", style="color:blue;font-weight:bold;padding-left:15px")),
                      fluidRow(
                        column(3, numericInput("Cost_Biomarkers", label="Biomarkers", value=56326.8, min=0)),
                        column(3, numericInput("Cost_Endoscopy", label="Endoscopy", value=309.33333, min=0))
                      ),
                      br(), br()
                  ),   #------------------------------------------------------------------
                )
        ),
        tabItem(tabName="Quality_of_Life",
                fluidRow(
                  #------------------------------------------------------------------
                  # Utilities_by_health_state <- c(Active=0.42, Remission=0.78, Hospitalized=0.32)
                  box(title="Input", width=10, solidHeader=T, status="primary", 
                      h5(div("Utilities by health state:", style="color:blue;font-weight:bold;padding-left:1px")),
                      fluidRow(
                        column(3, numericInput("QoL_Active", label="Active", value=0.42, min=0)),
                        column(3, numericInput("QoL_Remission", label="Remission", value=0.78, min=0)),
                        column(3, numericInput("QoL_Hospitalized", label="Hospitalized", value=0.32, min=0))
                      ),  
                      br(), br()
                  ),   #------------------------------------------------------------------
                )
        ),
        tabItem(tabName="Life_Tables",
                fluidRow(
                  useShinyjs(),
                  #------------------------------------------------------------------
                  box(title="Input", width=8, solidHeader=T, status="primary", 
                      h5(div("Life tables:", style="color:blue;font-weight:bold;padding-left:1px")),
                      rHandsontableOutput("life_tables_csv", width="100%"), 
                      br(), br()
                  ),   #------------------------------------------------------------------
                )
        ),
        
        
        ##### results
        tabItem(tabName="Results",
                fluidRow(
                  #------------------------------------------------------------------
                  box(title="Output", width=10, solidHeader=T, status="primary", 
                      hr(),
                      DT::dataTableOutput("tab", width="90%"),
                      conditionalPanel(condition="input.goButton>0",
                                       downloadButton('downloadData', 'Export full table (.csv)')
                      ),
                      hr(), 
                      h5(div("Convergence plot:", style="color:blue;font-weight:bold;padding-left:1px")),
                      uiOutput("convergence.ui"),
                      hr()
                  ), #------------------------------------------------------------------
                  hr()
                )
        ),
        
        ### sensitivity analysis
        tabItem(tabName="Heatmap",
                fluidRow(
                  box(title="Heatmap input", width=5, solidHeader=TRUE, status="primary", 
                      h5(div("Input:", style="color:blue;font-weight: bold;padding-left:1px")),
                      fluidRow(
                        column(5, selectInput("x1nam", label="Input 1", selected='Average_Cost_Endoscopy', 
                                              choices=c('Average_Cost_Endoscopy') )), 
                        column(3, numericInput("x1min", label="Min", value=200, min=0) ), 
                        column(3, numericInput("x1max", label="Max", value=20000, min=0) ) 
                      ),
                      fluidRow(
                        column(5, selectInput("x2nam", label="Input 2", selected='Utilities_by_health_state_Active',  choices=c('Utilities_by_health_state_Active') )), 
                        column(3, numericInput("x2min", label="Min", value=0.21, min=0, max=1) ), 
                        column(3, numericInput("x2max", label="Max", value=0.63, min=0, max=1) ) 
                      ),
                      fluidRow(
                        column(5, numericInput("Cohort_size_sen2d", label="Cohort size", value=10, min=1)),
                        column(3, numericInput("len1", label="#knots1", value=4, min=2)),
                        column(3, numericInput("len2", label="#knots2", value=5, min=2))
                      ), 
                      tags$head(
                        tags$style(HTML('#goSensitivity{background-color:rgba(255,0,0,.3)}'))
                      ),
                      actionButton("goSensitivity", "Run 2-D Sensitivity Analysis"),
                      br()
                  ),
                  box(title="Output", width=5, solidHeader=T, status="primary", 
                      h5(div("Heatmap:", style="color:blue;font-weight:bold;padding-left:1px")),
                      uiOutput("heat.ui"),
                      br()
                  )
                )
        )
        
        
        
      )
      
    )
    
    
    
  )
)



