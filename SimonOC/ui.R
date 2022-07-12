


# SimonOC: Simon's 2-stage design with Operating Characteristics
# by Zhen Zhang (zhangquake1@outlook.com)

require(shiny)

shinyUI(fluidPage(
  titlePanel("Simon's 2-stage with Operating Characteristics"),
  
  sidebarLayout(
    position = 'left',
    sidebarPanel(
      # img(src="logo.png", height=100, width = '50%', align='center'),
      # includeMarkdown("help.Rmd"),
      # includeHTML("help.html"),
      br(),
      fluidRow(
        hr(helpText("Design parameters:")),
        column(3, numericInput( "pu", "Unacceptable", value=0.2, step=.01)),
        column(3, numericInput( "pa", "Target", value=.4, step=.01)),
        column(3, numericInput( "e1", "Type 1", value=.05, step=.01)),
        column(3, numericInput( "pwr", "Power", value=.9, step=.05))
      ),
      fluidRow(
        sliderInput( "b1", label="X-axis", min=0, max=1, value=c(0, 0.65))
      ),
      tags$head(
        tags$style(HTML('#goButton{background-color:rgba(0,255,0,.3)}'))
      ),
      actionButton("goButton", "Update"),
      br(), br()
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        #column(4, 
        tabPanel(
          "Results",
          hr(), 
          uiOutput("plot.ui"),
          hr(),
          DT::dataTableOutput("tab", width = "80%"),
          
          # conditionalPanel( 
            # condition = "input.goButton>0",
          downloadButton('downloadData', 'Export table to Output.csv')
          # ),
        ), 
        
        tabPanel(
          "Codes",
          div(style="display: inline-block;vertical-align:top; width: 1000px;",
              includeHTML("codes.html")),
          br()
          #includeMarkdown("codes.Rmd")
        )
        
      )             
    )
    
  )
  
))

