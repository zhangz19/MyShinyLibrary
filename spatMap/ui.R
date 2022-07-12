

# spatMap: mapping spatial data 
# by Zhen Zhang (zhangquake1@outlook.com)

require(leaflet)

vars <- c(
  "Mean patient-reported outcome" = "mean_pro",
  "Sample size" = "sample_size"
)

navbarPage("spatMap", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      leafletOutput("map", width="100%", height="100%"),
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h2("Select variables"),
        selectInput("color", "Color", vars, selected = "mean_pro"),
        selectInput("size", "Size", vars, selected = "sample_size"),
        checkboxInput("hs", "Highlight only selected region?", FALSE),
        selectInput("method", "Type of plot", 
                    c("Histogram"), selected="Histogram"),
        plotOutput("plot1", height = 400)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('NAME of Database'), ' by Zhen Zhang (zhangquake1@outlook.com).'
      )
    )
  ),

  # tabPanel("Document",
  #          tags$iframe(
  #            style="height:660px; width:70%; scrolling=yes; frameBorder='0'",
  #            src="report.pdf#page=1&zoom=120")), 

)


# not run


