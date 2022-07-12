

# spatMap: mapping spatial data
# by Zhen Zhang (zhangquake1@outlook.com)

options(warn=-1) #0

require(leaflet)
# require(RColorBrewer)
# require(scales)
require(lattice)
require(arules)

#map1: spatial information  #dat: simulated outcome  #pro: patient level data
load('demo.RData') 

# custom your colors at: https://colorbrewer2.org/
J <- length(cols <- c('#a50026','#d73027','#f46d43','#fdae61','#fee08b',
                      '#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837'))

function(input, output, session) {
  
  useData <- reactive({
    foo <- dat
    return(list(foo=foo))
  })
  
  ## Interactive Map ###########################################
  output$map <- renderLeaflet({
    m <- leaflet() %>% 
      addTiles() %>%
      # addTiles(
      #   urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      # ) %>%
      setView(lng=-88.85, lat=42.45, zoom=6) #demo states
    # setView(lng = -86.85, lat = 39.45, zoom = 6) #US
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    foo <- useData()$foo
    
    colorBy <- input$color
    colorData <- foo[[colorBy]]
    cuts <- discretize(colorData, method="frequency", breaks=length(cols), onlycuts=T)
    y1 <- discretize(colorData, method='fixed', breaks=cuts)
    pal <- colorBin(palette=cols, colorData, bins=cuts, pretty=FALSE)
    
    sizeBy <- input$size
    fac <- 3300 #can be an option from the drag-down manu to control the size
    radius <- log(foo[[sizeBy]] / max(foo[[sizeBy]]) * 30000) *fac/2
    
    m <- leafletProxy("map") # %>% clearShapes() 
    m <- m %>%
      addPolygons(data=map1, stroke=FALSE, fillOpacity=0.5, smoothFactor=0.5, 
                  color=cols[y1])
    m <- m %>%
      addCircles(data=foo, ~Longitude, ~Latitude, radius=radius, layerId=~county, 
                 stroke=FALSE, fillOpacity=0.1, fillColor=cols[y1]) %>%
      addLegend("topleft", pal=pal, opacity=1, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  
  # Show a popup at the given location
  showZipcodePopup <- function(locId, lat, lng) {
    foo <- useData()$foo
    colorBy <- input$color
    colorData <- foo[[colorBy]]
    cuts <- discretize(colorData, method="frequency", breaks=length(cols), onlycuts=T)
    y1 <- discretize(colorData, method='fixed', breaks=cuts)
    inds <- which(foo$county == locId)[1]
    if(length(inds)>0 && !is.na(inds)){
      selectedLoc <- foo[inds,]
      content <- as.character(tagList(
        tags$h4(paste0("Mean PRO: ", round(selectedLoc$mean_pro, 4))),
        tags$strong(HTML(
          sprintf("County: %s", selectedLoc$county)
        )), tags$br(),
        sprintf("Longitude: %s,  Latitude: %s",
                round(selectedLoc$Longitude,2), round(selectedLoc$Latitude,2)
        ), tags$br(),
        sprintf("Sample size: %s", selectedLoc$sample_size)
      ))
      m <- leafletProxy("map") 
      if(input$hs){
        #m <- m %>% clearShapes()
        coordinates(selectedLoc) <- ~ Longitude + Latitude
        proj4string(selectedLoc) <- proj4string(map1)
        id <- over(selectedLoc, map1)
        m <- m %>% addPolygons(data=map1[id,], stroke=TRUE, fillOpacity=0.5, 
                               smoothFactor=0.5, color=cols[y1[id]])
      }
      m <- m %>% addPopups(lng, lat, content, layerId=locId)
    }
  }
  
  # When map is clicked, show a popup with county info
  observe({
    leafletProxy("map") %>% clearPopups() # turn this on for web
    event <- input$map_shape_click
    if(is.null(event))
      return()
    output$plot1 <- renderPlot({
      inds <- which(dat$county == event$id)[1]
      if(length(inds)>0){
        par(las=1, mfrow=c(1, 1), mar=c(6,2,1,.5)+.2,mgp=c(1,.2,0), tck=-0.01, cex.axis=1, cex.lab=1, cex.main=1.1)
        if(input$method=="Histogram" && !is.null(pro[[inds]])){
          hist(pro[[inds]], main=dat$county[inds], xlab='Patient reported outcome')
        }
      }
    })
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
    
  })
  
}

# not run



