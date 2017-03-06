library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(hash)
source("update.R")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]
# addMarkers(lat = ~lat,lng= ~long,popup = paste(block$district,"-",block$block)) %>%

function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  
  block <- eventReactive(input$recalc, {
    readRDS("cap_data/block_summary.RDS")}, ignoreNULL = FALSE)  
  
  observeEvent(input$recalc, {
    print("recalculating ")
  })
  
  output$map <- renderLeaflet({
    leaflet( ) %>%
      addTiles() %>%
      setView(lng = 36.345 , lat = 32.29 , zoom = 14)
      
  })
  
  output$contents <- renderTable({

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    dataFile <- read.csv(inFile$datapath)
    update(dataFile)
    
  })
  
  output$hist <- renderPlot(
    hist(block()[[colHash[[input$color]]]], 
         main = "Histogram",
         xlab = input$color
         ))
  
  output$scatter <- renderPlot(
    xyplot(eval(parse(text=colHash[[input$color]])) ~eval(parse(text=colHash[[input$size]])), 
           block())
        )

  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.


  observe({
    colorBy   <- input$color
    sizeBy    <- input$size

    colorData <- block()[[colHash[[colorBy]]]]
    if (colorBy == 'avg_info_source'){
    pal       <- colorBin("Blues", colorData, 7)
    }
    else{
      pal       <- colorQuantile("Blues", colorData, 7)
    }
    radius    <- block()[[colHash[[sizeBy]]]]
    

    leafletProxy("map", data = block()) %>%
      clearShapes() %>%
      addCircles(~long, ~lat, radius = radius,
                 stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # # Show a popup at the given location
  showBlockPopup <- function(block1, lat, lng) {
     lat <- round(lat,5)  
     lng <- round(lng,5)
     findDistrict <- block[(round(block$long,5)==lng & round(block$lat,5)==lat ),]
     #print(findDistrict)
     content <- as.character(tagList(
     tags$h4("District",findDistrict$district,"- Block",findDistrict$block), 
     tags$h4("Total Residents: ",findDistrict$total_residents),
     tags$h4("Total Educated Residents: ",findDistrict$total_educated),
     tags$h4("Literacy Rate: ",round(findDistrict$literacy,2)),
     tags$h4("Average Information Source Age: ",findDistrict$avg_info_source)
     
     
     ))
     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = block)
   }

   # When map is clicked, show a popup with city info
   observe({
     leafletProxy("map") %>% clearPopups()
     event <- input$map_shape_click
     if (is.null(event))
       return()
  
     isolate({
       showBlockPopup(event$id, event$lat, event$lng)
     })
   })
  #
  
  ## Data Explorer ###########################################
  
   output$sumTable <- DT::renderDataTable({
     df <- block %>% mutate()
     action <- DT::dataTableAjax(session, df)
     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
   })
  output$assetTable <- DT::renderDataTable({
    df <- asset %>% mutate()
    action <- DT::dataTableAjax(session, df)
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
   
   
   
}
