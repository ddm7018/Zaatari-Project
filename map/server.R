library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(hash)
library(rgdal)


source("update.R")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  
  block <- eventReactive(input$recalc, {
    readRDS("cap_data/block_summary.rds")}, ignoreNULL = FALSE)  
  
  observeEvent(input$recalc, {
    print("recalculating ")
  })
  
  block2 <- eventReactive(input$addfunc, { update1() }, ignoreNULL = FALSE)  
  
  observeEvent(input$addfunc, {
    print("adding the new function")
    addFunction(input$attr, input$func)
  })
  
  addFunction <- function(x ,y){
    print(paste(x,y))
    #add to list
  }
  
  
  output$map <- renderLeaflet({
    leaflet( ) %>%
      addTiles() %>%
      setView(lng = 36.345 , lat = 32.29 , zoom = 14)
      
  })
  
  output$functionbuilder <- DT::renderDataTable({
    inputText <- paste(input$func,"(",input$attr,")",sep="")    
    print(inputText)
    result = tryCatch({
      blockSumTable <- data.frame(asset[, list(eval(parse(text = inputText))),
                                        by = list(district,collector.block_number)])
    }, warning = function(w) {
      "warning"
    }, error = function(e) {
      "error"
    }, finally = {
    })
    
    result1 <- data.table(result)
    df <- result1 %>% mutate()
    action <- DT::dataTableAjax(session, df)
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })

  
  output$contents <- DT::renderDataTable({

    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dataFile <- read.csv(inFile$datapath)
    update(dataFile)
    
  })
  
  output$hist <- renderPlot(
    hist(block()[[colHash[[input$hist_input]]]], 
         main = "Histogram",
         xlab = input$hist_input
         ))
  
  output$scatter <- renderPlot(
    xyplot(eval(parse(text=colHash[[input$x_input]])) ~eval(parse(text=colHash[[input$y_input]])), 
           block(),
           xlab = input$x_input,
           ylab = input$y_input)
        )

  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy   <- input$color
    sizeBy    <- input$size
    print(sizeBy)
    print(colorBy)
    colorData <- dist[[colHash[[colorBy]]]]
    pal       <- colorBin("Blues", colorData, 7)
    
    noData <- function(ele){
      l = c()
      for(i in 1:length(ele)){
        if(ele[i] > 0){
          l = c(l,1)
        }
        else{
          l = c(l,0)
        }
      }
        return(l)
      }  

  #   leafletProxy("map", data = block()) %>%
  #     clearShapes() %>%
  #     addCircles(~long, ~lat, radius = radius,
  #                stroke=FALSE, fillOpacity=0.8, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #               layerId="colorLegend")
  # 
    leafletProxy("map", data = dist) %>%
      addPolygons(color = "#444444", weight = dist[[colHash[[sizeBy]]]] * .1, smoothFactor = 1.0,
                  opacity = 1, fillOpacity = noData(colorData),
                  fillColor=pal(colorData)
                 #highlightOptions = highlightOptions(color = "white",
                #                                       bringToFront = TRUE)
                 )
  })
    
  # # Show a popup at the given location
  showBlockPopup <- function(block1, lat, lng) {

    
     lat <- round(lat,5)  
     lng <- round(lng,5)
     print(lat)
     print(lng)
     for (i in 1:nrow(data.frame(dist))){
       if (lng > dist[i,]@bbox[1] & lng < dist[i,]@bbox[3] & lat > dist[i,]@bbox[2] & lat < dist[i,]@bbox[4]){
         findDistrict <- dist[i,]
       }
     }
     #print(findDistrict)
     content <- as.character(tagList(
     tags$h4("District",findDistrict$District,"- Block",findDistrict$Block), 
     tags$h4("Total Residents: ",findDistrict$sum_household)
     #tags$h4("Total Educated Residents: ",findDistrict$total_educated),
     #tags$h4("Literacy Rate: ",round(findDistrict$literacy,2)),
     #tags$h4("Average Information Source Age: ",findDistrict$avg_info_source)
     
     
     ))
     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = dist)
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
     df <- block() %>% mutate()
     action <- DT::dataTableAjax(session, df)
     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
   })
  output$assetTable <- DT::renderDataTable({
    df <- asset %>% mutate()
    action <- DT::dataTableAjax(session, df)
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
   
   
   
}
