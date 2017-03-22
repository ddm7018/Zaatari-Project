library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(hash)
library(rgdal)
library(classInt)
library(tidyr)
source("update.R")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
colmat.print<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex = 1)
  }
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}

colmat<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){
  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  #plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    #points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex = 1)
    }
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}


bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=4){
  quanmean<-rasterx
  quanmean[quanmean ==0] <- NA
  temp <- data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  #print(brks)
  r1 <- within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr<-data.frame(r1[,2]) 
  quanvar<-rastery
  quanvar[quanvar ==0] <- NA
  temp <- data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  #print(brks)
  r2 <- within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr2<-data.frame(r2[,2])
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  col.matrix2<-colormatrix
  cn<-unique(col.matrix2)
  for(i in 1:length(col.matrix2)){
    ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
  cols<-numeric(length(quantr[,1]))
  for(i in 1:length(quantr[,1])){
    a<-as.numeric.factor(quantr[i,1])
    b<-as.numeric.factor(quantr2[i,1])
    cols[i]<-as.numeric(col.matrix2[b,a])}
  r<-rasterx
  r[1:length(r)]<-cols
  return(r)}


function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  
  test <- c("check","check1")
  observe({
  updateSelectInput(session = session, inputId = "func", choices = test)
  print(input$attr)
    })
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
  
  output$legend_color <- renderPlot({
    col.matrix <- colmat.print(nquantiles=4, xlab = input$x_color, ylab = input$y_color)
  })
  
  
  output$map <- renderLeaflet({
    leaflet( ) %>%
      addTiles() %>%
      setView(lng = 36.345 , lat = 32.29 , zoom = 14)
    
  })
  
  typeFunc <- function(ele){

    t <- eval(parse(text=paste0("levels(asset$",ele,")")))
    if(typeof(t) == 'NULL'){
      return("null")
    }
    else if(is.na(as.numeric(t[1]))){
      return("non-numeric")
    }
    else{
      return("numeric")
    }
  }
  
  
  output$functionbuilder <- DT::renderDataTable({
    inputText <- paste(input$func,"(",input$attr," == 'yes')",sep="")    
    print(paste0("asset$",input$attr))
    levels1 <- levels(eval(parse(text = paste0("asset$",input$attr))))
    print(levels1)
    
    attrType <- typeFunc(input$attr)
    if(attrType == "non-numeric"){
    tableText = paste0("data.table(asset) %>% gather(type, district, collector.block_number, ",input$attr,")
        group_by( asset, district, collector.block_number) %>% summarize(true_count = sum(",input$attr," == 'True'))")
    }
    else{
      tableText = paste0("data.table(asset) %>% gather(type, district, collector.block_number, ",input$attr,")
        group_by( asset, district, collector.block_number) %>% summarize(true_count = sum(",input$attr,"))")
    }
    print(tableText)
    result = tryCatch({
      eval(parse(text = tableText))
    }, error = function(e) {
      e
      
    })
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
    x_color_by   <- input$x_color
    y_color_by    <- input$y_color
    colorData <- dist[[colHash[[x_color_by]]]]
    pal       <- colorBin("Blues", colorData, 7)
    
    noData <- function(ele){
      ele[is.na(ele)] <- 0
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
    col.matrix <- colmat(nquantiles=4)
    x <- bivariate.map(dist[[colHash[[x_color_by]]]],dist[[colHash[[y_color_by]]]], colormatrix=col.matrix, nquantiles=4)
    
    leafletProxy("map", data = dist) %>%
      addPolygons(color = "#444444", weight = 0.01, smoothFactor = 1.0,
                  opacity = 1, fillOpacity = noData(colorData),
                  fillColor=unique(col.matrix)[x]
                  #highlightOptions = highlightOptions(weight = 1,
                  #                                    color = "white",
                  #                                    bringToFront = TRUE)
      )
  })
  
  # # Show a popup at the given location
  showBlockPopup <- function(block1, lat, lng) {
    
    
    lat <- round(lat,5)  
    lng <- round(lng,5)
    coords <- as.data.frame(cbind(lng, lat))
    point <- SpatialPoints(coords)
    proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    findDistrict <- dist[point,]

    content <- as.character(tagList(
      tags$h4("District",findDistrict$District,"- Block",findDistrict$Block), 
      tags$h4("Total Residents: ",findDistrict$sum_household),
      tags$h4("Total Educated Residents: ",findDistrict$literate),
      tags$h4("Literacy Rate: ",findDistrict$literacy_rate),
      tags$h4("Average Information Source Age: ",findDistrict$average_informat)
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

