library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(hash)
library(rgdal)
library(classInt)
library(tidyr)
library(shinyStore)
source("core.R")

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)



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

colmat.print <- function(nquantiles = 10, upperleft = rgb(0,150,235, maxColorValue = 255), upperright = rgb(130,0,80, maxColorValue = 255), bottomleft="grey", bottomright = rgb(255,230,15, maxColorValue = 255), xlab = "x label", ylab = "y label"){
  my.data    <- seq(0,1,.01)
  my.class   <- classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1   <- findColours(my.class,c(upperleft,bottomleft))
  my.pal.2   <- findColours(my.class,c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col <- c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,] <- findColours(my.class,my.col)}
  plot(c(1,1),pch = 19,col = my.pal.1, cex = 0.5,xlim = c(0,1),ylim = c(0,1),frame.plot = F, xlab = xlab, ylab = ylab,cex.lab = 1.3)
  for(i in 1:101){
    col.temp <- col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col = col.temp, cex = 1)
  }
  seqs       <- seq(0,100,(100/nquantiles))
  seqs[1]    <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}

colmat <- function(nquantiles = 10, upperleft = rgb(0,150,235, maxColorValue = 255), upperright = rgb(130,0,80, maxColorValue = 255), bottomleft="grey", bottomright = rgb(255,230,15, maxColorValue = 255), xlab = "x label", ylab = "y label"){
  my.data    <- seq(0,1,.01)
  my.class   <- classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1   <- findColours(my.class,c(upperleft,bottomleft))
  my.pal.2   <- findColours(my.class,c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col <- c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,] <- findColours(my.class,my.col)}
  #plot(c(1,1),pch = 19,col = my.pal.1, cex = 0.5,xlim = c(0,1),ylim = c(0,1),frame.plot = F, xlab = xlab, ylab = ylab,cex.lab = 1.3)
  for(i in 1:101){
    col.temp <- col.matrix[i-1,]
    #points(my.data,rep((i-1)/100,101),pch=15,col = col.temp, cex = 1)
  }
  seqs       <- seq(0,100,(100/nquantiles))
  seqs[1]    <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
}

bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=4, data){
  
  quanmean<-rasterx
  for(ele in 1:length(data$sum_household)){
    if(data$sum_household[ele] == 0 & quanmean[ele] == 0){
      quanmean[ele] <- NA
    }
  }
  
  
  temp <- data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  print(brks)
  r1 <- tryCatch({
    within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))},
    error = function(e) {
      within(temp, quantile <- cut(quanmean, breaks = 4, labels = 2:length(brks),include.lowest = TRUE))
    })
    
  quantr<-data.frame(r1[,2]) 
  quanvar<-rastery
  for(ele in 1:length(data$sum_household)){
    if(data$sum_household[ele] == 0 & quanvar[ele] == 0){
      quanvar[ele] <- NA
    }
  }
  temp <- data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  brks1 <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  print(brks1)
  r2 <- tryCatch({
    within(temp, quantile <- cut(quanvar, breaks = brks1, labels = 2:length(brks),include.lowest = TRUE))},
    error = function(e) {
      within(temp, quantile <- cut(quanvar, breaks = 4, labels = 2:length(brks),include.lowest = TRUE))
    })
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

  block <- eventReactive(input$addDim, {
    print(input$store)
    print("inside block")
    print(latestDim)
    joinTable <- readRDS("cap_data/joinTable.rds")
    extraList = c()
    if(latestDim != ""){
      extraList = c(latestDim)
    }
  
    storeNames <- names(input$store)
    for(ele in storeNames){
      newVal <- paste0(ele,"---",eval(parse(text=paste0("input$store$",ele))))
      extraList = c(extraList, newVal)
      print(newVal)
    }
    sumData(joinTable,extraList = extraList)
    }, ignoreNULL = FALSE)


  observeEvent(input$addDim, {
    if(!input$dim %in% names(input$store) & length(names(input$store)) < 10){
      print("adding the new function")
      print(tableTextGlobal)
      tableTextGlobal<- strsplit(tableTextGlobal, "summarize")
      extracted <- tableTextGlobal[[1]][2]
      print(extracted)
      extracted <- substr(extracted,15,nchar(extracted)-1)
      updateStore(session, input$dim, extracted)
      assign("latestDim",paste(input$dim, extracted, sep ="---") , envir = .GlobalEnv)
      session$sendCustomMessage("type", "message")
      block()
    }
    #
    })
    
    #cat(extracted,input$dim,"\n",file="dim.txt",append=TRUE)
    #session$sendCustomMessage(type = 'testmessage', message = list(dimNAme = input$dim, dimVal = extracted))

    # vars[input$dim] <- input$dim
    # colHash[input$dim] <- input$dim
    # updateSelectInput(session = session,inputId = "x_color",choices = vars)
    # updateSelectInput(session = session,inputId = "y_color",choices = vars)
    
  
  observe({
    print(input$attr)
    if(input$attr %in% modifiedName){
    choices <- eval(parse(text = paste0("levels(asset$",input$attr,")")))
    if(typeFunc(input$attr) == "non-numeric"){
      for(ele in choices){
      choices <- c(choices,paste0("Percentage ",ele))
      }
    }
    else{
      choices <- c("sum", "mean","sd")
    }
    updateSelectInput(session = session, inputId = "func", choices = choices)
    }
    })
  
  
  output$legend_color <- renderPlot({
    col.matrix <- colmat.print(nquantiles=4, xlab = input$x_color, ylab = input$y_color)
  })
  
  output$editor <- DT::renderDataTable({
    data.table(val1 = names(input$store),val2 = input$store)
  })
  
  
  output$map <- renderLeaflet({
    leaflet( ) %>%
      addTiles() %>%
      setView(lng = 36.345 , lat = 32.29 , zoom = 14)
    
  })
  
  output$dimbuilder <- DT::renderDataTable({
    if(input$attr %in% modifiedName){
    print(input$attr)
    inputText <- paste(input$func,"(",input$attr," == 'yes')",sep="")    
    levels1 <- levels(eval(parse(text = paste0("asset$",input$attr))))
    attrType <- typeFunc(input$attr)
    if(attrType == "non-numeric"){
      if(startsWith(input$func,"Percentage") == TRUE){
        newVal <- substr(input$func,12,nchar(input$func))
        tableText = paste0("data.table(asset) %>% gather(type, district, collector.block_number, ",input$attr,")
        group_by( asset, district, collector.block_number) %>% summarize(true_count = sum(",input$attr,"=='",newVal,"')/length(",input$attr,"))")
    }
      else{
        tableText = paste0("data.table(asset) %>% gather(type, district, collector.block_number, ",input$attr,")
        group_by( asset, district, collector.block_number) %>% summarize(true_count = sum(",input$attr,"=='",input$func,"'))")
      }
    }
    else{
      tableText = paste0("data.table(asset) %>% gather(type, district, collector.block_number, ",input$attr,")
        group_by( asset, district, collector.block_number) %>% summarize(true_count = ",input$func,"(as.numeric(",input$attr,")))")
    }
    assign("tableTextGlobal", tableText, envir = .GlobalEnv)
    result = tryCatch({
      eval(parse(text = tableText))
    }, error = function(e) {
      e
    })
    
  }})
  
  output$contents <- DT::renderDataTable({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dataFile <- read.csv(inFile$datapath)
    update(dataFile)
    
  })
  
  output$hist <- renderPlot({
    hist(block()[[colHash[[input$hist_input]]]], 
         main = "Histogram",
         xlab = input$hist_input
    )})
  
  output$scatter <- renderPlot({
    xyplot(eval(parse(text=colHash[[input$x_input]])) ~eval(parse(text=colHash[[input$y_input]])), 
           block(),
           xlab = input$x_input,
           ylab = input$y_input)
  }
  )
  

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    for(ele in names(input$store)){
      vars[ele] = ele
      colHash[ele] = ele
    }
    updateSelectInput(session = session,inputId = "x_color",choices = vars, selected = input$x_color)
    updateSelectInput(session = session,inputId = "y_color",choices = vars, selected = input$y_color)
    updateSelectInput(session = session,inputId = "hist_input",choices = vars, selected = input$hist_input)
    updateSelectInput(session = session,inputId = "x_input",choices = vars, selected = input$x_input)
    updateSelectInput(session = session,inputId = "y_input",choices = vars, selected = input$y_input)
    
    x_color_by   <- input$x_color
    y_color_by    <- input$y_color
    
    for(attr in vars) {
      dist[[as.character(attr)]] <- 0
    }
    for(i in 1:nrow(data.frame(dist))){
      result <- block()[block()$district == dist[i,]$District & block()$block == dist[i,]$Block,]
      if (nrow(result) > 0 ){
        for(j in vars) {
          dist[[as.character(j)]][i] <- result[[as.character(j)]]
        }
      }
    }
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
    
    col.matrix <- colmat(nquantiles=4)
    x <- bivariate.map(dist[[colHash[[x_color_by]]]],dist[[colHash[[y_color_by]]]], colormatrix=col.matrix, nquantiles=4, data = dist)
    
    for(i in 1:length(x)){ 
        print(i)
        print(x[i])
        print(dist$Dim1[i])
        print(dist$Block[i])
        print(dist$District[i])
        cat("\n")
        }
    #browser()
    z <- unique(col.matrix)[x]
    z[is.na(z)] <- 0
    z[startsWith(z,"#")] <- 1
    
    leafletProxy("map", data = dist) %>%
      addPolygons(color = "#444444", weight = 0.01, smoothFactor = 1.0,
                  opacity = 1, fillOpacity = z,
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
    findDistrictinDist <- dist[point,]
    findDistrict <- block()[block()$block == findDistrictinDist$Block &  block()$district == findDistrictinDist$District,]
    
    popupTagList <- tagList(
      tags$h4("District",findDistrict$district,"- Block",findDistrict$block), 
      tags$h4("Total Residents: ",findDistrict$sum_household),
      tags$h4("Total Educated Residents: ",findDistrict$literate),
      tags$h4("Literacy Rate: ",findDistrict$literacy_rate),
      tags$h4("Average Information Source Age: ",findDistrict$average_informat)
    )
    
    for(ele in names(input$store)){ 
      popupTagList <- tagAppendChild(popupTagList, tags$h4(ele,eval(parse(text=paste0("findDistrict$",ele)))))
      }
    
    content <- as.character(popupTagList)
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

