library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(hash)
#library(rgdal)
library(classInt)
library(tidyr)
library(shinyStore)
library(stringi)
library(sp)
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
  print(my.pal.1[i])
  for(i in 1:101){
    my.col <- c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,] <- findColours(my.class,my.col)
    }
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
    }
    
    print(storeNames)
    sumData(joinTable,extraList = extraList)
    }, ignoreNULL = FALSE)


  observeEvent(input$addDim, {
    if(!input$dim %in% names(input$store) & length(names(input$store)) < 10 & regexpr("\\W+",input$dim ) == -1){
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
  output$testOutput <- renderText({ 
    table <- read.csv("asset-map.csv")
    as.character(table[table$X == input$attr,]["X.1"]$X.1)
  })  
  
  observe({
    print(input$attr)
    splitList <- strsplit(input$attr, "[.]")[[1]]
    val2 <- paste0(splitList[length(splitList)],"_" ,input$func)
    val2 <- stri_replace_all(val2,"_",fixed = ".")
    val2 <- stri_replace_all(val2,"_",fixed = " ")
    updateTextInput(session = session, inputId = "dim", value = val2 )
    
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
    if(input$func == ""){
      updateSelectInput(session = session, inputId = "func", choices = choices)
      }
    else{
    updateSelectInput(session = session, inputId = "func", choices = choices, selected = input$func)
    }
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
        group_by( asset, district, collector.block_number) %>% summarize(true_count = ",input$func,"(as.numeric(as.character(",input$attr,")),na.rm = TRUE))")
    }
    assign("tableTextGlobal", tableText, envir = .GlobalEnv)
    result = tryCatch({
      df <- eval(parse(text = tableText))
    }, error = function(e) {
      e
    })
    df <- df %>% mutate(true_count = round(true_count,3))
    df
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
    xyplot(eval(parse(text=colHash[[input$y_input]])) ~eval(parse(text=colHash[[input$x_input]])), 
           block(),
           xlab = input$x_input,
           ylab = input$y_input,
           main="Scatter Plot",
           type = c("p","r"))
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
         result1 <- tryCatch({dist[[as.character(j)]][i] <- result[[as.character(j)]]},
                  error = function(e){
                    print(e)
                  })
           
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
    result <- tryCatch({
          x <- bivariate.map(dist[[colHash[[x_color_by]]]],dist[[colHash[[y_color_by]]]], colormatrix=col.matrix, nquantiles=4, data = dist)},
      error = function(e) { })
   
    z <- unique(col.matrix)[x]
    z[is.na(z)] <- 0
    z[startsWith(z,"#")] <- .75

    #print(z)
    #print(unique(col.matrix)[x])
    
    leafletProxy("map", data = st_zm(dist)) %>% clearShapes() %>%
      addPolygons(color = "#444444", weight = 0.01, smoothFactor = 1.0,
                  opacity = .75, fillOpacity = z,
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
    point <- st_as_sf(point, coords = c('y', 'x'), crs = st_crs(map))
    findDistrictinDist <- dist[point,]
    
  
    findDistrict <- block()[block()$block == findDistrictinDist$Block &  block()$district == findDistrictinDist$District,]
    
    print(findDistrict)
    popupTagList <- tagList(
      tags$h4("District",findDistrict$district,"- Block",findDistrict$block), 
      tags$h4("Total Residents: ",findDistrict$sum_household),
      tags$h4("Employment Rate: ",format(round(findDistrict$employment_rate,2),nsmall = 2)),
      tags$h4("Literacy Rate: ",format(round(findDistrict$literacy_rate,2),nsmall = 2)),
      tags$h4("Average Age of Reported Information Source: ",format(round(findDistrict$average_informat,2),nsmall = 2))
    )
    
    for(ele in names(input$store)){ 
      popupTagList <- tagAppendChild(popupTagList, tags$h4(ele,round(eval(parse(text=paste0("findDistrict$",ele))),2)))
      }
    
    content <- as.character(popupTagList)
    if(nrow(findDistrict) > 0){
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = dist)
    }
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
  
  output$downloadRawData <- downloadHandler(
    filename = function() { paste("rawData", '.csv', sep='') },
    content = function(file) {
      write.csv(asset, file)
    }
  )
  
  output$downloadSumData <- downloadHandler(
    filename = function() { paste("sumData", '.csv', sep='') },
    content = function(file) {
      write.csv(block(), file)
    }
  )
  
  
  ## Data Explorer ###########################################
  output$sumTable <- DT::renderDataTable({
    joinTable <- readRDS("cap_data/joinTable.rds")
    extraList = c()
    storeNames <- names(input$store)
    for(ele in storeNames){
      newVal <- paste0(ele,"---",eval(parse(text=paste0("input$store$",ele))))
      extraList = c(extraList, newVal)
    }
    temp <- sumData(joinTable,extraList = extraList)
    
    df <- temp %>% mutate(average_informat = round(average_informat,2),literacy_rate = round(literacy_rate,2), long = round(long,4), lat = round(lat,4))
    df <- df[,colSums(is.na(df))<nrow(df)]
    round_df <- function(df, digits) {
      nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
      
      df[,nums] <- round(df[,nums], digits = digits)
      
      (df)
    }
    df <- round_df(df, digits=3)
    df <- df[,colSums(is.na(df))<nrow(df)]
    val <- strsplit(latestDim,"---")[[1]][1]
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  output$assetTable <- DT::renderDataTable({

    df <- asset %>% mutate()
    df[, "start"] <- NULL 
    df[, "end"] <- NULL 
    df[, "today"] <- NULL
    df[,"meta.instanceID"] <- NULL
    df[,"X_uuid"] <- NULL
    df[,"X_submission_time"] <- NULL
    action <- DT::dataTableAjax(session, df)
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}

