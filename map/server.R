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


# Function `typeFunc` is used to determine and return the type of the input element.
# It takes a variable `ele` and evaluates it in the context of asset levels.
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

# The function colmat.print generates a color matrix for use in creating plots.
# It takes the following parameters:
# - nquantiles: the number of quantiles to use when classifying the data.
# - upperleft: the color to use for the upper left portion of the plot.
# - upperright: the color to use for the upper right portion of the plot.
colmat.print <- function(numQuantiles = 10, upperLeftColor = rgb(0,150,235, maxColorValue = 255), upperRightColor = rgb(130,0,80, maxColorValue = 255), bottomLeftColor="grey", bottomRightColor = rgb(255,230,15, maxColorValue = 255), xLabel = "x label", yLabel = "y label"){
  dataSeq    <- seq(0,1,.01)
  classInterval   <- classIntervals(dataSeq,n=numQuantiles,style="quantile")
  palette1   <- findColours(classInterval,c(upperLeftColor,bottomLeftColor))
  palette2   <- findColours(classInterval,c(upperRightColor, bottomRightColor))
  colorMatrix <- matrix(nrow = 101, ncol = 101, NA)
  #print(palette1[i])
  for(i in 1:101){
    currentColor <- c(paste(palette1[i]),paste(palette2[i]))
    colorMatrix[102-i,] <- findColours(classInterval,currentColor)
  }
  plot(c(1,1),pch = 19,col = palette1, cex = 0.5,xlim = c(0,1),ylim = c(0,1),frame.plot = F, xlab = xLabel, ylab = yLabel,cex.lab = 1.3)
  for(i in 1:101){
    tempColor <- colorMatrix[i-1,]
    points(dataSeq,rep((i-1)/100,101),pch=15,col = tempColor, cex = 1)
  }
  sequences       <- seq(0,100,(100/numQuantiles))
  sequences[1]    <- 1
  colorMatrix <- colorMatrix[c(sequences), c(sequences)]
}

# The colmat function generates a color matrix for use in creating plots.
# It takes the following parameters:
# - nquantiles: the number of quantiles to use when classifying the data.
# - upperleft: the color to use for the upper left portion of the plot.
# - upperright: the color to use for the upper right portion of the plot.
# - bottomleft: the color to use for the bottom left portion of the plot.
# - bottomright: the color to use for the bottom right portion of the plot.
# - xlab: label for the x-axis
# - ylab: label for the y-axis
colmat <- function(numQuantiles = 10,
                                upperLeftColor = rgb(0,150,235, maxColorValue = 255),
                                upperRightColor = rgb(130,0,80, maxColorValue = 255), 
                                bottomLeftColor="grey", 
                                bottomRightColor = rgb(255,230,15, maxColorValue = 255), 
                                xLabel = "x label", 
                                yLabel = "y label"){
  
  dataSequence <- seq(0, 1, .01)
  classInterval <- classIntervals(dataSequence, n=numQuantiles, style="quantile")
  palette1 <- findColours(classInterval, c(upperLeftColor, bottomLeftColor))
  palette2 <- findColours(classInterval, c(upperRightColor, bottomRightColor))
  colorMatrix <- matrix(nrow = 101, ncol = 101, NA)
  
  for(i in 1:101){
    currentColor <- c(paste(palette1[i]), paste(palette2[i]))
    colorMatrix[102-i,] <- findColours(classInterval, currentColor)
  }
  
  sequences <- seq(0, 100, (100/numQuantiles))
  sequences[1] <- 1
  colorMatrix <- colorMatrix[c(sequences), c(sequences)]
  
  return(colorMatrix)
}
# The 'bivariate.map' function is used to map two rasters using a color matrix.
# The function takes five parameters:
# - rasterx: The first raster to be mapped.
# - rastery: The second raster to be mapped.
# - colormatrix: The color matrix to use for mapping the rasters. Default is 'col.matrix'.
# - nquantiles: The number of quantiles to use when classifying the data. Default is 4.
# - data: The data to be used for the mapping.
# Function to handle missing values in the input data


# Function to calculate quantiles and assign labels

# Main bivariate.map function
# bivariate.map2 <- function(rasterx, rastery, colormatrix = col.matrix, nquantiles = 4, data) {
#   print("beginning of bivariate.map")
#   # Handle missing values in the input data
#   rasterx <- handle_missing_values(rasterx, data)
#   rastery <- handle_missing_values(rastery, data)

#   print("after handling missing values")
#   # Calculate quantiles and assign labels for rasterx
#   quantr1 <- calculate_quantiles(rasterx, nquantiles)
  
#   # Calculate quantiles and assign labels for rastery
#   quantr2 <- calculate_quantiles(rastery, nquantiles)

#   print("after calculating quantiles")
#   # Map colors based on quantiles
#   cols <- map_colors(quantr1, quantr2, colormatrix)

#   # Update rasterx with mapped colors
#   rasterx[1:length(rasterx)] <- cols
  
#   return(rasterx)
# }


handle_missing_values <- function(x, data) {
  for (ele in 1:length(data$sum_household)) {
    if (data$sum_household[ele] == 0 & x[ele] == 0) {
      x[ele] <- NA
    }
  }
  return(x)
}

# Helper function to calculate quantiles
calculate_quantiles <- function(quanmean, nquantiles) {
  temp <- data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks <- with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  #print(brks)
  r1 <- tryCatch({
    within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))},
    error = function(e) {
      within(temp, quantile <- cut(quanmean, breaks = 4, labels = 2:length(brks),include.lowest = TRUE))
    })
    
  quantr<-data.frame(r1[,2])
  return(quantr)
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

map_colors <- function(quantile_values1, quantile_values2, color_matrix, raster_values) {
  col_matrix_updated <- color_matrix
  unique_colors <- unique(col_matrix_updated)
  
  # Update color matrix
  for (i in 1:length(col_matrix_updated)) {
    ifelse(is.na(col_matrix_updated[i]), col_matrix_updated[i] <- 1, col_matrix_updated[i] <- which(col_matrix_updated[i] == unique_colors)[1])
  }
  
  cols <- numeric(length(quantile_values1[, 1]))
  
  # Map colors based on quantile values
  for (i in 1:length(quantile_values1[, 1])) {
    quantile_index1 <- as.numeric.factor(quantile_values1[i, 1])
    quantile_index2 <- as.numeric.factor(quantile_values2[i, 1])
    cols[i] <- as.numeric(col_matrix_updated[quantile_index2, quantile_index1])
  }
  
  result_raster <- raster_values
  result_raster[1:length(result_raster)] <- cols
  
  return(result_raster)
}

bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=4, data){

  quanmean <- handle_missing_values(rasterx, data)
  quanvar  <- handle_missing_values(rastery, data)
  
  # Calculate quantiles and assign labels
  quantr  <- calculate_quantiles(quanmean, nquantiles)  
  quantr2 <- calculate_quantiles(quanvar, nquantiles)
  
  
  result <- map_colors(quantr, quantr2, colormatrix, rasterx)
  return(result)
  
  }




# Main bivariate.map function


function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map

  block <- eventReactive(input$addDim, {
    #print(input$store)
    #print("inside block")
    #print(latestDim)
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
    
    #print(storeNames)
    sumData(joinTable,extraList = extraList)
    }, ignoreNULL = FALSE)

  # The observeEvent function is used to monitor the 'addDim' input. 
  # When 'addDim' input changes, this function is invoked.
  # The function checks if 'dim' input is not in the 'store' names, checks if length of 'store' names is less than 10,
  # and also checks if 'dim' input does not have any non-word characters.
  # If all conditions are met, the function updates the store with new value, updates 'latestDim' global variable, 
  # sends a custom message to client-side, and invokes 'block' function.
  observeEvent(input$addDim, {
    if(!input$dim %in% names(input$store) & length(names(input$store)) < 10 & regexpr("\\W+",input$dim ) == -1){
      tableTextGlobal<- strsplit(tableTextGlobal, "summarize")
      extracted <- tableTextGlobal[[1]][2]
      extracted <- substr(extracted,15,nchar(extracted)-1)
      updateStore(session, input$dim, extracted)
      assign("latestDim",paste(input$dim, extracted, sep ="---") , envir = .GlobalEnv)
      session$sendCustomMessage("type", "message")
      block()
    }
    #
    })
    
    
  output$testOutput <- renderText({ 
    table <- read.csv("asset-map.csv")
    as.character(table[table$X == input$attr,]["X.1"]$X.1)
  })

  # The observe function monitors reactive expressions and executes the given code block
  # whenever those expressions change. This function is useful for causing side effects,
  # such as generating plots or tables based on user inputs, or saving user inputs to disk.
  # The observe function can also be used to create a dependency chain among reactive
  # expressions, where a change in one expression triggers a change in another.
    
  observe({
      selectedAttribute <- input$attr
      #print(selectedAttribute)
      splitAttributeList <- strsplit(selectedAttribute, "[.]")[[1]]
      modifiedValue <- paste0(splitAttributeList[length(splitAttributeList)],"_" ,input$func)
      modifiedValue <- stri_replace_all(modifiedValue,"_",fixed = ".")
      modifiedValue <- stri_replace_all(modifiedValue,"_",fixed = " ")
      updateTextInput(session = session, inputId = "dim", value = modifiedValue )
      
      if(selectedAttribute %in% modifiedName){
        attributeChoices <- eval(parse(text = paste0("levels(asset$",selectedAttribute,")")))
      }
  })

  output$legend_color <- renderPlot({
    col.matrix <- colmat.print(numQuantiles=4, xLabel = input$x_color, yLabel = input$y_color)
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
    #print(input$attr)
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
    
    col.matrix <- colmat(numQuantiles=4)
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
    
    #print(findDistrict)
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