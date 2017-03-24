library(gtools)
library(data.table)
library(XML)


#creating a datafram from the parsed block locations
getDisData <- function(){
  doc   <- xmlInternalTreeParse("cap_data/block_location.kml")
  coord <- xpathApply(doc,"/kml:kml//kml:coordinates",xmlValue)
  names <- xpathApply(doc,"/kml:kml//kml:name",xmlValue)
  names <- names[c(6:195)]
  
  data <- data.frame(
    district=character(),
    block=character(),
    long=numeric(),
    lat = numeric()
  )
  
  count = 1
  for (x in names){
    name     <- unlist(strsplit(x," - "))
    district <- name[1]
    district <- substr(district,2,nchar(district))
    block    <- name[2]
    block    <- substr(block,2,nchar(block))
    
    currentcoord <- coord[count]
    coordlist    <- unlist(strsplit(currentcoord[[1]],","))
    long         <- as.numeric(coordlist[1])
    lat          <- as.numeric(coordlist[2])
    row          <- data.frame(district=district, block=block, long = long, lat = lat)
    data         <- rbind(data,row)
    count        <- count + 1
  }
  return(data)
}




sumData <- function(joinTable,extraList = NULL){
  #summing at the block level from the asset data and the combining with a coordinates for a final complete 
  assetTable                 <- data.table(joinTable)
  l<-c()
  dimFile <- read.table("dim.txt")
  for(ele in extraList){
    eleSplit <- strsplit(ele, "---")
    newFrame <- data.frame(V1 = eleSplit[[1]][2] ,V2 = eleSplit[[1]][1])
    dimFile <- rbind(dimFile,newFrame)
  }
  
  for(i in 1:nrow(dimFile['V1'])) {
    b <- as.character(dimFile['V1'][i,])
    l<-c(l,b)
    i=i+1
  }
  blockSumTable              <- data.frame(assetTable[, list(
                                                             eval(parse(text =l[1])),
                                                             eval(parse(text =l[2])),
                                                             eval(parse(text =l[3])),
                                                             eval(parse(text =l[4])),
                                                             eval(parse(text =l[5])),
                                                             eval(parse(text =l[6])),
                                                             eval(parse(text =l[7])),
                                                             eval(parse(text =l[8])),
                                                             eval(parse(text =l[9])),
                                                             eval(parse(text =l[10]))),
                                                      by = list(district,collector.block_number)])

  colnames(blockSumTable)[2] <- "block"
  
  for(i in 1:nrow(dimFile)){
    colnames(blockSumTable)[i + 2] <- as.character(dimFile['V2'][i,])
  }
  print(blockSumTable)
  
  mergeTable                 <- merge(x=blockSumTable, y = getDisData(), keyby=list("district","block"), all = TRUE)
  cleanFinal                 <- mergeTable[!is.na(mergeTable['sum_household']),]
  
  #saveRDS(assetTable,"cap_data/raw_assets.rds")
  return(cleanFinal)
}

