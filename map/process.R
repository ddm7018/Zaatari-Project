library(gtools)
library(data.table)
library(XML)

#read in asset csv files, assign district column based on relevant district
#join the tables together
d2          <- read.csv("cap_data/d2.csv")
d8          <- read.csv("cap_data/d8.csv")
d9          <- read.csv("cap_data/d9.csv")
d2$district <- "2"
d8$district <- "8"
d9$district <- "9"
joinTable   <- smartbind(d2,d8)
joinTable   <- smartbind(joinTable,d9)

#reading and parsing the block locations from the kml files
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

#creating a datafram from the parsed block locations

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

#summing at the block level from the asset data and the combining with a coordinates for a final complete 
assetTable                 <- data.table(joinTable)
blockSumTable              <- data.frame(assetTable[, list(sum(as.numeric(education_skills.literate)),
                                                      sum(household.household_member),
                                                      sum(as.numeric(education_skills.literate))/sum(household.household_member)*100
                                                      ),
                                                      by = list(district,collector.block_number)])
colnames(blockSumTable)[2] <- "block"
colnames(blockSumTable)[3] <- "total_educated"
colnames(blockSumTable)[4] <- "total_residents"
colnames(blockSumTable)[5] <- "literacy"

mergeTable                 <- merge(x=blockSumTable, y = data, keyby=list("district","block"), all = TRUE)
cleanFinal                 <- mergeTable[!is.na(mergeTable['total_residents']),]

saveRDS(assetTable,"cap_data/raw_assets.rds")
saveRDS(cleanFinal,"cap_data/block_summary.rds")


