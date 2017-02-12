library(gtools)

d2          <- read.csv("cap_data/d2.csv")
d8          <- read.csv("cap_data/d8.csv")
d9          <- read.csv("cap_data/d9.csv")
d2$district <- "2"
d8$district <- "8"
d9$district <- "9"
new         <- smartbind(d2,d8)
new         <- smartbind(new,d9)
saveRDS(new,"cap_data/data.rds")


unique(d2$collector.block_number)
unique(d8$collector.block_number)
unique(d9$collector.block_number)


library(XML)
doc <- xmlInternalTreeParse("cap_data/block_location.kml")
coord <-xpathApply(doc,"/kml:kml//kml:coordinates",xmlValue)
names <-xpathApply(doc,"/kml:kml//kml:name",xmlValue)
names <- names[c(6:195)]

data <- data.frame(
      district=character(),
      block=character(),
      long=numeric(),
      lat = numeric()
   )



count = 1
for (x in names){
  name <- unlist(strsplit(x," - "))
  district <- name[1]
  district <- substr(district,2,nchar(district))
  block <- name[2]
  block <- substr(block,2,nchar(block))
  
  currentcoord <- coord[count]
  coordlist    <- unlist(strsplit(currentcoord[[1]],","))
  
  long = as.numeric(coordlist[1])
  lat = as.numeric(coordlist[2])
  
  row <- data.frame(district=district, block=block, long = long, lat = lat)
  data <- rbind(data,row)
  count = count + 1
}

saveRDS(data,"cap_data/coord.rds")


library(data.table)
DT <- data.table(new)
test <- data.frame(DT[, list(sum(as.numeric(education_skills.literate)),sum(household.household_member)), by = list(district,collector.block_number)])
colnames(test)[2] <- "block"
final <- merge(x=test, y = data, keyby=list("district","block"), all = TRUE)
removeNulls <- final[!is.na(final['V1']),]

saveRDS(removeNulls,"cap_data/coord1.rds")


