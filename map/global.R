library(dplyr)
library(hash)
#library(rgdal)

library(sf)

#tableTextGlobal <- "old"
latestDim <- ""
asset <- readRDS("cap_data/joinTable.rds")
block <- readRDS("cap_data/block_summary.rds")

dist <- st_read("boundaries/")
#dist  <- readOGR("boundaries/template_datasets/Zaatari_reference_datasets")
dist  <- st_transform(dist, st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

modifiedName = c()
table <- read.csv("asset-map.csv")
for(ele in table$X){
  modifiedName <- c(modifiedName,ele)
}



#move reading Dim to core.R
colHash <- hash()
dimFile <- read.table("dim.txt")

for(i in 1:nrow(dimFile)){
  colHash[as.character(dimFile['V2'][i,])] <- as.character(dimFile['V2'][i,])
}

dimFile <- read.table("dim.txt")
vars = c()
for(i in 1:nrow(dimFile)){
  vars[[as.character(dimFile['V2'][i,])]] <- as.character(dimFile['V2'][i,])
}
