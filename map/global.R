library(dplyr)
library(hash)
library(rgdal)

#tableTextGlobal <- "old"

asset <- readRDS("cap_data/raw_assets.rds")
block <- readRDS("cap_data/block_summary.rds")

dist <- readOGR("boundaries/template_datasets/Zaatari_reference_datasets")
dist <- spTransform(dist, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#move to core.R
dimFile <- read.table("dim.txt")
for(i in 1:nrow(dimFile)) {
  dist[[as.character(dimFile['V2'][i,])]] <- 0
}

for(i in 1:nrow(data.frame(dist))){
  result <- block[block$district == dist[i,]$District & block$block == dist[i,]$Block,]
  if (nrow(result) > 0 ){
    for(j in 1:nrow(dimFile)) {
    dist[[as.character(dimFile['V2'][j,])]][i] <- result[[as.character(dimFile['V2'][j,])]]
    }
  }
}

modifiedName <- names(asset)
modifiedName <- modifiedName[-which(modifiedName == "subscriberid")]
modifiedName <- modifiedName[-which(modifiedName == "X_tags")]
modifiedName <- modifiedName[-which(modifiedName == "X_notes")]
modifiedName <- modifiedName[-which(modifiedName == "start")]
modifiedName <- modifiedName[-which(modifiedName == "end")]
modifiedName <- modifiedName[-which(modifiedName == "today")]

modifiedName[239] <-  modifiedName[1]
modifiedName[240] <-  modifiedName[2]
modifiedName <- modifiedName[-1]
modifiedName <- modifiedName[-1]

asset[asset$education_skills.literate == "other"] = NA


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
