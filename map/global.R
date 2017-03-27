library(dplyr)
library(hash)
library(rgdal)

#tableTextGlobal <- "old"
latestDim <- ""
asset <- readRDS("cap_data/joinTable.rds")
block <- readRDS("cap_data/block_summary.rds")
dist  <- readOGR("boundaries/template_datasets/Zaatari_reference_datasets")
dist  <- spTransform(dist, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

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

#asset[asset$education_skills.literate == "other"] = NA

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
