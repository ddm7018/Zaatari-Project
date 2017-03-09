library(dplyr)
library(hash)
library(rgdal)

asset <- readRDS("cap_data/raw_assets.rds")
block <- readRDS("cap_data/block_summary.rds")

dist <- readOGR("boundaries/template_datasets/Zaatari_reference_datasets")
dist <- spTransform(dist, CRS("+proj=longlat +datum=WGS84"))

dist$total_educated  <- 0
dist$total_residents <- 0
dist$literacy        <- 0 
dist$avg_info_source <- 0

for(i in 1:nrow(data.frame(dist))){
  result <- block[block$district == dist[i,]$District & block$block == dist[i,]$Block,]
  if (nrow(result) > 0 ){
    dist$total_educated[i]  <- result$total_educated
    dist$total_residents[i] <- result$total_residents
    dist$literacy[i]        <- result$literacy
    dist$avg_info_source[i] <- result$avg_info_source
  }
  
}

colHash <- hash()
colHash["total_educated"] <- "total_educated"
colHash["total_residents"]    <- "total_residents"
colHash["literacy"] <- "literacy"
colHash["avg_info_source"] <- "avg_info_source"