library(dplyr)
library(hash)

asset <- readRDS("cap_data/raw_assets.rds")
#block <- readRDS("cap_data/block_summary.rds")

dist <- readOGR("boundaries/template_datasets/Zaatari_reference_datasets")
dist <- spTransform(dist, CRS("+proj=longlat +datum=WGS84"))

dist$new <- 0

count <- 0
for(i in 1:nrow(data.frame(dist))){
  #print(dist[i,])
  count <- count + .01
  result <- block[block$district == dist[i,]$District & block$block == dist[i,]$Block,]
  if (nrow(result) > 0 ){
    dist$new[i] <- count
  }
  else{
    dist$new[i] <- 0
  }
  
}

colHash <- hash()
colHash["total_educated"] <- "total_educated"
colHash["total_residents"]    <- "total_residents"
colHash["literacy"] <- "literacy"
colHash["avg_info_source"] <- "avg_info_source"