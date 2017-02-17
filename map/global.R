library(dplyr)
library(hash)

asset <- readRDS("cap_data/raw_assets.rds")
block <- readRDS("cap_data/block_summary.rds")

colHash <- hash()
colHash["total_educated"] <- "total_educated"
colHash["total_residents"]    <- "total_residents"
colHash["literacy"] <- "literacy"
colHash["avg_info_source"] <- "avg_info_source"