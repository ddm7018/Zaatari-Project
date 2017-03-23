library(gtools)
library(data.table)
library(XML)
source("core.R")

update <- function(data){

  #need the distirct 
  data$district <- "2"
  
  #ETL rule (hardcoded for now)
  data$information.age_informant <- as.character(data$information.age_informant)
  data$information.age_informant2 <- as.character(data$information.age_informant2)
  data$information.age_informant[data$information.age_informant == "15_24"] <- "20"
  data$information.age_informant[data$information.age_informant == "25_44"] <- "34"
  data$information.age_informant[data$information.age_informant == "45_64"] <- "55"
  data$information.age_informant[data$information.age_informant == "64_"] <- "65"

  #call the save RDS
  assetTable <- readRDS("cap_data/raw_assets.rds")
  #cleanFinal <- readRDS("cap_data/block_summary.rds")
  
  #join raw data
  newAssetData   <- smartbind(assetTable,data)
  
  #district summary
  sumDataAndSave(newAssetData)
  
}






