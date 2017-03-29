library(gtools)
library(data.table)
library(XML)

source("core.R")

file.remove("cap_data/raw_assets.rds")
file.remove("cap_data/block_summary.rds")


#read in asset csv files, assign district column based on relevant district
#join the tables together

d2          <- read.csv("cap_data/d2.csv")
d8          <- read.csv("cap_data/d8.csv")
d9          <- read.csv("cap_data/d9.csv")


d2$district <- "2"
d8$district <- "8"
d9$district <- "9"

d2$information.age_informant <- as.character(d2$information.age_informant)
d8$information.age_informant <- as.character(d8$information.age_informant)
d9$information.age_informant <- as.character(d9$information.age_informant)
d2$information.age_informant[d2$information.age_informant == "15_24"] <- "20"
d2$information.age_informant[d2$information.age_informant == "25_44"] <- "34"
d2$information.age_informant[d2$information.age_informant == "45_64"] <- "55"
d2$information.age_informant[d2$information.age_informant == "64_"] <- "65"

joinTable   <- smartbind(d2,d8)
joinTable   <- smartbind(joinTable,d9)


joinTable[joinTable$education_skills.literate == "other",]$education_skills.literate <-NA
joinTable[joinTable$education_skills.literate_other == "n/a",]$education_skills.literate_other<- NA

joinTable$literate <- NA
for(ele in 1:nrow(joinTable)){
   joinTable$literate[ele] <- sum(as.numeric(as.character(joinTable$education_skills.literate[ele])),as.numeric(as.character(joinTable$education_skills.literate_other[ele])),na.rm = TRUE)
}



saveRDS(joinTable, "cap_data/joinTable.rds")

#reading and parsing the block locations from the kml files
saveRDS(sumData(joinTable),"cap_data/block_summary.rds")



