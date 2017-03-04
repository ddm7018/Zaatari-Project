library(gtools)
library(data.table)
library(XML)

source("core.R")


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

#reading and parsing the block locations from the kml files
sumDataAndSave(joinTable)



