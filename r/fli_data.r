library(stringr)

source("P:/MarkEngeln/r functions/find_replace.r")

#bug data
bugs_all <-local({
  load("data/metadata.rdata")
  bugs<- rbind(read.csv("L:/CSCI_ME/temp/development_data/andy/bugs.ref.csv"),
               read.csv("L:/CSCI_ME/temp/development_data/andy/bugs.nonref.csv"))
  bugs$FinalID <- str_trim(bugs$FinalID)
  replace <- c("Microtendipes Rydalensis Group", "Orthocladius Complex", "Cricotopus Bicinctus Group", "Cricotopus Trifascia Group")
  bugs$FinalID <- find_replace(bugs$FinalID, replace, c("Microtendipes rydalensis group", 
                                                        "Orthocladius complex", "Cricotopus bicinctus group", "Cricotopus trifascia group"))
  
  
  bugs$Family_OTU <- as.character(metadata$Family_OTU[match(bugs$FinalID, metadata$FinalID)])
  bugs
})
bugs_all$LifeStageCode <- toupper(as.character(bugs_all$LifeStageCode))
bugs_all$LifeStageCode[bugs_all$LifeStageCode == ""] <- "L"

#Natural gradient/GIS data
# stations <- rbind(read.csv("L:/CSCI_ME/temp/development_data/andy/stations.ref.csv"),
#                   read.csv("L:/CSCI_ME/temp/development_data/andy/stations.nonref.csv"))
stations <- read.csv("data/full_stations.csv")
