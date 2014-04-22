library(stringr)

source("r/find_replace.r")

#bug data
bugs_all <-local({
  load("data/metadata.rdata")
  bugs<- rbind(read.csv("data/bugs.ref.csv"),
               read.csv("data/bugs.nonref.csv"))
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
stations <- read.csv("data/full_stations.csv")
