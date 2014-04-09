source("P:/MarkEngeln/r functions/find_replace.r")

load("data/station.rdata")
BMI <- read.csv("data/full_BMI_metrics.csv")
BMI[is.na(BMI)] <- 0
BMI <- BMI[, colwise(function(x)length(unique(x)))(BMI) > 5]
bugs_all <-local({
  load("data/metadata.rdata")
  bugs<- rbind(read.csv("development_data/andy/bugs.ref.csv"), read.csv("development_data/andy/bugs.nonref.csv"))
  bugs$FinalID <- str_trim(bugs$FinalID)
  replace <- c("Microtendipes Rydalensis Group", "Orthocladius Complex", "Cricotopus Bicinctus Group", "Cricotopus Trifascia Group")
  bugs$FinalID <- find_replace(bugs$FinalID, replace, c("Microtendipes rydalensis group", 
                                                        "Orthocladius complex", "Cricotopus bicinctus group", "Cricotopus trifascia group"))
  
  
  bugs$Family_OTU <- as.character(metadata$Family_OTU[match(bugs$FinalID, metadata$FinalID)])
  bugs
})

BMI$StationCode <- sapply(strsplit(as.character(BMI$SampleID), "_"), function(l)l[1])
BMI$SiteSet <- bugs_all$SiteSet[match(BMI$SampleID, bugs_all$SampleID)]
BMI <- subset(BMI, StationCode %in% stations$StationCode)
stations <- merge(BMI[, c("StationCode", "SampleID")], stations, all.x=TRUE)
natural_preds <- c("New_Long", "New_Lat", "SITE_ELEV", "LogWSA", "ELEV_RANGE", "TEMP_00_09", "PPT_00_09", 
                   "SumAve_P", "KFCT_AVE", "BDH_AVE", "MgO_Mean", "Log_P_MEAN", "CaO_Mean", "PRMH_AVE", "S_Mean",
                   "PCT_SEDIM", "LPREM_mean","Log_N_MEAN")

stations <- within(stations, {
  Log_P_MEAN <- log(P_MEAN)
  Log_N_MEAN <- log(N_MEAN)
})

bugs_sub <- bugs_all[bugs_all$SampleID %in% unique(bugs_all$SampleID)[1:10], ]
stations$elevation <- stations$MAX_ELEV
stations$Lat <- stations$New_Lat
stations$Long <- stations$New_Long
stations$ppt <- stations$PPT_00_09
stations$temp <- stations$TEMP_00_09