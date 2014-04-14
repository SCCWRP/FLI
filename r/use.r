library(BMIMetrics)
library(Hmisc)
library(plyr)
library(randomForest)
library(reshape2)
source("r/gis.r")
# source("r/test_data.r")
# load("data/OEMOD_family1.0.rdata")
# load("data/MMI_stuff.rdata")
load("L:/CSCI_ME/FamilyIndex/data/metadata.rdata")
load("move/mmimodelsV2_20140227.rdata")
load("move/oemodelsv2_20140328.rdata")

fli_oe <- function(bugs, pred, size){
  
  ### OE###
  
  
  
  bugs$Family_OTU <- as.character(metadata$Family_OTU[match(bugs$FinalID, metadata$FinalID)])
  
  communityMatrix <- function(bugs, OTU, x=size){ # x = value to subsample to
    bugs.sub <- bugs[bugs[, OTU] %nin% c("Exclude", "Ambiguous", "NA"), ]
    bugall<- acast(bugs.sub, SampleID ~ Family_OTU, value.var="BAResult", fun.aggregate=sum)
    samp <- rep(x, times=nrow(bugall))
    samp[rowSums(bugall)< x] <- rowSums(bugall)[rowSums(bugall)< x]  
    rrarefy(bugall, samp)
  }
  
  set <- oemodels[[match(size, c(100, 200, 300, 500))]]
  OEModelPredict <- function(bugs, predictors, rfmodel = set$mod,
                             calibration = set$calibration, 
                             calibration_preds = set$calibration_preds,
                             cutoff = 0.5){
 
    qc <- ddply(bugs, .(SampleID), summarise,
                count = length(Family_OTU),
                excluded = sum(Family_OTU %in% c("Unambiguous_NotAtRefCal", "Exclude")),
                prctAmbiguousIndividuals = 100*mean(Family_OTU == "Ambiguous"),
                prctAmbiguousTaxa = 100*mean(unique(Family_OTU) == "Ambiguous"))
    
    excluded <- bugs$Family_OTU %in% c("Unambiguous_NotAtRefCal", "Exclude", "Ambiguous")
    bugs <- bugs[!excluded, ]
    
    bugs_pa <- communityMatrix(bugs)
    bugs_pa[bugs_pa > 0] <- 1
    predictors <- join(
      unique(bugs[, c("StationCode", "SampleID")]), predictors, match="first", by="StationCode"
    )
    
    bugs_pa <- bugs_pa[as.character(predictors$SampleID), , drop=FALSE]
    
    
    group_probs <- predict(rfmodel, newdata = predictors, type = 'prob')
    group_occurence <- apply(calibration, 2, function(x)tapply(x, calibration_preds$GroupID, mean))
    capture_probs <- group_probs %*% group_occurence
    row.names(capture_probs) <- row.names(bugs_pa)
    
    result <- Reduce(rbind, lapply(row.names(capture_probs), function(sample){
      in_group <- colnames(capture_probs)[which(capture_probs[sample, , drop = FALSE] >= cutoff)]
      data.frame(E = sum(capture_probs[sample, in_group]),
                 O = sum(bugs_pa[sample, colnames(bugs_pa) %in% in_group])
      )
    }))
    row.names(result) <- row.names(capture_probs)
    
    
    null_model <- apply(calibration, 2, mean)
    null_taxa <- names(null_model)[null_model >= cutoff]
    result$E_Null <- sum(null_model[null_taxa])
    result$O_Null <- apply(bugs_pa[, colnames(bugs_pa) %in% null_taxa, drop=FALSE], 1, sum)
    
    result <- within(result, {
      OoverE <- O/E
      OoverE_Null <- O_Null/E_Null
    })
    
    result$SampleID <- row.names(result)
    
    result <- merge(result, qc, by="SampleID")
      
    list(OoverE = result[, c("SampleID", "count", "excluded",
                             "prctAmbiguousIndividuals",
                             "prctAmbiguousTaxa",
                             "E", "O", "OoverE",
                             "E_Null", "O_Null",
                             "OoverE_Null")],
         Capture_Probs = capture_probs,
         Group_Probs = group_probs)
  }
  
  oe <- OEModelPredict(bugs, pred)
  oe
}

fli_mmi <- function(bugs, pred, size) {
  ### MMI
  pred <- rename(pred, c("Lat"="New_Lat", "Long"="New_Long",
                  "ppt"="PPT_00_09", "temp"="TEMP_00_09", "elevation"="SITE_ELEV"))
  stations <- pred[, c("StationCode", "New_Long", "New_Lat", 
                       "SITE_ELEV", "TEMP_00_09", "PPT_00_09")]


  set <- mmimodels[[match(size, c(200, 300, 500, 100))]]

  mets <- local({
    
    bugs2 <- join(bugs, BMIMetrics::loadMetaData(), by=c("FinalID", "LifeStageCode"),
                  type="left")
    
    ddply(bugs2, .(SampleID), summarise,
          StationCode = unique(StationCode),
          Intolerant_PercentTaxa = mean(ToleranceValue <= 2, na.rm=TRUE),
          Clinger_Taxa = sum(Habit == "CN", na.rm=TRUE),
          Noninsect_PercentTaxa = mean(Class != "Insecta", na.rm=TRUE),
          Ephemeroptera_Taxa = sum(Order == "Ephemeroptera", na.rm=TRUE),
          Plecoptera_PercentTaxa = mean(Order == "Plecoptera", na.rm=TRUE),
          Plecoptera_Taxa = sum(Order == "Plecoptera", na.rm=TRUE),
          Trichoptera_Taxa = sum(Order == "Trichoptera", na.rm=TRUE),
          Shredder_Taxa = sum(FunctionalFeedingGroup == "SH", na.rm=TRUE),
          Shredder_PercentTaxa = mean(FunctionalFeedingGroup == "SH", na.rm=TRUE),
          EPT_PercentTaxa = mean(Order %in% c("Ephemeroptera",
                                              "Plecoptera",
                                              "Trichoptera"), na.rm=TRUE),
          EPT_Taxa = sum(Order %in% c("Ephemeroptera",
                                      "Plecoptera",
                                      "Trichoptera"), na.rm=TRUE),
          Predator_PercentTaxa = mean(FunctionalFeedingGroup == "P", na.rm=TRUE)
          )
  })

  BMIstations <- join(mets, stations, by="StationCode", match="first")
  metricsL <- strsplit(set[[1]], "_")
  metrics <- sapply(metricsL, function(x)paste(x[1], x[2], sep="_"))
  type <- sapply(metricsL, "[", 3)
  minmax <- set[[3]]
  
  scores <- mapply(function(metric, resid){
    x <- predict(set[[2]][[metric]], BMIstations)
    if(!is.na(resid))
      x <- BMIstations[, metric] - x
    if(metric == "Noninsect_PercentTaxa"){
      (x - minmax[metric, "max_i"])/(minmax[metric, "min_i"] - minmax[metric, "max_i"])
    } else
      (x - minmax[metric, "min_d"])/(minmax[metric, "max_d"] - minmax[metric, "min_d"])
  }, metrics, type)
  
  if(class(scores) != "matrix"){
    scores <- t(scores)
  }
  
  scores[scores < 0] <- 0
  
  scores <- as.data.frame(scores)

  scores$SampleID <- BMIstations$SampleID
  scores$SiteSet <- BMIstations$SiteSet
  scores$MMI <- apply(scores[, 1:length(metrics)], 1, mean, na.rm=TRUE)
  
  list(scores, mets)
 }



fli <- function(bugs, pred, sampleSize = 300) {
  oe <- fli_oe(bugs, pred, sampleSize)
  mmi <- fli_mmi(bugs, pred, sampleSize)
  core <- merge(mmi[[1]], oe[[1]])
  core <- core[, c("SampleID", "count", "excluded",
                   "prctAmbiguousIndividuals",
                   "prctAmbiguousTaxa",
                   "E", "O", "OoverE",
                   "MMI")]
  core$FLI <- apply(core[, c("OoverE", "MMI")], 1, mean)
  list(core = core, metrics = mmi[[2]], scores=mmi[[1]],
       captureProbs = oe[[2]], groupProbs = oe[[3]])
}

generate_stations <- function(coords) {
  stations <- gis(as.matrix(coords[, c("Long", "Lat")]))
  stations <- cbind(coords, stations[, c("ppt", "temp", "elevation")])
  stations
}

# loc <- stations[, c("StationCode", "New_Lat", "New_Long")]
# names(loc)[2:3] <- c("Lat", "Long")
# 
# gen_stations <- generate_stations(loc)
# 
# result <- fli(bugs_sub, gen_stations)