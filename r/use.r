library(BMIMetrics)
library(Hmisc)
library(plyr)
library(randomForest)
library(reshape2)
source("r/gis.r")
source("r/aggregate_family.r")

load("data/metadata.rdata")
load("data/mmimodels.rdata")
load("data/oemodels.rdata")


fli_oe <- function(bugs, pred, size){
  
  ### OE###
  
  bugs$Family_OTU <- as.character(metadata$Family_OTU[match(bugs$FinalID, metadata$FinalID)])
  observed <- ddply(bugs, .(SampleID, Family_OTU), summarise,
                    O = sum(BAResult))
  
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
                count = sum(BAResult),
                excluded = sum(BAResult[Family_OTU %in% c("Unambiguous_NotAtRefCal", "Exclude")]),
                prctAmbiguousIndividuals = 100*mean(BAResult[Family_OTU == "Ambiguous"]),
                prctAmbiguousTaxa = 100*mean(unique(Family_OTU) == "Ambiguous"))
    qc$prctAmbiguousIndividuals[is.nan(qc$prctAmbiguousIndividuals)] <- 0
    
    excluded <- bugs$Family_OTU %in% c("Unambiguous_NotAtRefCal", "Exclude", "Ambiguous")
    bugs <- bugs[!excluded, ]
    
    bugs_pa <- communityMatrix(bugs)
    bugs_pa[bugs_pa > 0] <- 1
    predictors <- join(
      unique(bugs[, c("StationCode", "SampleID")]), predictors, match="first", by="StationCode"
    )
    
    
    group_probs <- predict(rfmodel, newdata = predictors, type = 'prob')
    row.names(group_probs) <- predictors$StationCode
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
  oe$Capture_Probs <- melt(oe$Capture_Probs)
  oe$Capture_Probs$StationCode <- bugs$StationCode[match(oe$Capture_Probs[, 1],
                                                         bugs$SampleID)]
  names(oe$Capture_Probs) <- c("SampleID", "Family_OTU", "CaptureProb", "StationCode")
  oe$Capture_Probs <- merge(oe$Capture_Probs, observed, all.x=TRUE)
  oe$Capture_Probs$O[is.na( oe$Capture_Probs$O)] <- 0
  oe$Capture_Probs <- oe$Capture_Probs[, c("StationCode", "SampleID",
                                           "Family_OTU", "CaptureProb", "O")]
  oe
}

fli_mmi <- function(bugs, pred, size) {
  ### MMI
  pred <- rename(pred, c("Lat"="New_Lat", "Long"="New_Long",
                  "ppt"="PPT_00_09", "temp"="TEMP_00_09", "elevation"="SITE_ELEV"))
  stations <- pred[, c("StationCode", "New_Long", "New_Lat", 
                       "SITE_ELEV", "TEMP_00_09", "PPT_00_09")]


  set <- mmimodels[[match(size, c(200, 300, 500, 100))]]
  
  BMI <- sample(BMI(bugs), size)
  BMI <- aggregate(BMI)
  mets <- BMIall(BMI, 1)
  mets$StationCode <- bugs$StationCode[match(mets$SampleID,
                                             bugs$SampleID)]
  
  BMIstations <- join(mets, stations, by="StationCode", match="first")
  metricsL <- strsplit(set[[1]], "_")
  metrics <- sapply(metricsL, function(x)paste(x[1], x[2], sep="_"))
  type <- sapply(metricsL, "[", 3)
  minmax <- set[[3]]
  
  predicted <- as.data.frame(sapply(metrics, function(m){
    predict(set[[2]][[m]], BMIstations)
  }))
  
  scores <- mapply(function(metric, resid){
    x <- BMIstations[, metric] - predicted[, metric]
    if(metric == "Noninsect_PercentTaxa"){
      (x - minmax[metric, "max_i"])/(minmax[metric, "min_i"] - minmax[metric, "max_i"])
    } else
      (x - minmax[metric, "min_d"])/(minmax[metric, "max_d"] - minmax[metric, "min_d"])
  }, metrics, type)
  
  if(class(scores) != "matrix"){
    scores <- t(scores)
  }
  colnames(scores) <- set[[1]]
  scores[scores < 0] <- 0
  
  scores <- as.data.frame(scores)

  scores$SampleID <- BMIstations$SampleID
  scores$MMI <- apply(scores[, 1:length(metrics)], 1, mean, na.rm=TRUE)
  
  full <- scores
  names(full)[names(full) != "SampleID"] <- paste0(names(full)[names(full) != "SampleID"],
                                                   "_score")
  names(predicted) <- paste0(names(predicted), "_predicted")
  mets <- mets[, c("StationCode", metrics)]

  full <- cbind(full, predicted, mets)
  first <-c("StationCode", "SampleID")
  full <- full[, c(first, names(full)[!names(full) %in% first])]
  
  full <- full[, c("StationCode", sort(names(full)[names(full) != "StationCode"]))]
  list(scores, full)
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
  list(core = core, pMMI_supplement = mmi[[2]],
       OE_supplement = oe[[2]], groupProbs = oe[[3]],
       stationGIS = pred)
}

generate_stations <- function(coords) {
  stations <- gis(as.matrix(coords[, c("Long", "Lat")]))
  stations <- cbind(coords, stations[, c("ppt", "temp", "elevation")])
  stations
}
