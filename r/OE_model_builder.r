###O/E Model Building###

library(randomForest)
library(reshape2)
library(plyr)
library(stringr)
library(vegan)

source("r/fli_data.r")
source("r/oe_model_fun.r")

`%nin%` <- function(x,y) !(x %in% y)

# Step 1A - Read and organize predictor data

bugs <- bugs_all
predictors <- stations
predictors <- rename(predictors, c("New_Lat"="Lat", "New_Long"="Long", 
                                   "PPT_00_09"="ppt", "TEMP_00_09"="temp",
                                   "SITE_ELEV"="elevation"))

oemodels <- lapply(c(200, 300, 500, 100), function(s){
  
  ## Step 1B - Input the bug data as a site-by-taxa matrix, and subsample.
  
  bugs_community <- communityMatrix(bugs, "Family_OTU", s)
  
  ## Step 1c - Align bug and predictor data, by site/sample
  
  
  predictors <- merge(
    unique(bugs[, c("StationCode", "SampleID")]), predictors, all.x=TRUE
  )
  
  bugs_community <- bugs_community[as.character(predictors$SampleID),]
  
  ### Step 1d - Create subsets of calibration and validation data
  bugs_pa <- bugs_community
  bugs_pa[bugs_pa > 0] <- 1 
  
  
  predictors_cal <- predictors[predictors$SampleID %in% bugs$SampleID[bugs$SiteSet == "RefCal" & bugs$SelectedSamples == "Selected"],]
  bugs_pa_cal <- bugs_pa[predictors$SampleID %in% bugs$SampleID[bugs$SiteSet == "RefCal" & bugs$SelectedSamples == "Selected"],]
  bugs_pa_cal <- bugs_pa_cal[, which(colSums(bugs_pa_cal)/nrow(bugs_pa_cal) > 0.05)]
  
  #STEP 2 -- DISSIMILARITIES AND CLUSTER ANALYSIS
  #Bray-Curtis (Sorenson) dissimilarity for abundance data
  
#   bugs_dist <- vegdist(bugs_pa_cal[, which(colSums(bugs_pa_cal)/nrow(bugs_pa_cal) > 0.05)],
#                        method="bray")
#   
  #Cluster analysis
  #   predictors_cal$GroupID <- kmeans(bugs_dist, centers = 3)$cluster
  #Use CSCI groups instead
  
  predictors_cal <- rename(predictors_cal, c("BioticGroup"="GroupID"))
  
  
  #STEP 3 BUILD RANDOM FOREST MODEL TO PREDICT GROUP MEMBERSHIP
  candidate_predictors <- c("elevation", "Lat", "Long", "ppt", "temp")
  rfdata <- data.frame(predictors_cal[, c(candidate_predictors, "GroupID")])
  rfdata$elevation[is.na(rfdata$elevation)] <- 0
  
  rfmod <- randomForest(as.factor(GroupID) ~ ., data=rfdata, ntree = 5000, importance = TRUE,
                        norm.votes = TRUE, keep.forest = TRUE)
  list(mod = rfmod, calibration = bugs_pa_cal, calibration_preds = predictors_cal)
})
# save(oemodels, file="oemodelsv2_20140328.rdata")




