library(randomForest)
library(ggplot2)
library(caret)
library(reshape2)
library(BMIMetrics)
library(ggmap)
source("r/aggregate_family.r")
source("P:/MarkEngeln/r functions/find_replace.r")

# load("data/station.rdata")
# BMI <- read.csv("data/full_BMI_metrics.csv")
# BMI[is.na(BMI)] <- 0
# BMI <- BMI[, colwise(function(x)length(unique(x)))(BMI) > 5]
stations <- rbind(read.csv("L:/CSCI_ME/temp/development_data/andy/stations.ref.csv"),
                  read.csv("L:/CSCI_ME/temp/development_data/andy/stations.nonref.csv"))
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
# bugs2 <- ddply(bugs_all, .(SampleID, Family_OTU, LifeStageCode, Distinct),
#                summarise, BAResult = sum(BAResult))
# 
# names(bugs2)[2] <- "FinalID"
# bugs2 <- bugs2[!bugs2$FinalID %in% c("Unambiguous_NotAtRefCal", "Exclude", "Ambiguous"), ]
# bugs2 <- bugs2[bugs2$FinalID != "Corydalidae" & bugs2$LifeStageCode != "A", ]
# bugs2 <- bugs2[bugs2$FinalID != "Deuterophlebiidae" & bugs2$LifeStageCode != "A", ]

BMI <- BMI(bugs_all)
BMImets <- lapply(c(200, 300, 500, 100), function(s){
  BMI <- sample(BMI, s)
  BMI <- aggregate(BMI)
  BMIall(BMI, 1)
})

names(BMImets) <- c(200, 300, 500, 100)
BMIlevels <- lapply(BMIlevels, function(x){
  x$StationCode <- bugs_all$StationCode[match(x$SampleID, bugs_all$SampleID)]
  x
})
# BMI$StationCode <- sapply(strsplit(as.character(BMI$SampleID), "_"), function(l)l[1])
# BMI$SiteSet <- bugs_all$SiteSet[match(BMI$SampleID, bugs_all$SampleID)]
# BMI <- subset(BMI, StationCode %in% stations$StationCode)
load("subsampled_family_metrics.rdata")

test <- lapply(BMIlevels, function(BMI){
  
  stations <- merge(BMI[, c("StationCode", "SampleID")], stations, all.x=TRUE)
  natural_preds <- c("New_Long", "New_Lat", "SITE_ELEV", "LogWSA", "ELEV_RANGE", "TEMP_00_09", "PPT_00_09", 
                     "SumAve_P", "KFCT_AVE", "BDH_AVE", "MgO_Mean", "Log_P_MEAN", "CaO_Mean", "PRMH_AVE", "S_Mean",
                     "PCT_SEDIM", "LPREM_mean","Log_N_MEAN")
  
  stations <- within(stations, {
    Log_P_MEAN <- log(P_MEAN)
    Log_N_MEAN <- log(N_MEAN)
  })
  
  
  ###Metric Modeling (raw or residual?)#################
  
  rfmods <- lapply(as.list(subset(BMI, select = Invasive_Percent:Noninsect_Taxa)), function(response){
    response[is.na(response)] <- 0 
    randomForest(x = stations[, natural_preds], y = response, ntree = 1000)})
  rfmods
})
res1m <- lapply(rfmods, function(rfmods){
#   names(rfmods) <- names(subset(BMI, select = Invasive_Percent:Noninsect_Taxa))
  BMIres <- as.data.frame(sapply(rfmods, function(rf){
    if(!is.na(rf$rsq[length(rf$rsq)]) &&  rf$rsq[length(rf$rsq)] > .10)(rf$predicted -  rf$y) else
      (rf$y)
  }))
  names(BMIres)[t(colwise(function(x)any(x < 0))(BMIres))[, 1]] <- paste0(
    names(BMIres)[t(colwise(function(x)any(x < 0))(BMIres))[, 1]],
    "_residual")
  BMIres
})
  



####Metric Selection################
selected <- lapply(res1m, function(BMIres){
  BMIres <- BMIres[, !grepl("Toler", names(BMIres))]
  cor_matrix <- cor(as.matrix(BMIres))^2
  
  
  BMIres$SiteSet <- bugs_all$SiteSet[match(BMIlevels[[1]]$SampleID, bugs_all$SampleID)]
  BMIres$SampleID <- BMIlevels[[1]]$SampleID
  BMIres_sub <- subset(BMIres, SiteSet %in% c("RefCal", "StressCal"))
  BMI_melt <- melt(BMIres_sub, id.vars =c("SampleID", "SiteSet"))
  BMIcast <- dcast(BMI_melt, SampleID + variable ~ SiteSet, value.var="value")
  performance <- Reduce(rbind, lapply(unique(BMIcast$variable), function(metric){
    sub <- subset(BMIcast, variable == metric)
    data.frame(metric = metric,
               t_stat = abs(t.test(sub$RefCal, sub$StressCal)$statistic))
  }))
  performance <- performance[!is.na(performance$t_stat), ]
  
  
  metricSelect <- function(x, select){
    
    if(nrow(x) == 0)return(select)
    
    
    x <- x[x$t_stat > 2,]
    x <- x[order(x$t_stat, decreasing = TRUE),]
    
    if(length(select) != 0){
      if(any(na.omit(cor_matrix[select, as.character(x$metric[1])]) >= 0.5))return(metricSelect(tail(x, -1), select))
    }
    
    select[length(select) + 1] <- as.character(x$metric[1])
    x <- x[-grep(substr(x$metric[1], 1, 3), x$metric), ]
    return(metricSelect(x, select))
  }
  
  metrics <- metricSelect(performance, character(0))
  metrics[!grepl("Crusta", metrics)]
})
selected
# metrics <- metrics[-5] ###Tolerant taxa is really in the same category as intolerant taxa, so kick it out

###RF Model Refinment################
BMIstations <- merge(BMI, stations)
metrics <- c("Intolerant_PercentTaxa", "Clinger_Taxa", "Noninsect_PercentTaxa", "Ephemeroptera_Taxa", 
            "Plecoptera_PercentTaxa", "Trichoptera_Taxa", "Shredder_Taxa")
# refined_models <- lapply(as.list(BMIstations[, metrics]), function(met){
#   rfe(stations[, !(names(stations) %in% c("LogWSA", "ELEV_RANGE"))], met, sizes = 1:(ncol(stations)-2),
#                    rfeControl = rfeControl(functions = rfFuncs))
# })



mmimodels <- Map(function(mets, BMI){
  metrics <- sapply(strsplit(mets, "_"), function(x)paste(x[1], x[2], sep="_"))
  BMIstations <- merge(BMI, stations, all.x=TRUE)
  envpreds <- c("New_Long", "New_Lat", "SITE_ELEV", "TEMP_00_09", "PPT_00_09")
  BMIstations <- na.omit(BMIstations[, c(envpreds, metrics, "SiteSet", "SampleID")])
  preds <- BMIstations[BMIstations$SiteSet == "RefCal", envpreds]
  convenient_models <- lapply(as.list(BMIstations[BMIstations$SiteSet == "RefCal", metrics]), function(met){  
    randomForest(x=preds,
                 y=met, ntree=1000)
  })
  ####Scoring####################
  BMIstations <- na.omit(BMIstations[, c(metrics, envpreds, "SampleID", "SiteSet")])
  minmax <- Reduce(rbind, lapply(metrics, function(metric){
    x <- predict(convenient_models[[metric]], BMIstations)
    if(grepl("residual", mets[grepl(metric, mets)]))
      BMIstations[, metric] <- BMIstations[, metric] - x
    else
      BMIstations[, metric] <- x
    data.frame(
      min_d = quantile(BMIstations[BMIstations$SiteSet == "StressCal", metric], 0.05, na.rm=TRUE),
      max_d = quantile(BMIstations[BMIstations$SiteSet == "RefCal", metric], 0.95, na.rm=TRUE),
      min_i = quantile(BMIstations[BMIstations$SiteSet == "RefCal", metric], 0.05, na.rm=TRUE),
      max_i = quantile(BMIstations[BMIstations$SiteSet == "StressCal", metric], 0.95, na.rm=TRUE)
    )}))
  row.names(minmax) <- metrics
  
  scores <- sapply(metrics, function(metric){
    x <- predict(convenient_models[[metric]], BMIstations)
    x <- BMIstations[, metric] - x
    if(metric == "Noninsect_PercentTaxa"){
      (x - minmax[metric, "max_i"])/(minmax[metric, "min_i"] - minmax[metric, "max_i"])
    } else
      (x - minmax[metric, "min_d"])/(minmax[metric, "max_d"] - minmax[metric, "min_d"])
  })
  
#   scores[scores > 1] <- 1
  scores[scores < 0] <- 0
  scores <- as.data.frame(scores)
  scores$SampleID <- BMIstations$SampleID
  scores$SiteSet <- BMIstations$SiteSet
  scores$MMI <- apply(scores[, 1:(ncol(scores)-2)], 1, mean, na.rm=TRUE)
  scores$MMI <- scores$MMI / mean(scores$MMI[scores$SiteSet == "RefCal"])
  list(mets, convenient_models, minmax, scores)
}, selected, BMIlevels)






