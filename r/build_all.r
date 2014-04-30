load("data/selectedFamilyvars.rdata")
load("data/BMIlevels.rdata")
source("r/OE_model_builder.r")


mmimodels <- Map(function(mets, BMI){
  metrics <- sapply(strsplit(mets, "_"), function(x)paste(x[1], x[2], sep="_"))
  BMIstations <- merge(BMI, stations, all.x=TRUE)
  envpreds <- c("New_Long", "New_Lat", "SITE_ELEV", "TEMP_00_09", "PPT_00_09")
  BMIstations <- na.omit(BMIstations[, c(envpreds, metrics, "SiteSet", "SampleID")])
  preds <- BMIstations[BMIstations$SiteSet == "RefCal", envpreds]
  convenient_models <- lapply(as.list(BMIstations[BMIstations$SiteSet == "RefCal", metrics]), function(met){  
    set.seed(12345)
    randomForest(x=preds,
                 y=met, ntree=1000)
  })
  ####Scoring####################
  BMIstations <- na.omit(BMIstations[, c(metrics, envpreds, "SampleID", "SiteSet")])
  minmax <- Reduce(rbind, lapply(metrics, function(metric){
    
    x <- predict(convenient_models[[metric]], BMIstations)
    BMIstations[, metric] <- BMIstations[, metric] - x
      
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
  
  scores[scores < 0] <- 0
  scores <- as.data.frame(scores)
  scores$SampleID <- BMIstations$SampleID
  scores$SiteSet <- BMIstations$SiteSet
  scores$MMI <- apply(scores[, 1:(ncol(scores)-2)], 1, mean, na.rm=TRUE)
  scores$MMI <- scores$MMI / mean(scores$MMI[scores$SiteSet == "RefCal"])
  list(mets, convenient_models, minmax, scores)
}, selected, BMIlevels)

save(oemodels, file="data/oemodels.rdata")
save(mmimodels, file="data/mmimodels.rdata")
