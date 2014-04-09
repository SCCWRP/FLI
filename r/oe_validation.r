library(reshape2)
library(plyr)
library(ggplot2)
library(randomForest)

source("r/oe_model_fun.r")
source("r/fli_data.r")
load("oemodelsv2_20140328.rdata")

predictors_all <- stations[, c("StationCode", "New_Lat", "New_Long",
                               "TEMP_00_09", "PPT_00_09", "SITE_ELEV")]
names(predictors_all)[2:6] <- c("Lat", "Long", "temp", "ppt", "elevation")


###Run model
result <- OEModelPredict(bugs_all, predictors_all, rfmod = oemodels[[1]]$mod,
                         calibration = oemodels[[1]]$calibration, 
                         calibration_preds = oemodels[[1]]$calibration_preds,
                         cutoff=0.5)

###Summary###
validation <- result$OoverE
validation$status <- bugs_all$SiteStatus[match(row.names(validation), bugs_all$SampleID)]
validation$set <- bugs_all$SiteSet[match(row.names(validation), bugs_all$SampleID)]
validation$select <- bugs_all$SelectedSamples[match(row.names(validation), bugs_all$SampleID)]
val_summary <-ddply(validation, "set", function(df){
  c(set = as.character(unique(df$set)), Mean_OoverE = mean(df$OoverE, na.rm=TRUE),
    Mean_OoverE_Null = mean(df$OoverE_Null, na.rm=TRUE), 
    SD_OoverE = sd(df$OoverE, na.rm=TRUE),
    SD_OoverE_Null = sd(df$OoverE_Null, na.rm=TRUE))
})
validation$SampleID <- row.names(validation)
validation$StationCode <- bugs_all$StationCode[match(validation$SampleID, bugs_all$SampleID)]

ggplot(validation[validation$set != "RefCal" & !is.na(validation$status),], aes(E, O)) + geom_point() + geom_smooth(method="lm") + geom_abline() +
  xlim(0, 25) + ylim(0, 25) + facet_wrap(~status)

ggplot(validation[!is.na(validation$status),], aes(set, OoverE)) + geom_boxplot()
sd(validation$OoverE[validation$set %in% c("RefVal", "RefCal")], na.rm=TRUE)
sd(validation$OoverE_Null[validation$set %in% c("RefVal", "RefCal")], na.rm=TRUE)

sapply(as.character(na.omit(unique(validation$set))), function(x)mean(validation$OoverE[validation$set == x], na.rm=TRUE))

###Group membership
validation$GroupID <- Reduce(c, apply(result$Group_Probs, 1, function(sample){
  which(sample %in% max(sample))[1]
}))
summary(aov(data=validation[validation$set == "RefCal" & validation$select == "Selected", ], OoverE ~ as.factor(GroupID)))
summary(aov(data=validation[validation$set == "RefCal" & validation$select == "Selected", ], OoverE_Null ~ as.factor(GroupID)))

###Precision test###
mean(ddply(validation, "StationCode", function(df)sd(df$OoverE, na.rm=TRUE))[, 2], na.rm=TRUE)
mean(ddply(validation, "StationCode", function(df)sd(df$OoverE_Null, na.rm=TRUE))[, 2], na.rm=TRUE)


###Sensitivity test##
t.test(validation$OoverE[validation$set == "RefCal"], validation$OoverE[validation$set == "StressCal"])
t.test(validation$OoverE[validation$set == "RefVal"], validation$OoverE[validation$set == "StressVal"])

t.test(validation$OoverE_Null[validation$set == "RefCal"], validation$OoverE_Null[validation$set == "StressCal"])
t.test(validation$OoverE_Null[validation$set == "RefVal"], validation$OoverE_Null[validation$set == "StressVal"])


###F test by PSA region (Accuracy)###
validation_stressors <- merge(validation, stations, by="StationCode", all.x=TRUE)

summary(aov(data=validation_stressors[validation_stressors$set %in% c("RefCal") & validation_stressors$select == "Selected", ],
            OoverE ~ as.factor(PSA6c)))
summary(aov(data=validation_stressors[validation_stressors$set %in% c("RefVal") &  validation_stressors$select == "Selected",],
            OoverE ~ as.factor(PSA6c)))
summary(aov(data=validation_stressors[validation_stressors$set %in% c("RefCal") & validation_stressors$select == "Selected",],
            OoverE_Null ~ as.factor(PSA6c)))
summary(aov(data=validation_stressors[validation_stressors$set %in% c("RefVal") & validation_stressors$select == "Selected",],
            OoverE_Null ~ as.factor(PSA6c)))

####Variance explained by gradients###
rf_stress <- na.omit(validation_stressors[, c(4, 7:8, 42:61)])
rf_stress_oe <- randomForest(data = rf_stress[, -(2:3)], OoverE ~ .)
rf_stress_oenull <- randomForest(data = rf_stress[, c(-1, -3)], OoverE_Null ~ .)

rf_natural <- na.omit(validation_stressors[, c(4, 7:9, 26, 28:34, 36:39)])
rf_natural_oe <- randomForest(data = rf_natural[rf_natural$set == "RefCal", -(2:4)], OoverE ~ ., importance=TRUE)
rf_natural_oenull <- randomForest(data = rf_natural[rf_natural$set == "RefCal", c(-1, -3, -4)], OoverE_Null ~ .)


valid.melt <- melt(validation_stressors[!is.na(validation_stressors$set), c("OoverE", "OoverE_Null", "PSA9c", "set")],
                   id.vars=c("set", "PSA9c"))
ggplot(valid.melt, aes(PSA9c, value)) + geom_boxplot() + facet_grid(set~variable) + xlab("") + ylab("O/E") + 
  theme(strip.text  = element_text(size = 20), 
        axis.text.x = element_text(size = 15)) +
  geom_hline(yintercept=1)

validation_stressors$elev <- predictors_all$elevation[match(validation_stressors$StationCode, predictors_all$StationCode)]
validation_stressors$StationCode[which(validation_stressors$elev - validation_stressors$SITE_ELEV < -5)]
