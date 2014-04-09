library(randomForest)
library(plyr)
library(reshape2)
library(ggmap)

library(lubridate)
library(plyr)
library(reshape2)
## FLI validation ###

source("r/oe_model_fun.r")
source("r/fli_data.r")
load("oemodelsv2_20140328.rdata")
load("BMImets.rdata")
load("mmimodelsV2_20140227.rdata")
load("../fullCSCI_results.rdata")

phab <- read.csv("../extra FLI stuff/phab.csv")
phab$SampleDate <-mdy(phab$SampleDate)


predictors_all <- stations[, c("StationCode", "New_Lat", "New_Long",
                               "TEMP_00_09", "PPT_00_09", "SITE_ELEV")]
names(predictors_all)[2:6] <- c("Lat", "Long", "temp", "ppt", "elevation")

FLIres <- function(oe, n, mmi){
  oeresult <- OEModelPredict(bugs_all, predictors_all, rfmod = oe$mod,
                             calibration = oe$calibration, 
                             calibration_preds = oe$calibration_preds,
                             subsamp = n) 
  scores <- mmi[[4]]
  spt <- strsplit(as.character(scores$SampleID), "_")
  scores$StationCode <- sapply(spt, '[', 1)
  scores$PSA9c <- stations$PSA9c[match(scores$StationCode, stations$StationCode)]
  scores$StationCode[is.na(scores$PSA9c)] <- sapply(spt[is.na(scores$PSA9c)], function(x){
    paste(x[1], x[2], sep="_")
  })
  scores$selected <- bugs_all$SelectedSamples[match(scores$SampleID, bugs_all$SampleID)]
  
  com <- cbind(scores, oeresult[[1]][mmi[[4]]$SampleID, ])
  com$FLI <- apply(com[, c("OoverE", "MMI")], 1, mean)
  com
}

result <- Map(FLIres, oemodels, c(200, 300, 500, 100), mmimodels)
names(result) <- c(200, 300, 500, 100)

stressors <- c("Ag_2000_WS", "CODE_21_2000_1K", "CODE_21_2000_5K", 
  "CODE_21_2000_WS", "URBAN_2000_1K", "URBAN_2000_5K", "URBAN_2000_WS", 
  "RoadDens_1K", "RoadDens_5K", "RoadDens_WS", "PAVED_INT_1K", 
  "PAVED_INT_5K", "PAVED_INT_WS", "PerManMade_WS", "InvDamDist", 
  "MINES_5K", "MINES_WS", "GravelMineDensL_R5K", "MaxOfCOND", "MaxOfW1_HALL"
)

result <- lapply(result, function(x){
  q <- merge(x, stations)
  q$CSCI_MMI <- results2[[1]]$MMI[match(q$SampleID, results2[[1]]$SampleID)]
  q$CSCI_OoverE <- results2[[1]]$OoverE[match(q$SampleID, results2[[1]]$SampleID)]
  q$CSCI <- results2[[1]]$CSCI[match(q$SampleID, results2[[1]]$SampleID)]
  q
})
combn <- rbind.fill(lapply(c(100, 200, 300, 500), function(x){
  sub <- result[[as.character(x)]]
  sub$subsample <- x
  sub
}))
phabc <- merge(combn, phab, by.x="StationCode", 
               by.y="DelineationCode")
validation <- function(sub){
  res1 <- result[[as.character(sub)]]
  res <-  res1[res1$selected == "Selected", ]
  
  acc_cal <- summary(aov(data=res[res$SiteSet %in% c("RefCal") & res$select == "Selected", ],
                         FLI ~ as.factor(PSA9c)))[[1]]$"F value"[1]
  acc_val <- summary(aov(data=res[res$SiteSet %in% c("RefVal") & res$select == "Selected", ],
                         FLI ~ as.factor(PSA9c)))[[1]]$"F value"[1]
  
#   res <- merge(res, stations)
  stressmod <- randomForest(data = na.omit(res[, c("FLI", stressors)]),
                            FLI ~ .)
  naturalmod <- randomForest(data = na.omit(res[res$SiteSet == "RefCal", c("FLI", "TEMP_00_09", "PPT_00_09",
                                                                  "SITE_ELEV",
                                                                  "New_Lat")]),
                             FLI ~ .)
  
  sens_cal <- t.test(res$FLI[res$SiteSet == "RefCal"], res$FLI[res$SiteSet == "StressCal"])
  sens_val <- t.test(res$FLI[res$SiteSet == "RefVal"], res$FLI[res$SiteSet == "StressVal"])
  
  prec <- mean(ddply(res1, .(StationCode), summarize, var=sd(FLI))$var, na.rm=TRUE)
  
  data.frame(size = sub,
             sensitivity_cal = sens_cal$statistic,
             sensitivity_val = sens_val$statistic,
             accuracy_cal = acc_cal,
             accuracy_val = acc_val,
             precision = prec,             
             stress_model_rsq = tail(stressmod$rsq, 1),
             natural_model_rsq = tail(naturalmod$rsq, 1))
}

 test <- rbind.fill(lapply(c(100, 200, 300, 500), validation))





ggplot(result[[2]], aes(SiteSet, FLI)) + geom_boxplot() +
  facet_wrap(~PSA9c)

summary(lm(data=result[[4]], CSCI ~ FLI))



ggplot(combn, aes(CSCI, (FLI-CSCI), colour=as.factor(subsample))) +
  geom_smooth(method="lm") +
  theme_bw() +
  scale_color_discrete("Level of subsampling") +
  theme(text=element_text(size=18))

phab2 <- melt(phabc, id.vars=names(phabc)[!names(phabc) %in% c("CSCI", "FLI")])
phab2$var <- as.character(phab2$var)
phab2$var[phab2$var=="FLI"] <- paste0("FLI-", phab2$subsample[phab2$var=="FLI"])
phab2$year <- year(phab2$SampleDate)
phab2$month <- month(phab2$SampleDate)

ggplot(phab2[phab2$SiteSet != "Other", ], aes(SiteSet, value, fill=var)) + geom_boxplot() +
  theme_bw() + ylim(0, 1.5) + xlab("") + ylab("") +
  scale_fill_discrete("") + theme(text=element_text(size=18))

bmap <- get_map("Los Angeles", zoom=8, maptype="satellite")
ggmap(bmap) + 
  geom_point(data=phab2[phab2$var %in% c("CSCI","FLI-500"), ],
             aes(New_Long, New_Lat, colour=value),
             size=3, shape=16) +
  scale_color_gradient("", low="red", high="green") +
  facet_grid(~var) +
  xlab("") + ylab("")

ggplot(phab2[phab2$SiteSet == "RefCal", ], aes(var, value)) + geom_boxplot() +
  facet_wrap(~PSA6c) + theme_bw() + xlab("") + ylab("") +
  ylim(0, 1.5) + theme(text=element_text(size=18))

ggplot(phab2[phab2$SiteSet %in% c("RefCal", "RefVal"), ], 
       aes(XSLOPE, value)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~var) +
  theme_bw() +
  theme(text=element_text(size=18)) +
  ylab("") + ylim(0, 1.5) + xlim(0, 5)

combn2 <- melt(combn, id.vars=names(combn)[!names(combn) %in% c("CSCI", "FLI")])
combn2$var <- as.character(combn2$var)
combn2$var[combn2$var=="FLI"] <- paste0("FLI-", combn2$subsample[combn2$var=="FLI"])
combn2$AgUrbanCode21_1k <- apply(combn2[, c("Ag_2000_1K", "CODE_21_2000_1K",
                                            "URBAN_2000_1K")], 1, sum, na.rm=TRUE)
combn2$Condition <- ifelse(grepl("Ref", combn2$SiteSet), "Reference", ifelse(
  grepl("Stress", combn2$SiteSet), "Stressed", "Intermediate"))

sdtable <- ddply(phab2[phab2$SiteSet %in% c("RefCal", "RefVal"), ], .(StationCode, var), summarize, count = length(value), sd = sd(value))
sdtable2 <- ddply(sdtable[sdtable$sd > 0, ], .(var), summarize, sd = mean(sd, na.rm=TRUE))
ggplot(sdtable[which(sdtable$sd > 0), ], aes(var, sd)) + geom_boxplot() +
  theme(text=element_text(size=22)) +
  ylab("within-site standard deviation") + xlab("") +
  theme_bw()

ggplot(combn2, aes(AgUrbanCode21_1k, value)) +
  facet_wrap(~var) +
  geom_point(aes(colour=Condition)) +
  geom_smooth(method="lm", colour="black") +
  theme_bw() +
  scale_color_discrete("") +
  theme(text=element_text(size=18)) +
  ylab("") + ylim(0, 1.5) 