library(randomForest)
library(ggplot2)
library(ggmap)

#####Validation############################################
load("BMImets.rdata")
load("mmimodelsV2_20140227.rdata")

stations <- rbind(read.csv("L:/CSCI_ME/temp/development_data/andy/stations.ref.csv"),
                  read.csv("L:/CSCI_ME/temp/development_data/andy/stations.nonref.csv"))

scores <- mmimodels[[c(1, 4)]]
spt <- strsplit(as.character(scores$SampleID), "_")
scores$StationCode <- sapply(spt, '[', 1)
scores$PSA9c <- stations$PSA9c[match(scores$StationCode, stations$StationCode)]
scores$StationCode[is.na(scores$PSA9c)] <- sapply(spt[is.na(scores$PSA9c)], function(x){
  paste(x[1], x[2], sep="_")
})
scores$PSA9c <- stations$PSA9c[match(scores$StationCode, stations$StationCode)]

sd(scores$MMI[scores$SiteSet %in% c("RefCal")])
tapply(scores$MMI, scores$SiteSet, mean)

ggplot(scores[scores$SiteSet %in% c("RefCal", "RefVal", "StressCal", "StressVal"),], aes(SiteSet, MMI)) +
  geom_boxplot() + geom_hline(yintercept=(1 - 1.65*0.098))


ggplot(scores[scores$SiteSet %in% c("RefCal", "RefVal", "StressCal", "StressVal"),], aes(PSA9c, MMI)) +
  geom_boxplot() + facet_wrap(~SiteSet) + geom_hline(yintercept=(1 - 1.65*0.098))

testmap <- get_googlemap(center = "California",
                         maptype="satellite", crop=T, style="feature:road|element:all|visibility:off", zoom=6)

scores <- within(scores, {
  Latitude <- stations$New_Lat[match(StationCode, stations$StationCode)]
  Longitude <- stations$New_Long[match(StationCode, stations$StationCode)] 
})

ggmap(testmap) + geom_point(data=scores, aes(Longitude, Latitude, colour=MMI)) + 
  scale_color_gradient2(midpoint=(1 - 1.65*0.098), low="red", mid="yellow", high="darkgreen")

