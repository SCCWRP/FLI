library(ggmap)

CSCI <- read.csv("data/sample.data.csv")

validation_stressors$CSCI <- CSCI$OoverE[match(validation_stressors$SampleID, CSCI$SampleID)]
testmap <- get_googlemap(center = c(lon= -120.5, lat=37),
                         maptype="satellite", crop=T, style="feature:road|element:all|visibility:off", zoom=6)

ggmap(testmap) + geom_point(data=predictors_cal, aes(Long, Lat), colour="red") + facet_wrap(~GroupID)

compare <- melt(validation_stressors[, c("New_Long", "New_Lat", "OoverE", "CSCI")], id.vars=c("New_Long", "New_Lat"))
compare$variable <- as.character(compare$variable)
compare$variable[compare$variable == "OoverE"] <- "Family"
ggmap(testmap) + geom_point(data=compare, aes(New_Long, New_Lat, colour=value), size=3, alpha=0.8) + 
  scale_color_gradient2(low="red", mid="yellow", high="darkgreen", midpoint=0.6, name="O/E score") + facet_wrap(~ variable) +
  xlab("") + ylab("") + theme(text=element_text(size=20))



ggplot(validation_stressors, aes(OoverE, CSCI)) + geom_point() + geom_abline() + xlab("Family O/E") +
  ylab("CSCI O/E") + theme(text = element_text(size = 20)) + geom_smooth(method="lm")

with(validation_stressors, sqrt(mean((OoverE - CSCI)^2, na.rm=T)))

summary(lm(data=validation_stressors, OoverE ~ CSCI))


phab <- read.csv("C:/Documents and Settings/gisuser/Desktop/PHAB_result.csv")
testframe <- data.frame(bugs = as.character(unique(bugs_all$BugCode)),
           phab1 = unique(phab$SampleID)[pmatch(as.character(bugs_all$StationCode[!duplicated(bugs_all$BugCode)]), as.character(unique(phab$SampleID)))],
           phab2 = unique(phab$SampleID)[pmatch(as.character(unique(bugs_all$BugCode)), as.character(unique(phab$SampleID)))])

testframe <- within(testframe, {
  phab3 <- ifelse(!is.na(phab1), as.character(phab1), as.character(phab2))
})
testframe$safn <- phab$mean[which(phab$metric == "PCT_SAFN")][match(testframe$phab3, phab$SampleID[which(phab$metric == "PCT_SAFN")])]
testframe$slope <- phab$mean[which(phab$metric == "XSLOPE")][match(testframe$phab3, phab$SampleID[which(phab$metric == "XSLOPE")])]

validation$SAFN <- testframe$safn[match(validation$StationCode, testframe$bugs)]
validation$slope <- testframe$slope[match(validation$StationCode, testframe$bugs)]
vmelt <- melt(validation[, c("SAFN", "slope", "status", "set", "OoverE")], id.vars=c("status", "set", "OoverE"))
ggplot(vmelt[vmelt$set != "RefCal" & !is.na(vmelt$set), ], aes(value, OoverE)) +  
  geom_smooth(method="lm", aes(colour=status)) + facet_wrap(~variable, scales="free_x") + theme(text = element_text(size=20)) +
  xlab("")


