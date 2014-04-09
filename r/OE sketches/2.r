library(lubridate)
library(ggplot2)


phab <- read.csv("data/phab.csv")
phab$StationCode <- phab$DelineationCode
phab$SampleDate <- mdy(phab$SampleDate)
validation$SampleDate <- mdy(substr(bugs_all$SampleDate[match(validation$SampleID, bugs_all$SampleID)], 1, 9))

threshold <- mean(phab_validation$OoverE[phab_validation$set == "RefCal"], na.rm=TRUE) -  1.65*sd(phab_validation$OoverE[phab_validation$set == "RefCal"], na.rm=TRUE)

phab_validation <- merge(validation, phab, by=c("SampleDate", "StationCode"))

ggplot(phab_validation[phab_validation$status == "Reference", ], aes(PCT_SAFN, OoverE)) + geom_point(aes(colour=set)) +
  geom_smooth(method="lm") + geom_hline(yintercept = threshold) + theme(text = element_text(size=20))

ggplot(phab_validation[phab_validation$status == "Reference", ], aes(XSLOPE, OoverE)) + geom_point(aes(colour=set)) +
  geom_smooth(method="lm") + xlim(0, 5) + geom_hline(yintercept = threshold) + theme(text = element_text(size=20))

ggplot(phab_validation, aes(W1_HALL_SWAMP, OoverE)) + geom_point(aes(colour=status)) +
  geom_smooth(method="lm") + geom_hline(yintercept = threshold) + theme(text = element_text(size=20))



validation_stressors <- within(validation_stressors, {
  AgUrbanCode21_1K <- Ag_2000_1K + CODE_21_2000_1K + URBAN_2000_1K
  AgUrbanCode25_5K <- Ag_2000_5K + CODE_21_2000_5K + URBAN_2000_5K
  AgUrbanCode21_WS <- Ag_2000_WS + CODE_21_2000_WS + URBAN_2000_WS
  Residual_Conductivity <- abs(MaxOfCOND - CondQR50)
})

ggplot(validation_stressors[validation_stressors$status == "Reference", ], aes(PCT_SEDIM, OoverE)) +
  geom_point(aes(colour=set)) + geom_smooth(method="lm") +
  geom_hline(yintercept = threshold) + theme(text = element_text(size=20))


ggplot(validation_stressors, aes(AgUrbanCode21_1K, OoverE)) + geom_point(aes(colour=status)) + geom_smooth(method="lm") +
  geom_hline(yintercept = threshold) + theme(text = element_text(size=20))

ggplot(validation_stressors, aes(Residual_Conductivity, OoverE)) + geom_point(aes(colour=status)) + geom_smooth(method="lm") +
  geom_hline(yintercept = threshold) + theme(text = element_text(size=20)) + xlim(0, 5000)