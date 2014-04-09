library(vegan)
library(ggplot2)
library(reshape2)


familycount <- ddply(bugs_all, .(SampleID, Family), function(df)length(unique(df$FinalID)))

test <- ddply(familycount, .(SampleID), function(df){
  data.frame(families = nrow(df),
             richness = sum(df$V1),
             loss = nrow(df) - sum(df$V1))
})

test$lossPercent <-abs(test$loss)/test$richness

ggplot(test, aes(richness, lossPercent)) + geom_point()
bugs_all2 <- bugs_all
bugs_all2$Family <- as.character(bugs_all2$Family)
bugs_all2$Family[bugs_all2$Family == ""] <- bugs_all2$FinalID[bugs_all2$Family == ""]
familyMatrix <- acast(bugs_all2, SampleID ~ Family, value.var="BAResult", sum)[, -1]
familyMatrix[is.na(familyMatrix)] <- 0
familyDist <- vegdist(familyMatrix)

speciesMatrix <- acast(bugs_all, SampleID ~ FinalID, value.var="BAResult", sum)
speciesMatrix[is.na(speciesMatrix)] <- 0
speciesDist <- vegdist(speciesMatrix)

taxDist <- mantel(familyDist, speciesDist, method="spearman", permutations=100)

procrust <- procrustes(familyDist, speciesDist)


richness <- ddply(bugs_all, .(SampleID), function(df){
  data.frame(family = length(unique(df$Family)),
             species = length(unique(df$FinalID)))
  })

cor.test(richness$family, richness$species, method="pearson")


mean(richness$species/richness$family)


### Further matricies

familypa <- familyMatrix
familypa[familypa > 0] <- 1
familypaD <- vegdist(familypa)

speciespa <- speciesMatrix
speciespa[speciespa > 0] <- 1
speciespaD <- vegdist(speciespa)

padist <- mantel(familypaD, speciespaD, method="spearman", permutations=50)


familysub <- communityMatrix(familyMatrix, 300)
familysubD <- vegdist(familysub)

speciessub <- rrarefy(speciesMatrix, 300)
speciessubD <- vegdist(speciessub)

subdist <- mantel(familysubD, speciessubD, method="spearman", permutation=50)


familysub <- communityMatrix(familyMatrix, 300)
familysub[familysub > 0] <- 1 
familysubD <- vegdist(familysub)

speciessub <- rrarefy(speciesMatrix, 300)
speciessub[speciessub > 0] <- 1
speciessubD <- vegdist(speciessub)

subdist <- mantel(familysubD, speciessubD, method="spearman", permutation=50)


mean(rowSums(speciessub)/rowSums(familysub))



mean(1 - apply(familyMatrix, 1, diversity)/apply(speciesMatrix, 1, diversity))
mean(1 - apply(familypa, 1, diversity)/apply(speciespa, 1, diversity))
