library(Hmisc)
library(plyr)
load("inst/metadata.rdata")
metadata$Family_OTU <- metadata$EffectiveEffortLevel_Family

refbugs <- subset(bugs, SiteSet == "RefCal")

refbugs$FamilySTE <- metadata$EffectiveEffortLevel_Family[match(refbugs$FinalID, metadata$FinalID)]



taxacount <- count(refbugs$FamilySTE)
taxacount[!grepl("idae", taxacount[, 1]), ]
###Oribatei & Trombidiformes lumped to Acari###
metadata$Subclass[metadata$Family_OTU == "Oribatei"] <- "Acari"
metadata$Subclass[metadata$Family_OTU == "Parasitiformes"] <- "Acari"
metadata$Family_OTU[metadata$Subclass == "Acari"] <- "Acari"

###Lumping to Oligochaeta###

metadata$Family_OTU[metadata$Class == "Oligochaeta"] <- "Oligochaeta"

###Lump to Ostracod###

metadata$Family_OTU[metadata$Class == "Ostracoda"] <- "Ostracoda"

###Lump to Gastropoda

metadata$Family_OTU[metadata$Class == "Gastropoda"] <- "Gastropoda"

###Lump to Bivaliva
Pelecypoda <- metadata[metadata$FinalID == "Bivalvia", ]
Pelecypoda$FinalID[1] <- "Pelecypoda"
metadata <- rbind(metadata, Pelecypoda)
metadata$Family_OTU[metadata$Class == "Bivalvia"] <- "Bivalvia"

###Not at refcal
metadata$Family_OTU[which(!(metadata$Family_OTU %in% refbugs$FamilySTE))] <- "Unambiguous_NotAtRefCal"

###Crustacean lumping
metadata$Family_OTU[metadata$Order == "Decapoda"] <- "Decapoda"
metadata$Family_OTU[metadata$Order == "Isopoda"] <- "Isopoda"

###Ambiguous taxa###
ambig <- c("Brachycera", "Coleoptera", "Diptera", "Ephemeroptera",
           "Hemiptera", "Megaloptera", "Odonata", "Plecoptera", "Trichoptera")

metadata$Family_OTU[metadata$Family_OTU %in% ambig] <- "Ambiguous"

###Excludes###

exclude <- c("Notonectidae", "Apataniidae", "Ptilodactylidae", "Staphylinidae", "Scirtidae", "Eulichadidae", "Hydrochidae",
             "Pelecorhynchidae", "Thaumaleidae", "Culicidae", "Dolichopodidae", "Ptychopteridae", "Chaoboridae",     
             "Muscidae", "Syrphidae", "Oreoleptidae")

metadata$Family_OTU[metadata$Family_OTU %in% exclude] <- "Exclude"

###Arctopsychinae exception

metadata$Family_OTU[metadata$Subfamily == "Arctopsychinae"] <- "Arctopsychinae"
#save(metadata, file="inst/metadata_OTU_update.rdata")
