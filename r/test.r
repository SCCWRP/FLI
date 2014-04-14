source("r/use.r")
options(stringsAsFactors=FALSE)




b1 <- read.csv("L:/CSCI_ME/bugs_sub.csv")
s1 <- read.csv("L:/CSCI_ME/stations_sub.csv")
test2 <- fli(b1, s1, 500)



# gisdat <- read.csv("C:/Users/marke/Desktop/latlong.csv")
# 
# test3 <- generate_stations(gisdat)
# 
# gdat <- as.matrix(gisdat[, 2:3])
# layer <- elev2
# gdat[which(is.na(extract(layer, gdat))), ]
