source("r/use.r")
options(stringsAsFactors=FALSE)



b1 <- read.csv("www/BMI_example.csv")
s1 <- read.csv("www/gis_results.csv")
test <- fli(b1, s1, 500)

# b1 <- read.csv("C:/Users/marke/Desktop/test 1tl.csv")
# s1 <- read.csv("C:/Users/marke/Desktop/test 1_stations.csv")
# test <- fli(worksheetConvert(b1), generate_stations(s1), 100)
