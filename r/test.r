source("r/use.r")
options(stringsAsFactors=FALSE)



b1 <- read.csv("www/BMI_example.csv")
s1 <- read.csv("www/gis_results.csv")
test <- fli(b1, s1, 500)
