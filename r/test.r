source("r/use.r")
options(stringsAsFactors=FALSE)



b1 <- read.csv("www/BMI_example.csv")
s1 <- read.csv("www/gis_results.csv")
test <- fli(b1, s1, 500)

b2 <- read.csv("C:/Users/marke/Desktop/testbug.csv")
s2 <- read.csv("C:/Users/marke/Desktop/teststations.csv")
test <- fli(b2, generate_stations(s2), 100)
