source("r/use.r")
options(stringsAsFactors=FALSE)



worksheetConvert(c(head(b1$FinalID), "TAXON15", "TAXON12"))
b1 <- read.csv("www/BMI_example.csv")
s1 <- read.csv("www/gis_results.csv")
test <- fli(b1, s1, 100)

