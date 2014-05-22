worksheetConvert <- function(x){
  lookup <- read.csv("data/idlookup.csv",
                     colClasses = c("character", "character"))
  x[x %in% lookup$ID] <- lookup$Name[match(x[x %in% lookup$ID],
                                           lookup$ID)]
  x
}