library(shiny)

if(!all(file.exists("data/mmimodels.rdata", "data/oemodels.rdata"))){
  cat("First time use: building MMI and OE models")
  suppressMessages(source("r/build_all.r"))
}
cat("Launching application")
suppressMessages(runApp(".", launch.browser=TRUE, port=5678))
