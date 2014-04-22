library(shiny)

if(!all(file.exists("data/mmimodels.rdata", "data/oemodels.rdata"))){
  print("First time use: building MMI and OE models")
  source("r/build_all.r")
}

runApp(".", launch.browser=TRUE, port=5678)