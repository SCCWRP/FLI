library(shiny)
library(shinyIncubator)

source("r/use.r")
options(error = stop)
options(stringsAsFactors=FALSE)

mayfly <- normalizePath(file.path(getwd(), "img", "baetis_habitus3.png"))
shinyServer(function(input, output, session){
  
  ### FLI SECTION ###
  
  results <- reactive({
    if(input$bug_submit > 0){
    bugs <- read.csv(isolate(input$bugs$datapath))
    stations <- generate_stations(read.csv(isolate(input$GIS$datapath)))
    tryCatch(fli(bugs, stations, as.numeric(input$sampleSize)),
             error = function(e){
               cat("\n")
               print(e)
               list(data.frame(ThereWasAnError=NA))})
    } else list(test = data.frame(SampleID = NA,
                                  count = NA,
                                  excluded = NA,
                                  pctAmbiguousIndividuals = NA,
                                  pctAmbiguousTaxa = NA,
                                  E = NA,
                                  O = NA,
                                  OoverE = NA,
                                  pMMI = NA,
                                  FLI = NA))
  })
  
  output$results <- renderTable(results()[[1]])
  output$mf <- renderImage(list(src=mayfly), deleteFile=FALSE)
  
  output$dlhandler <- downloadHandler(function()paste0(input$report, ".csv"),
                                      function(f){
                                        rn <- ifelse(input$report %in% c("captureProbs",
                                                                         "groupProbs"),
                                                     TRUE, FALSE)
                                        write.csv(results()[[input$report]], f,
                                                  row.names=rn)
                                      })
})




