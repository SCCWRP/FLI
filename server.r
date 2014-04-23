library(shiny)
library(shinyIncubator)

source("r/use.r")
options(error = stop)
mayfly <- normalizePath(file.path(getwd(), "img", "mayfly_side.jpg"))
shinyServer(function(input, output, session){
  
  ### FLI SECTION ###
  
  results <- reactive({
    if(input$bug_submit > 0){
    bugs <- read.csv(isolate(input$bugs$datapath))
    stations <- generate_stations(read.csv(isolate(input$GIS$datapath)))
    tryCatch(fli(bugs, stations, input$sampleSize),
             error = function(e)list(data.frame(ThereWasAnError=NA)))
    } else list(test = data.frame(SampleID = "ExampleSite",
                      E = 15,
                      O = 12,
                      OoverE = 12/15,
                      MMI = .78,
                      FLI = mean(c(12/15, .78))))
  })
  
  output$results <- renderTable(results()[[1]])
  output$mf <- renderImage(list(src=mayfly), deleteFile=FALSE)
  
   
#   output$dlhandler <- downloadHandler("FLI_results.tar",
#                                       function(f){
#                                         if(!file.exists("result"))dir.create("result")
#                                         ns <- paste0("result//", names(results()), ".csv")
#                                         mapply(function(x,y)write.csv(x, y), results(), ns)
#                                         tar(f, "result")
#                                       })
  output$dlhandler <- downloadHandler(function()paste0(input$report, ".csv"),
                                      function(f){
                                        rn <- ifelse(input$report %in% c("captureProbs",
                                                                         "groupProbs"),
                                                     TRUE, FALSE)
                                        write.csv(results()[[input$report]], f,
                                                  row.names=rn)
                                      })
})




