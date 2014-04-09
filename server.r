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
    stations <- read.csv(isolate(input$GIS$datapath))
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
  
  
#   output$dlhandler <- downloadHandler("FLI_results.csv",
#                                       function(f)write.csv(results()[[1]], f, row.names=FALSE))
#   
  output$dlhandler <- downloadHandler("FLI_results.tar",
                                      function(f){
                                        if(!file.exists("result"))dir.create("result")
                                        ns <- paste0("result//", names(results()), ".csv")
                                        mapply(function(x,y)write.csv(x, y), results(), ns)
                                        tar(f, "result")
                                      })

  ### GIS SECTION ###

  output$status <- renderText({
    if(!is.null(input$coords$datapath) & is.null(gis_result()))"Please Wait"
  })
  
  gis_result <- reactive({
    if(!is.null(input$coords$datapath)){
      data <- read.csv(input$coords$datapath)
      generate_stations(data)}
    else 
      NULL
  })

  
  output$gis_result <- renderTable(gis_result())
    
  
  output$gishandler <- downloadHandler("gis_results.csv",
                                       function(f)write.csv(gis_result(),
                                                            f, row.names=FALSE))
})




