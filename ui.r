library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Family Level Index"),
  
  sidebarPanel(
    HTML(readLines("html/fli_instructions.html")),
    tags$br(),
    tags$br(),
    imageOutput("mf")),
  
  mainPanel(
    fileInput("bugs", "Upload BMI data"),
    fileInput("GIS", "Upload station coordinates"),
    radioButtons("sampleSize", "Sample Size", c(100, 200, 300, 500)),
    tags$br(),
    actionButton("bug_submit", "submit"),
    tags$br(),
    tags$br(),
    tableOutput("results"),
    tags$br(),
    selectInput("report", "Select Report", c("core",
                                             "captureProbs",
                                             "groupProbs",
                                             "metrics",
                                             "scores",
                                             "stationGIS"),
                selected = "core"),
    downloadButton("dlhandler", "Get Report")
  ) 
))

#gis_result

#!output.gis_result & 