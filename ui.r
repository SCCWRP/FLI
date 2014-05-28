library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Family Level Index"),
  
  sidebarPanel(
    includeCSS("css/flicss.css"),
    HTML(readLines("html/fli_instructions.html")),
    tags$br(),
    tags$br(),
    imageOutput("mf")),
  
  mainPanel(
    fileInput("bugs", "Upload BMI data"),
    fileInput("GIS", "Upload station coordinates"),
    radioButtons("sampleSize", "Sample Size", c(100, 500)),
    tags$br(),
    actionButton("bug_submit", "submit"),
    tags$br(),
    tags$br(),
    tableOutput("results"),
    tags$br(),
    selectInput("report", "Select Report", c("core",
                                             "OE_supplement",
                                             "groupProbs",
                                             "pMMI_supplement",
                                             "stationGIS"),
                selected = "core"),
    downloadButton("dlhandler", "Get Report")
  ) 
))

#gis_result

#!output.gis_result & 