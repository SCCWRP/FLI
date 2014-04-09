library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Family Level Index"),
  
  sidebarPanel(
    conditionalPanel(condition="input.mainTab==1", HTML(readLines("html/fli_instructions.html"))),
    conditionalPanel(condition="input.mainTab==2", HTML(readLines("html/gis_instructions.html"))),
    tags$br(),
    tags$br(),
    imageOutput("mf")),
    
  mainPanel(tabsetPanel(
    tabPanel("FLI Calculator",
             fileInput("bugs", "Upload BMI data"),
             fileInput("GIS", "Upload GIS data"),
             numericInput("sampleSize", "Sample Size", 100, min=1),
             tags$br(),
             actionButton("bug_submit", "submit"),
             tags$br(),
             tags$br(),
             tableOutput("results"),
             tags$br(),
             downloadButton("dlhandler"),
             value=1),
    tabPanel("Get GIS data",
             fileInput("coords", "Upload Station Coordinates"),
             tags$br(),
             conditionalPanel("output.gis_result", downloadButton("gishandler")),
             tableOutput("gis_result"),
             value=2),
    id="mainTab"))
  
  )
)

#gis_result

#!output.gis_result & 