# Define UI for application that draws a histogram
library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sketchy"),
  titlePanel("Cell Pool List Maker"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV or Excel File"),
      textInput("pool_name", "Pool Name *Will be final name of sheet*"),
      numericInput("cells", "Cells in One Vial", value = 5e6, min = 1),
      downloadButton("download", "Download Picklist"),
      br(),
      br(),
      img(src = "logo-primary.png", height = 70, width = 250),
      br(),
      "This app was created by Alvin Kalathungal",
      br(),
      "For more information on this app and all my other work checkout on",
      tags$a(href = "https://github.com/AlvinKalathungal/", "Github"),
      
    
  )
,
  mainPanel(
    DTOutput("table")
  )
)
)
