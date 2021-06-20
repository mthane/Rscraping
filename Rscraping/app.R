#
library(tm)
library(data.table)
library(topicmodels)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(data.table)
library(plotly)
library(tidytext)
library(wordcloud)
library(lubridate)
library(RCurl)
library(shiny)
library(tidyverse)  
library(rvest)    
library(stringr)   
# Verbose regular expressions
library(rebus)     
# Eases DateTime manipulation
library(lubridate)
library(DT)
library(purrr)
library(data.table)
library(jsonlite)
library(shinyTime)
## app.R ##
library(shinydashboard)
source("shiny/Code/datacollection_module.R")

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        redditDataCollectionUI("redditDataCollection")
    )
)

server <- function(input, output) {
    redditDataCollectionServer("redditDataCollection")
}

shinyApp(ui, server)