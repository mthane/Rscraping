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

source("shiny/Code/Programmingtrend_module.R")

ui <- dashboardPage(
    dashboardHeader(title = "Programming Trends"),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Data sets", tabName = "datasets"),
        
        menuItem("Trends", tabName = "trends")
        
        )
    ),
    dashboardBody(
        tabItems(
        tabItem("datasets",
                tabsetPanel(
                    tabPanel("Reddit Data",
                             redditDataCollectionUI("redditDataCollection")
                    ),
                    tabPanel("Stackoverflow Data"
                    )
                    
                    
                )
        ),
        tabItem("trends",
                trendUI("trend")
                )
    )
    
    )
    
    
)

server <- function(input, output) {
    data <- redditDataCollectionServer("redditDataCollection")
    options(shiny.maxRequestSize=100*1024^2)
    trendServer("trend",data)
    
}

shinyApp(ui, server)