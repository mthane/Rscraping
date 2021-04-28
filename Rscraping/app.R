#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)  
library(rvest)    
library(stringr)   
# Verbose regular expressions
library(rebus)     
# Eases DateTime manipulation
library(lubridate)

library(DT)
library(rjson)
library(purrr)

library(htm2txt)
library(XML)
library(data.table)
source("RedditScraper.R")
# Define UI for application that draws a histogram
ui <- fluidPage(


textInput("reddit","Reddit","de"),
numericInput("nposts","Number of posts",10),
actionButton("scrape_json","Scrape"),
DTOutput("jsonprint")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #download json file
    scrape_json <- observeEvent(input$scrape_json,{
        download.file(paste("https://www.reddit.com/r/",
                            input$reddit,
                            "/new/.json?limit=",
                            input$nposts,sep=""),
                      "test.json")
    })
    
    data <- reactive({
        jsonfile = fromJSON(file = "test.json")
        ndocuments <- length(jsonfile$data$children)
        dfs <- list()
        df <- data.frame()
        for (i in 1:ndocuments){
            data <- jsonfile$data$children[i][[1]]$data
            print(names(data))
            columns <- names(data)[1:10]
            
            list_data <- data[columns]%>%as.list()
            
            dfs[[i]]<-list_data%>%replace_na("")%>%as.data.frame()
            
            
        }
        rbindlist(dfs,fill=TRUE)
        
    })
    
    output$jsonprint <- renderDataTable({
        data()
        
        })

        
    
}

# Run the application 
shinyApp(ui = ui, server = server)












