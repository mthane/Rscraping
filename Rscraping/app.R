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
library(purrr)
library(data.table)
library(jsonlite)
library(shinyTime)

source("RedditScraper.R")
# Define UI for application that draws a histogram
ui <- fluidPage(


textInput("reddit","Reddit","de"),
numericInput("nposts","Number of posts",10),
dateInput("time_from", "From", value = Sys.time()),
dateInput("time_to", "To", value= Sys.time()),
actionButton("scrape_json","Scrape"),
DTOutput("jsonprint2"),
DTOutput("tableAPI"),
textOutput("time_test")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    # FIRST SOLUTION
    #download json file
    # scrape_json1 <- observeEvent(input$scrape_json,{
    #     download.file(paste("https://www.reddit.com/r/",
    #                         input$reddit,
    #                         "/new/.json?limit=",
    #                         input$nposts,sep=""),
    #                   "test.json")
    #     
    #     
    # })
    # 
    # 
    # data1 <- reactive({
    #     jsonfile = fromJSON(file = "test.json")
    #     ndocuments <- length(jsonfile$data$children)
    #     dfs <- list()
    #     df <- data.frame()
    #     for (i in 1:ndocuments){
    #         data <- jsonfile$data$children[i][[1]]$data
    #         columns <- names(data)[1:10]
    #         list_data <- data[columns]%>%as.list()
    #         dfs[[i]]<-list_data%>%replace_na("")%>%as.data.frame()
    #     }
    #     rbindlist(dfs,fill=TRUE)
    # })
    # 
    
    # SECOND SOLUTION
    
    scrape_json2 <- eventReactive(input$scrape_json,
                                  {
                                     fromJSON(url(paste("https://www.reddit.com/r/",
                                                        input$reddit,
                                                        "/new/.json?limit=",
                                                        input$nposts,sep="")),flatten = T)
                                      
                                  })
    

    data2 <- reactive({
        scrape_json2()$data$children%>%
        select("data.subreddit",
               "data.title",
               "data.author",
               "data.selftext",
               "data.name",
               "data.id",
               "data.domain",
               "data.url",
               "data.created",
               "data.created_utc",
               "data.upvote_ratio",
               "data.ups",
               "data.score",
               "data.num_comments"
               )
    })
    
    
    
    output$jsonprint2 <- renderDataTable({
        data2()
        })
    
    scrape_pushshiftAPI <- eventReactive(input$scrape_json,
                                         {
                                             pushshift_url = paste("https://api.pushshift.io/reddit/search/submission/?",
                                                                   "before=",
                                                                   paste0(as.numeric(input$time_to),"00000"),
                                                                   "&after=",
                                                                   paste0(as.numeric(input$time_from),"00000"),
                                                                   "&size=",
                                                                   input$nposts,
                                                                   "&subreddit=",
                                                                   input$reddit,sep="")
                                             fromJSON(url(pushshift_url),flatten = T)
                                         })
    
    
    output$tableAPI <- renderDataTable({
        scrape_pushshiftAPI()$data
        
    })
    output$time_test <- renderPrint({
        
        print(as.numeric(input$time_from))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)












