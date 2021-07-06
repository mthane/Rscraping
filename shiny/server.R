#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(lubridate)
library(forecast)
library(tidyverse)
library(tidymodels)
library(kableExtra)

library(jsonlite)
library(wordcloud) 
library(tm)
library(vistime)
library(stringr)

library(wordcloud) 

library(vistime)
library(stringr)

library(data.table)
library(topicmodels)
library(jsonlite)

library(ggplot2)
library(stringr)
library(data.table)
library(plotly)
library(tidytext)
library(lubridate)
library(RCurl)
library(ggcorrplot)
library(corrr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

 
  
    ##### ----------------Overview------------------
    output$researchquelist <- renderUI(HTML("<ul>
                                    <li>Can we decide which topic/language a certain post is about?</li>
                                   <li>How do number of upvotes, comments and number of posts correlate to popularity?</li>
                                   <li>How does the popularity of programming languages change over time?</li>
                                   <li>Can we predict the popularity of programming languages in the future?</li>
                                   <li>How do the two platforms compare based on programming languages?</li>
                                   </ul>"))
    
    
    ##### ----------------Datasets------------------------------
    redditData <- reactive({
      readRDS("data/reddit_data16-20.Rds")
      
    })
    
    stackData <- reactive({
      readRDS('data/stack_data_final.rds')%>%
        mutate(prog = case_when(str_detect(Tags, stringr::fixed("<c++>")) ~ "CPP",
                                str_detect(Tags, "<python>") ~ "Python",
                                str_detect(Tags, "<r>") ~ "R",
                                str_detect(Tags, "<java>") ~ "Java",
                                str_detect(Tags, "<javascript>")~ "JavaScript",
                                str_detect(Tags, "<c>")~ "C",
                                str_detect(Tags, "<ruby>")~ "Ruby",
                                str_detect(Tags, "<php>")~ "PHP",TRUE ~ Tags)) %>% 
        filter(prog %in% c("Python","R", "Java","JavaScript", "C","CPP","Ruby","PHP")) %>%
        mutate(year = year(CreationDate)) %>% filter (year %in% c("2016","2017","2018","2019","2020")) %>%
        mutate(text_new = paste(Body,Title,sep=" "))%>%
        mutate(text_new =  tolower(text_new))
    })
    
    output$redditdatashow <- renderDataTable({
        rd <-redditData()%>%
            mutate(date=as.POSIXct(created_utc, origin="1970-01-01"))%>%
            select(!c("values","language"))%>%
            filter(date > input$datasetdaterange[1] & date< input$datasetdaterange[2])
        datatable(head(rd,20),style="bootstrap",options = list(scrollX=TRUE, scrollCollapse=TRUE))
       
    })
    
    
    output$stackoverflowdatashow <- function(){
        stack_data <- stackData()%>%
            mutate(date_temp=as.POSIXct(CreationDate, origin="1970-01-01"))%>%
            filter(date_temp >input$datasetdaterange[1] & date_temp<input$datasetdaterange[2])
            
        datatable(head(stack_data,20),style="bootstrap",options = list(scrollX=TRUE, scrollCollapse=TRUE))
    }
    
    
    ##### ---------------------------EDA Reddit---------------------------------
    
    ##########REDDIT EDA###############
    # For EDA-------------------------------------------------------

    #Average Comment cOunt Per Programming Language Filtered by Year
    output$plot_comment_count_reddit <- renderPlot({
      
      plot_comment_count_r <- redditData()%>%
        group_by(language)%>%
        dplyr::summarise(
          comments = sum(num_comments)/n())%>%
        ggplot(aes(x=reorder(language,-comments),y=comments))+
        geom_col(fill='#FF5700',alpha=0.7)+
        labs(title="Average number of comments per post for all languages",
             y = "Number of comments",
             x = "Programming Language"
        )+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90))
      plot_comment_count_r
    })
    
    
    #Average number of comments per post for all languages and subreddits
    output$plot_comment_count_subreddit <- renderPlot({
      
      plot_comment_comments_sb <- redditData()%>%
        group_by(language,subreddit)%>%
        dplyr::summarise(
          comments = sum(num_comments)/n())%>%
        ggplot(aes(x=reorder(language,-comments),y=comments))+
        geom_col(fill='#FF5700',alpha=0.7)+
        labs(title="Average number of comments per post for all languages and subreddits",
             y = "Number of comments",
             x = "Programming Language"
        )+
        facet_wrap(vars(subreddit))+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90))
      plot_comment_comments_sb
      
    })
    
    #A Text length analysis for different languages
    output$plot_textlength_reddit <- renderPlot({
      
      temp <- redditData()
      tlrd<-data.frame(temp$language)
      tlrd$language <- temp$language
      names<-("language")
      tlrd$text_length <- nchar(as.character(temp$text_new))
      plot_textlength_r <- ggplot(tlrd, aes(x=language, y=text_length)) +
        geom_boxplot(outlier.shape = NA,fill='#FF5700') +
        scale_y_continuous(limits = quantile(tlrd$text_length, c(0.1, 0.9))) + 
        stat_summary(fun=mean, geom="point", shape=5, size=1)+
        theme_bw()
      plot_textlength_r
      
    })
    
    ldReddit <- reactive({
      # extracting the language data
      
      count_prop <- function(text,word){
        str_count(text,word)/length(strsplit(text," "))
      }
      # adding Ruby, C#, Javascript
      ld <- redditData()%>%
        mutate(text_new = paste(selftext,title,sep=" "))%>%
        mutate(text_new =  tolower(text_new))%>%
        mutate(java =count_prop(text_new," java "))%>%
        mutate(python =count_prop(text_new,"python"))%>%
        mutate(r =count_prop(text_new," r "))%>%
        mutate(javascript =count_prop(text_new,"javascript "))%>%
        mutate(c =(count_prop(text_new," c ")))%>%
        mutate(cpp = count_prop(text_new,stringr::fixed("c++ ")))%>%
        
        
        mutate(ruby =count_prop(text_new," ruby "))%>%
        mutate(php =count_prop(text_new," php "))
      
      
      
    })
    
    MReddit <- reactive({
      
      cm <- ldReddit() %>%
        select(java,python,r,javascript,c,cpp,ruby,php)
      cm[,-1] 
    })
    
    output$redditCorrplot <- renderPlot({
      
      p.mat <- cor_pmat(MReddit())
      ggcorrplot(cor(MReddit(),method = 'pearson'), p.mat = p.mat ,hc.order=T,type='full',lab=T)
      
    })
    
    output$redditNetwork <- renderPlot({
      MReddit()%>%correlate(method="pearson")%>%
        network_plot(min_cor=0,curved=T, colors = c("red", "green"))
      
    })
    
    #Change in Post COunt Per Programming Language by Year
    output$plot_time_seriesReddit <- renderPlot({
      
      ts<- redditData() %>% 
        mutate(date = as.POSIXct(created_utc, origin="1970-01-01"))%>%
        mutate(year = year(date))%>% 
        filter (year %in% c("2016","2017","2018","2019","2020"))%>%
        group_by(year,language) %>%  dplyr::summarise(post = n())
      ts <- data.frame(ts)
      
      ts<-ts[order(ts$year),] 
      
      count_vec <- function(lang) {
        ts %>%
          filter(language == lang) %>% select(post) %>% unlist()
      }
      
      
      python_vec <- count_vec("python")
      r_vec <- count_vec("r")
      java_vec <- count_vec("java")
      javascript_vec <- count_vec("javascript")
      c_vec <- count_vec("c")
      c_plus_vec <- count_vec("cpp")
      # no data for ruby in 2020
      ruby_vec <- c(count_vec("ruby"),NA)
      php_vec <- count_vec("php")
      
      data <- data.frame(
        year = 2016:2020,
        Python = python_vec,
        R = r_vec,
        Java = java_vec,
        JavaScript = javascript_vec,
        C = c_vec,
        CPP = c_plus_vec,
        Ruby = ruby_vec,
        PHP = php_vec
      )
      
      data_long <- melt(data, id.vars = "year")
      
      ggplot(data_long, aes(x = year,y = value,col = variable)) +
        geom_line() + 
        geom_point() + 
        scale_fill_discrete(labels = c("Python", "R", "Java", "JavaScript", "C", "CPP", "Ruby", "PHP")) +
        xlab("Year") + ylab("Total Posts")+
        theme_bw()
      # labs(title = "Change in Number of Posts by Year",
      #      subtitle = "Total Posts per Programming Language vs Year") +
    })
    
    
    ##### ---------------------------EDA Stackoverflow---------------------------------
    

    #Average Comment cOunt Per Programming Language Filtered by Year
    output$plot_comment_count <- renderPlot({
      
      colbar <- stackData() %>% filter(year == input$comm_year) %>%
        group_by(prog) %>%
        dplyr::summarise(avg = sum(AnswerCount) / n())
      
      plot_comment_count<- ggplot(colbar,aes(x=reorder(prog,-avg),y=avg)) +
        geom_col(fill="#bcbbbb")  +
        xlab("Programming Languages")+
        ylab("Average Comment Count of Posts") +
        theme(axis.text.x = element_text(angle=65, vjust=0.6))+
        theme_bw()
      
      plot_comment_count 
    })
    
    #Average Answers cOunt Per Programming Language Filtered by Year
    output$plot_answered_posts <- renderPlot({
      
      lollypop <- stackData() %>% filter(year == input$stk_year) %>%
        group_by(prog) %>%
        dplyr::summarise(avg = sum(AnswerCount) / n()) 
      
      plot_answered_posts <-
        ggplot(lollypop, aes(x = reorder(prog,-avg), y = avg)) +
        geom_col(fill = "#bcbbbb")+
        xlab("Programming Language") + ylab("Average Answered Count of Posts") +
        theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) 
      plot_answered_posts
    })
    #Yearly User Views Per Programming Language by Year
    output$plot_user_view <- renderPlot({
      
      user_view <- stackData() %>%
        group_by(year, prog) %>%
        dplyr::summarise(avg = sum(ViewCount) / n()) 
      
      plot_user_view<-ggplot(user_view,aes(x = reorder(year,-avg),y = avg)) +
        geom_bar(stat='identity',fill="#bcbbbb") +
        facet_wrap(~prog,nrow=2) +
        labs(x = "Year", y = "Average View Count of Posts") +  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
        theme_bw()
      
      plot_user_view
    })
    
    
    #Change in Post COunt Per Programming Language by Year
    output$plot_time_series <- renderPlot({
      
      ts<- stackData() %>% group_by(year,prog) %>%  dplyr::summarise(post = n())
      ts <- data.frame(ts)
      
      ts<-ts[order(ts$year),] 
      
      count_vec <- function(lang) {
        ts %>%
          filter(prog == lang) %>% select(post) %>% unlist()
      }
      
      
      python_vec <- count_vec("Python")
      r_vec <- count_vec("R")
      java_vec <- count_vec("Java")
      javascript_vec <- count_vec("JavaScript")
      c_vec <- count_vec("C")
      c_plus_vec <- count_vec("CPP")
      ruby_vec <- count_vec("Ruby")
      php_vec <- count_vec("PHP")
      
      
      data <- data.frame(
        year = 2016:2020,
        Python = python_vec,
        R = r_vec,
        Java = java_vec,
        JavaScript = javascript_vec,
        C = c_vec,
        CPP = c_plus_vec,
        Ruby = ruby_vec,
        PHP = php_vec
      )
      
      data_long <- melt(data, id.vars = "year")    
      
      plot_time_series <- ggplot(data_long,
                                 aes(x = year,
                                     y = value, col = variable)) +
        geom_line() + geom_point() + scale_fill_discrete(labels = c      ("Python", "R", "Java", "JavaScript", "C", "CPP", "Ruby", "PHP")) +
        xlab("Year") + ylab("Total Posts")+
        theme_bw()
      # labs(title = "Change in Number of Posts by Year",
      #      subtitle = "Total Posts per Programming Language vs Year") +
      
      
      plot_time_series
    })
    
    #Finding Corrleation between Programming Languages
    corr_reactive <- reactive({
      
      count_prop <- function(text, word) {
        str_count(text, word) / length(strsplit(text, " "))
      }
      # adding Ruby, C#, Javascript
      ld <- stackData() %>%
        mutate(text_new = paste(Body, Title, sep = " ")) %>%
        mutate(text_new =  tolower(text_new)) %>%
        mutate(java = count_prop(text_new, " java ")) %>%
        mutate(python = count_prop(text_new, "python")) %>%
        mutate(r = count_prop(text_new, " r ")) %>%
        mutate(javascript = count_prop(text_new, "javascript ")) %>%
        mutate(c = (count_prop(text_new, " c "))) %>%
        mutate(cpp = count_prop(text_new, stringr::fixed("c++ "))) %>%
        mutate(ruby = count_prop(text_new, " ruby ")) %>%
        mutate(php = count_prop(text_new, " php "))
      
      cm <- ld %>%
        select(java, python, r, javascript, c, cpp, ruby, php)
      M <- cm
      M
      
    }) 
    output$corr_plot <- renderPlot({
      
      
      
      corr_plot <-
        ggcorrplot(
          cor(corr_reactive(), method = 'pearson') ,
          hc.order = T,
          lab = TRUE,
          type = 'full'
        )
      corr_plot
      
      
    })
    
    output$remove_corr <- renderPlot({
      
      
      p.mat <- cor_pmat(corr_reactive())
      remove_corr <-
        ggcorrplot(
          cor(corr_reactive(), method = 'pearson'),
          p.mat = p.mat ,
          hc.order = T,
          type = 'full',
          lab = T
        )
      remove_corr
      
    })
    
    
    
    #Network Plot for correlation for programming Languages
    output$network_plt <- renderPlot({
      
      
      p.mat <- cor_pmat(corr_reactive())
      network_plt <- corr_reactive() %>% correlate(method = "pearson") %>%
        network_plot(
          min_cor = 0,
          curved = T,
          colors = c("red", "green")
        )
      network_plt
      
    })
    output$plot_text_length <- renderPlot({
      stack_data <- stackData()
      stack_data$text_new <- paste(stack_data$Title, " ", stack_data$Body)
      tlso = data.frame(stack_data$prog)
      tlso$language <- stack_data$prog
      names<-("language")
      tlso$text_length <- nchar(as.character(stack_data$text_new))
      plot_text_length <- ggplot(tlso, aes(x=language, y=text_length)) +
        geom_boxplot(outlier.shape = NA, fill="#bcbbbb") +
        scale_y_continuous(limits = quantile(tlso$text_length, c(0.1, 0.9))) + stat_summary(fun=mean, geom="point", shape=5, size=1)+
        theme_bw()
      
      plot_text_length
    })    
    
    
    ##### ---------------------------Forecasting---------------------------------
    
    # Reddit
    forcasting_predataReddit <- reactive({
      data <- redditData()
      
      data$date <- format(as.Date(as.POSIXct(data$created_utc, origin="1970-01-01")), "%Y-%m")
      r<- data %>% group_by(date,language) %>%  dplyr::summarise(post = n())
      k<- data %>% group_by(date) %>%  dplyr::summarise(total_post = n())
      total <- merge(r,k,by="date") %>%  mutate(fraction = round(post/total_post, 2))
      total
    })
    
    
    output$forcastingReddit <- renderPlot({
      total_temp <- forcasting_predataReddit()
      py <- filter(total_temp,language == input$languageReddit)
      py <- ts(py$fraction, frequency = 12, start = c(2016, 1), end=c(2020, 1))
      fit <- Arima(window(py, end=round(as.numeric(input$yearEndReddit))),c(1,2,2))
      test <- forecast(fit,h=input$monthReddit)
      
      autoplot(test)+
        ylab("Point Forecast")+
        theme_bw()
      
    })
    # Stackoverflow
    forcasting_predataStack <- reactive({
      data <- stackData()
     
      data$date <- format(as.Date(as.POSIXct(data$CreationDate, origin="1970-01-01")), "%Y-%m")
      r<- data %>% group_by(date,prog) %>%  dplyr::summarise(post = n())
      k<- data %>% group_by(date) %>%  dplyr::summarise(total_post = n())
      total <- merge(r,k,by="date") %>%  mutate(fraction = round(post/total_post, 2))
      total
    })
    
    
    output$forcastingStack <- renderPlot({
      total_temp <- forcasting_predataStack()
      py <- filter(total_temp,prog == input$languageStack)
      py <- ts(py$fraction, frequency = 12, start = c(2016, 1), end=c(2020, 1))
      fit <- Arima(window(py, end=input$yearEndStack),c(1,2,2))
      test <- forecast(fit,h=input$monthStack)
      
      autoplot(test)+
        ylab("Point Forecast")+
        theme_bw()
      
    })
    
    ##### ---------------------------Topic Modeling---------------------------------
    
    # Reddit
    topicModelReddit <- eventReactive(input$startReddit,{
      df <- redditData()%>%
        filter(language==input$languageRedditTM)
      df <- df[sample(nrow(df), round(input$sampleReddit*nrow(df))), ]
      lda_topic_model(df,input$KReddit)
    })
    
    output$wordcloudReddit <- renderPlot({
      dat <- topicModelReddit()$dat
      wordcloud(words = dat$word, freq = dat$freq, min.freq = 20,
                max.words=100, random.order=FALSE, rot.per=0.1,
                colors=brewer.pal(8, "Dark2"))
      
    })
    
    output$topicsReddit <- renderPlot({
      ap_topics <- tidy(topicModelReddit()$model, matrix = "beta")
      plot_frequent_topics(ap_topics,input$nwordsReddit)
      
      
    })
    
    # Stackoverflow
    topicModelStack <- eventReactive(input$startStack,{
      df <- stackData()%>%
        filter(prog==input$languageStackTM)
      
      df <- df[sample(nrow(df), round(input$sampleStack*nrow(df))), ]
      lda_topic_model(df,input$KStack)
    })
    
    output$wordcloudStack <- renderPlot({
      dat <- topicModelStack()$dat
      wordcloud(words = dat$word, freq = dat$freq, min.freq = 20,
                max.words=200, random.order=FALSE, rot.per=0.1,
                colors=brewer.pal(8, "Dark2"))
      
    })
    
    output$topicsStack <- renderPlot({
      ap_topics <- tidy(topicModelStack()$model, matrix = "beta")
      plot_frequent_topics(ap_topics,input$nwordsStack)
      
      
    })
    
    
    })


lda_topic_model <- function(data,K){
  
  
  #Create a vector containing only the text
  led <- data
  text <- led %>% select(text_new)
  # Create a corpus  
  docs <- Corpus(VectorSource(text$text_new))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation)%>%
    tm_map(removeNumbers)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  memory.limit()
  gc()
  tdm <- TermDocumentMatrix(docs)
  
  dtm <- DocumentTermMatrix(docs)
  
  dtm <- removeSparseTerms(dtm,0.97)
  
  sel_idx <- slam::row_sums(dtm) > 0
  dtm <- dtm[sel_idx, ]
  
  
  matrix <- as.matrix(tdm) 
  f <- sort(rowSums(matrix),decreasing=TRUE)
  dat <- data.frame(word = names(f),freq=f)
  
  message("create LDA model")
  m = model = LDA(dtm,K)
  list(dtm = dtm,model = m,dat = dat)
}

#

plot_frequent_topics <- function(ap_topics,n=20){
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    slice_max(beta, n =n) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()+theme_bw()
  
}


