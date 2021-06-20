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
create_redditData <- function(subreddit,
                              nposts=100,
                              from=NA,
                              to=NA,
                              sort_type="created_utc"){
  
  columns <- c("subreddit",
               "title",
               "author",
               "selftext",
               "id",
               "domain",
               "url",
               "created_utc",
               #"upvote_ratio",
               "score",
               "num_comments"
    
    
  )
  df = NA
  if(is.na(from) | is.na(to)){
    pushshift_url = paste("https://api.pushshift.io/reddit/search/submission/?",
                          "&size=",nposts,
                          "&subreddit=",subreddit,
                          "&sort_type=",sort_type,
                          sep="")
  }else{
    pushshift_url = paste("https://api.pushshift.io/reddit/search/submission/?",
                          "before=",
                          paste0(to),
                          "&after=",
                          paste0(from),
                          "&size=",nposts,
                          "&subreddit=",subreddit,
                          "&sort_type=",sort_type,
                          sep="")
  }
  tryCatch(
    {
      
      df = fromJSON(URLencode(pushshift_url),flatten = T)$data%>%as.data.frame()
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", pushshift_url))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    }
  )
  print(nrow(df))
  
  if(length(df)<1){
    return(NA)
  }
  if(!is.na(df)&!is.null(df))
  {
    if(columns %in% colnames(df)& nrow(df)>0){
      
      df%>%select(columns)
    }
  }else{
    print("no data available")
    NA
  }

  
}

#d <- create_redditData("programming")

fetch_subreddits <- function(subreddits,sort_type="created_utc",from=NA,to=NA){
  print(subreddits)
  l = data.frame(subreddit = subreddits,
           sort_type = rep(sort_type,length(subreddits)),
           from = rep(from,length(subreddits)),
           to = rep(to,length(subreddits)))

  rds <- list()
  for (i in 1:length(subreddits)){
    print(l[i,]$subreddit)
    rd <- create_redditData(l[i,]$subreddit,l[i,]$sort_type,l[i,]$from,l[i,]$to)
    Sys.sleep(sample(1:5, 1)/500)
    rds[[i]]<- rd
  }
  bind_rows(rds[!is.na(rds)])
}



extract_languages <- function(data){
  
    count_prop <- function(text,word){
      str_count(text,word)/length(strsplit(text," "))
    }
    # adding Ruby, C#, Javascript
    data%>%
        mutate(wordprop_java =(count_prop(selftext,"Java")+ count_prop(title,"Java"))/2)%>%
        mutate(wordprop_cpp =(count_prop(selftext,"c++")+ count_prop(title,"c++"))/2)%>%
        mutate(wordprop_python =(count_prop(selftext,"python")+ count_prop(title,"python"))/2)%>%
        mutate(wordprop_r =(count_prop(selftext,"R ")+ count_prop(title,"R ")))%>%
        mutate(wordcount_java =(str_count(selftext,"Java")+ str_count(title,"Java")))%>%
        mutate(wordcount_cpp =(str_count(selftext,"c++")+ str_count(title,"c++")))%>%
        mutate(wordcount_python =(str_count(selftext,"python")+ str_count(title,"python")))%>%
        mutate(wordcount_r =(str_count(selftext,"R ")+ str_count(title,"R ")))%>%
        mutate(languageprop = wordprop_java+wordprop_cpp+wordprop_python+wordprop_r)%>%
        mutate(languagecount = wordcount_java+wordcount_cpp+wordcount_python+wordcount_r)#%>%
       
}




fetch_redditData <- function(from,to,subreddits){
  d = difftime(from, to,
               units = c("days"))
  y <- as.POSIXct(from) +lubridate::days(1:as.numeric(d))
  times <- format(round(as.numeric(y), 3), digits = 13)
  to <- times[seq(1,length(times)-1)]
  from <- times[seq(2,length(times))]
  rdfs <- list()
  for (i in 1:length(from)){
    print(round(i/length(from),3))
    rd <- fetch_subreddits(
      subreddits,
      sort_type = "num_comments",
      from= from[i],
      to = to[i]
    )%>%
      extract_languages()
    rdfs[[i]] <- rd
  }
  bind_rows(rdfs[!is.na(rdfs)])
  
}
### TESTING
time1 = "2016-10-01"
time2 = "2016-12-31"
sreddits <- c(
      "LearnProgramming",
      "AskProgramming",
      "Programming",
      "Coding",
      "datascience",
      "MachineLearning",
      "webdev",
      "Python",
      "javascript",
      "golang",
      "ProgrammerHumor"
    )

rd <- fetch_redditData(time1,time2,subreddits=sreddits)
# 

fwrite(rd, "2016_rd.csv")


redditData <- fread("reddit_raw4.csv")
###### PLOTS

redditData <- rd
p <- redditData %>%
  filter(wordcount!=0)%>%
  ggplot(aes(x=language,y=wordcount))+
  geom_boxplot()

ggplotly(p)

#
#
ma <- function(x, n =30){stats::filter(x, rep(1 / n, n), sides = 2)}
library(stats)

redditData <- fread("reddit_raw4.csv")
colnames(redditData) <- c(colnames(redditData)[1:ncol(redditData)-1],"wordprop")


plot_wordprop <- function(redditData){
  redditData%>%
    pivot_longer(c('wordprop_java',
                   'wordprop_cpp',
                   'wordprop_python',
                   'wordprop_r'
    ),
    names_to = "language",
    values_to = "values")%>%
    filter(languageprop !=0)%>%
    mutate(date = as.POSIXct(created_utc, origin="1970-01-01"))%>%
    ggplot(aes(x=date,y=values))+
    
    #geom_point(alpha=0.2)+
    geom_smooth()+
    facet_grid(vars(language), scales="free")  
}


lda_topic_model <- function(redditData,K){
  text <- redditData$selftext
  
  corpus <- Corpus(VectorSource(text))
  #tdm <- TermDocumentMatrix(corpus)
  
  english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
  # Preprocessing chain
  message("transform to lower")
  processedCorpus <- tm_map(corpus, content_transformer(tolower))
  message("remove stopwords")
  processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
  message("remove punctuation")
  processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  message("remove numbers")
  processedCorpus <- tm_map(processedCorpus, removeNumbers)
  message("stem document")
  processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
  message("strip whitespace")
  processedCorpus <- tm_map(processedCorpus, stripWhitespace)
  message("create DTM")
  minimumFrequency <- 15
  DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
  #rowTotals <- apply(DTM , 2, sum) #Find the sum of words in each Document
  #dtm.new   <- DTM[rowTotals> 0, ]           #remove all docs without words
  #print(dtm.new)
  #LDA(dtm.new,K)
  sel_idx <- slam::row_sums(DTM) > 0
  DTM <- DTM[sel_idx, ]
  
  message("create LDA model")
  LDA(DTM,K)
}


lda_model <- lda_topic_model(redditData,4)

ap_topics <- tidy(lda_model, matrix = "beta")



plot_frequent_topics <- function(ap_topics){
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
  
}
plot_frequent_topics(ap_topics)

plot_wordcloud <- function(ap_topics, n){
  
  topicn <-ap_topics%>%
    group_by(term)%>%
    slice(which.max(beta))%>%
    as.data.frame()%>%
    filter(beta > 0.001)%>%
    filter(topic==n)
  wordcloud(topicn$term,freq = topicn$beta)
  
}


