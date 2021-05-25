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


create_redditData <- function(subreddit,
                              nposts=100,
                              from=NA,
                              to=NA,
                              sort_type="created_utc"){
  
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
  
  
  fromJSON(url(pushshift_url),flatten = T)$data%>%
    select("subreddit",
            "title",
            "author",
            "selftext",
            "id",
            "domain",
            "url",
            "created_utc",
            #"upvote_ratio",
            "score",
            "num_comments",
            "subreddit_subscribers"
     )
  
}

d <- create_redditData("programming")

fetch_subreddits <- function(subreddits,sort_type="created_utc",from=NA,to=NA){
  
  l = list(subreddit = subreddits,
           sort_type = rep(sort_type,length(subreddits)),
           from = rep(from,length(subreddits)),
           to = rep(to,length(subreddits)))
  pmap(l,create_redditData)%>%bind_rows()

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


# redditData <- fetch_subreddits(
#   c(
#     "LearnProgramming",
#     "AskProgramming",
#     "Programming",
#     "Coding",
#     "datascience",
#     "MachineLearning"
#    ),sort_type = "num_comments"
#   )%>%
#   extract_languages()


#
# to = paste(seq(0,3000,10),"d",sep="")
# from = paste(seq(10,3010,10),"d",sep="")
# rdfs <- list()
# 
# for (i in 1:length(from)){
#   Sys.sleep(sample(1:5, 1)/100)
#   print(from[i])
#   rd <- fetch_subreddits(
#     c(
#       "LearnProgramming",
#       "AskProgramming",
#       "Programming",
#       "Coding",
#       "datascience",
#       "MachineLearning",
#       "rprogramming",
#       "Python"
#     ),sort_type = "num_comments",
#     from= from[i],
#     to = to[i]
#   )%>%
#   extract_languages()
#   rdfs[[i]] <- rd
# }


redditData <- fread("reddit_raw4.csv")
###### PLOTS


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
    filter(wordprop!=0)%>%
    mutate(date = as.POSIXct(created_utc, origin="1970-01-01"))%>%
    ggplot(aes(x=date,y=values))+
    
    geom_point(alpha=0.2)+
    geom_smooth()+
    facet_grid(vars(language), scales="free")
  
  
  
}

plot_wordprop(redditData)


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


