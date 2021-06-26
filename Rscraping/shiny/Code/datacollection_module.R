

redditDataCollectionUI <- function(id){
  ns <- NS(id)
  fluidRow(
  column(4,
         wellPanel(
         # dateInput(ns("time_from"), "From", value = Sys.time()),
         # dateInput(ns("time_to"), "To", value= Sys.time()),
         # actionButton(ns("scrape_json"),"Scrape"),
         fileInput(ns("fileReddit"),
                   "Choose CSV File",
                   multiple = FALSE,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"))
         
         )
         ),
  column(8,
         
         DTOutput(ns("tableReddit"))
         )
)}

redditDataCollectionServer <- function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        
        
        redditData <-  reactive({
          req(input$fileReddit)
          read.csv(input$fileReddit$datapath)})
        
        # redditData <- eventReactive(input$scrape_json,
        #                                {
        #                                  data <- fetch_redditData(input$time_from,
        #                                                   input$time_to,
        #                                                   c(
        #                                                     "LearnProgramming",
        #                                                     "AskProgramming",
        #                                                     "Programming",
        #                                                     "Coding",
        #                                                     "datascience",
        #                                                     "MachineLearning",
        #                                                     "webdev",
        #                                                     "Python",
        #                                                     "javascript",
        #                                                     "golang",
        #                                                     "ProgrammerHumor"
        #                                                   )
        #                                                   )
        #                                  data
        #                                })


        output$tableReddit <- renderDataTable({
          req(redditData())
              redditData()%>%
                select(subreddit,title,selftext,id,created_utc,num_comments,score)
            
            #formatStyle( 0, target= 'row',color = 'black', backgroundColor = 'yellow',
            #             fontWeight ='bold', lineHeight='20%')
        },
        options = list(scrollX = TRUE,pageLength = 5,autowidth=TRUE))
        
        redditData 
      }
  )
  
  
}


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

