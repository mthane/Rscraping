#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)
library(vembedr)


# Define UI for application that draws a histogram
shinyUI(navbarPage(theme= shinytheme("superhero"), title = "Programming Language Trends",
                   tabPanel("Home",
                            fluidPage(
                              tags$head(
                                tags$link(rel = "stylesheet", type = "text/css", href = "home.css")),
                                p(class="line-1 anim-typewriter","Trends in Programming Language Popularity()",style="margin-top: 1%;"),
                              div(img(src="homeui.png",height=300,width=450),style="text-align: center; margin-top: 5%;"),
                              
                              div(embed_youtube("qkKfiqyzYlM"),style="tex-align: center; margin-left: 36%;margin-top:7%;"),
                              
                              tags$footer( HTML("<footer><small><b>&copy; Developed with ❤️ by Indrani, Indranil, Micheal, Sharanya.</b></small></footer>"), align="center", style="position:absolute; bottom:0; width:95%; height:50px; color: #FFFFFF; padding: 0px; background-color: transparent; z-index: 1000;margin-left:1.5%;")
                              
                              )
                            
                            ),
                   tabPanel("Overview",fluidPage(
                       titlePanel(""),
                       
                       navlistPanel(
                           "Overview",
                           tabPanel("Introduction",
                                    h1("Introduction"),
                                    p("Reddit and Stackoverflow are two community based websites which are used heavily by people all across the world. Reddit is known as the “Front Page of the Internet” and is a popular forum especially among young people where users can post anything and everything. It has a large international community and a lot of programming related content. Reddit is a place for all possible topics of discussion whereas Stack Overflow is centered around programming languages, ideas and discussions surrounding it. Stack overflow is more of a question answer based system. Reddit is mostly a post and comments based system. Both of these websites have an upvote/downvote system and are great resources for coders and programmers around the globe. "),
                                    div(img(src="reddit.png",height=400,width=800),style="text-align: center;"),
                                    h4("Reddit Interface",align="center"),
                                    div(img(src="stackoverflow.png",height=400,width=800),style="text-align: center;"),
                                    h4("StackOverFlow Interface",align="center")),
                           tabPanel("Motivation",
                                    h1("Motivation"),
                                    p("We want to use data from the Reddit forum in order to better understand the popularity of Programming languages among Reddit users.Additionally, we want to compare it to data from the Stack Overflow forum. We want to evaluate what programming languages are being discussed in both forums and compare how their usage or popularity has changed over time. For reaching our aim we want to use Visualization and Machine Learning methods based on text data but also use the quantification that we get from the upvotes and number of comments on both the platforms.

Since both platforms are very popular and most sought after and contain a lot of discussion related to programming, it would be good to study and analyse the trend of how users have been using some of the topmost programming languages. The change of trend would help us in understanding  how the popularity of programming language has changed over time and also we could then predict which programming language would be centre of discussion or most queried of in these two platforms in near future. ")
                                    ),
                           tabPanel("Objective",
                                    h1("Objective"),
                                    p("The objective of our project is to find a correlation between the different parameters of  questions or posts in these platforms and try to calculate how the popularity of programming language has changed over time. And then further we would like to predict what would be the trends of these programming languages in near future. We could use this information to also relate the kinds of problems or topics which are most related with this programming languages and analyze the kind of topics or solutions most used for certain kinds of problems. We would try to answer the following research questions :"),
                                    uiOutput("researchquelist")
                                    ),
                           tabPanel("Related Work",
                                    h1("Related Work"),
                                    tags$ul(
                                        tags$li("The growth of R programming language blog by stackoverflow",tags$a(href="https://stackoverflow.blog/2017/10/10/impressive-growth-r/","Link")),
                                        tags$li("The growth of Python programming language blog by stackoverflow",tags$a(href="https://stackoverflow.blog/2017/09/06/incredible-growth-python/","Link")),
                                        tags$li("The PYPL indexing for programming language",tags$a(href="https://pypl.github.io/PYPL.html","Link")),
                                        tags$li("The TIOBE indexing for programming language",tags$a(href="https://www.tiobe.com/tiobe-index/","Link")),
                                        p(),
                                        p("The first 2 blogs are a very detailed analysis of how the popularity of programming languages like R and Python grew with time. It has a very intuitive and analytical approach and also gives us data analysis results via usage of meaningful curves which show how questions related to programming languages have been viewed or in which sectors of industry or which domain they have influenced more with a clear comparative analysis. They also give us insights into which packages of a programming language has been used more or even future predictive analysis of how the usage of the programming language will be in the near future."),
                                        p("The PYPL indexing for programming languages is based on google trends of how often problems , questions and queries are made related to certain programming languages and it also has a wonderful dynamic visualisation for easy comparison of the changing trends.
The TIOBE indexing is also another related work which analyses count of web pages involving the programming keywords to decide their popularity and changing trend.")
                                        
                                        
                                    ),
                                    
                                    h3("Related work for Topic Modelling"),
                                    p("Topic modeling is an unsupervised learning approach, which mainly helps in identifying hidden relationships or themes in the data. Highly useful to understand the underlying connection within the data especially in cases of larger corpus and when there is no labeled data. With no prior knowledge about the themes, it uses probabilistic framework to make sense of the data based on the count and context of the words used in the text documents. It is often a good technique for exploratory analysis of text data through which documents/text in the data can be grouped for other downstream tasks of analysis, classification, etc.

In the case of our project, we intend to use topic modeling to pick out themes within already separated groups of reddit and stackoverflow posts.  The Reddit and StackOverflow posts are initially grouped into multiple sets of posts categorized based on the programming language used in the post. This grouping into different sets is handled via the tags in case of stackoverflow posts and via search pattern recognistion in case of reddit posts. Once they, seperated into individual groups of programming languages, we perform Latent Dirichlet Allocation(LDA) on each of these sets to help us identify the keywords, common themes across all of these different sets.
LDA uses generative probabilistic modeling and Dirichlet distributions to infer possible themes based on the words used in the document sets.
Through this, we are aiming to identify and visualize the word cloud of each of the programming language sets and what makes them similar or distinctive to each other."),
                                    tags$a(href="https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25","Towards DataScience Blog"),
                                    tags$a(href="https://slcladal.github.io/topicmodels.html","LDA Tutorial"),
                                    tags$a(href="https://www.researchgate.net/profile/Solomia-Fedushko/publication/331276764_Proceedings_of_the_Sixth_International_Conference_on_Computer_Science_Engineering_and_Information_Technology_CCSEIT_2016_Vienna_Austria_May_2122_2016/links/5c6fcd63299bf1268d1bc2b0/Proceedings-of-the-Sixth-International-Conference-on-Computer-Science-Engineering-and-Information-Technology-CCSEIT-2016-Vienna-Austria-May-2122-2016.pdf#page=212","Paper")
                                    
                                    )
                           
                       )
                       
                   )),
                   tabPanel("Datasets",
                           fluidPage(title="",sidebarPanel(dateRangeInput("datasetdaterange", "Datasets for years 2016-2020:",
                                                                 start  = "2016-01-01",
                                                                 end    = "2016-01-08",
                                                                 min    = "2016-01-01",
                                                                 max    = "2020-12-31",
                                                                 format = "mm/dd/yy",
                                                                 separator = " - ")),
                                     mainPanel(
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Reddit Data", DTOutput("redditdatashow")),
                                                   tabPanel("Stackoverflow Data", DTOutput("stackoverflowdatashow"))
                                       
                                     ))
                          
                            )),
                   tabPanel("Exploratory Data Analysis",
                            fluidPage(
                     titlePanel(""),
                     tabsetPanel(type ="tabs",
                                 tabPanel("Reddit",
                                          
                                          navlistPanel("Exploring Reddit",
                                             tabPanel("Average Comment Count",
                                                      fluidPage(
                                                        plotOutput("plot_comment_count_reddit"))),
                                             tabPanel("Average Comment Count for Subreddits",
                                                      fluidPage(
                                                        plotOutput("plot_comment_count_subreddit"))),
                                             tabPanel("Text Length Analysis",
                                                      fluidPage(
                                                        plotOutput("plot_textlength_reddit"))),
                                             
                                             tabPanel("Change in Post Count With Time",
                                                      
                                                      plotOutput("plot_time_seriesReddit")
                                                      ),
                                             tabPanel("Correlation between Languages",
                                                      fluidPage(
                                                        fluidRow(
                                                          plotOutput("redditCorrplot")
                                                      ))),
                                             tabPanel("Correlation Network",
                                                      fluidPage(
                                               
                                               plotOutput("redditNetwork")))
                                             
                                             )),
                                 # ---------------------EDA Stack Overflow ------------------
                                 tabPanel("Stack Overflow",
                                          navlistPanel(
                                            "Exploring Stackoverflow",
                                            # Average Comment Count Per Programming Language
                                            tabPanel("Average Comment Count by Users"
                                                     ,fluidPage(
                                                       sidebarLayout(          
                                                         sidebarPanel(
                                                           selectInput("comm_year", 
                                                                       "Choose Year:",
                                                                       choices = c("2016" ,"2017","2018","2019","2020")
                                                                       ,selected ="2020")) ,
                                                         mainPanel(
                                                           plotOutput("plot_comment_count"))))),
                                            
                                            # Average Answered Posts Per Programming Language
                                            tabPanel("Average Answer Count by Users"                                                 ,fluidPage(
                                              sidebarLayout(          
                                                sidebarPanel(
                                                  selectInput("stk_year", 
                                                              "Choose Year:",
                                                              choices = c("2016" ,"2017","2018","2019","2020"),
                                                              selected ="2020")) ,
                                                mainPanel(
                                                  plotOutput("plot_answered_posts"))))), 
                                            
                                            
                                            # Change in User View Count Per Programming Language over Years          
                                            tabPanel("Average User Views Per Post "                             
                                                     ,fluidPage(
                                                       plotOutput("plot_user_view"))), 
                                            tabPanel("Text Length of Posts"                             
                                                     ,fluidPage(
                                                       plotOutput("plot_text_length"))),
                                            
                                            # Change in Posts Count Over Time
                                            tabPanel("Change in Post Count With Time"                                                 ,fluidPage(
                                              
                                              plotOutput("plot_time_series"))),
                                            # Change in Posts Count Over Time
                                            tabPanel("Correlation between Languages",
                                                     fluidPage(
                                                       fluidRow(
                                                         column(width = 6,
                                                                plotOutput("corr_plot")
                                                         ),
                                                         column(width = 6,
                                                                plotOutput("remove_corr"))
                                                       )
                                                     )),
                                            tabPanel("Correlation Network"                                                               ,fluidPage(
                                              
                                              plotOutput("network_plt")))
                                          )) #Stack Overflow ends
                                 
                     ))),
                   
                   tabPanel("Forecasting",
                            
                            tabsetPanel(type = "tabs",
                                        tabPanel("Reddit Data", 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     sliderInput("yearEndReddit",
                                                                 "Predict using data until",
                                                                 min= 2017,max = 2020,value=2020,
                                                                 sep = ""),
                                                     selectInput("languageReddit", "Programming Language:",
                                                                 
                                                                 choices = c(
                                                                   "Python" = "python",
                                                                   "Java" = "java",
                                                                   "JavaScript" = "javascript",
                                                                   "C++" = "cpp",
                                                                   "PHP" = "php",
                                                                   "C" = "c",
                                                                   "Ruby" = "ruby",
                                                                   "R" = "r"
                                                                 )),
                                                     sliderInput("monthReddit","Number of Months to Predict",min=6,max = 36,value=12,step=12),
                                                     
                                                     
                                                     
                                                   ),
                                                   mainPanel(
                                                     
                                                     plotOutput("forcastingReddit")
                                                   )
                                                   
                                                 )
                                                
                                                 
                                                 ),
                                        tabPanel("Stackoverflow Data", 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     sliderInput("yearEndStack",
                                                                 "Predict using data until",
                                                                 min= 2017,max = 2020,value=2020,
                                                                 sep = ""),
                                                     selectInput("languageStack", "Programming Language:",
                                                                 
                                                                 choices = c(
                                                                   "Python" = "Python",
                                                                   "Java" = "Java",
                                                                   "JavaScript" = "JavaScript",
                                                                   "C++" = "CPP",
                                                                   "PHP" = "PHP",
                                                                   "C" = "C",
                                                                   "Ruby" = "Ruby",
                                                                   "R" = "R"
                                                                 )),
                                                     sliderInput("monthStack","Number of Months to Predict",min=6,max = 36,value=12,step=12),
                                                     
                                                     
                                                   ),
                                                 mainPanel(
                                                   
                                                   plotOutput("forcastingStack")
                                                   
                                                 )
                                                 
                                                 )
                                                 )
                              
                            )
                            
                            
                   ),
                   
                   
                   tabPanel("Topic Modelling",
                            tabsetPanel(
                              tabPanel("Reddit",
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           wellPanel(
                                             selectInput("languageRedditTM",label = "Select language",
                                                         choices = c(
                                                           "Python" = "python",
                                                           "Java" = "java",
                                                           "JavaScript" = "javascript",
                                                           "C++" = "cpp",
                                                           "PHP" = "php",
                                                           "C" = "c",
                                                           "Ruby" = "ruby",
                                                           "R" = "r"
                                                         )),
                                             numericInput("KReddit","Number of topics",value = 2,2,30),
                                             sliderInput("sampleReddit","Sample fraction", 0,1,0.1),
                                             
                                             actionButton("startReddit", "Create model"),
                                             
                                             sliderInput("nwordsReddit","Top N words", 3,15,5)
                                           )
                                           
                                         ),
                                         mainPanel(
                                           
                                           plotOutput("wordcloudReddit"),
                                           
                                           plotOutput("topicsReddit"),
                                         )
                                       )  
                                
                                
                              ),
                              tabPanel("Stackoverflow",
                                       sidebarLayout(
                                         sidebarPanel(
                                           wellPanel(
                                             selectInput("languageStackTM",label = "Select language",choices = c(
                                               "Python" = "Python",
                                               "Java" = "Java",
                                               "JavaScript" = "JavaScript",
                                               "C++" = "CPP",
                                               "PHP" = "PHP",
                                               "C" = "C",
                                               "Ruby" = "Ruby",
                                               "R" = "R"
                                             )),
                                             numericInput("KStack","Number of topics",value = 2,2,30),
                                             sliderInput("sampleStack","Sample fraction", 0,1,0.1),
                                             
                                             actionButton("startStack", "Create model"),
                                             
                                             sliderInput("nwordsStack","Top N words", 3,15,5)
                                           )
                                           
                                         ),
                                         mainPanel(
                                           
                                           plotOutput("wordcloudStack"),
                                           
                                           plotOutput("topicsStack"),
                                         )
                                       )
                                       
                                       )
                              
                              
                            )
                            
                            
 
                            
                            
                   ),
                   tabPanel("Final Analysis"),
                   
                   
                   navbarMenu("Resources",
                              tabPanel(tags$a(href="https://www.google.com/","R Markdown Process Notebook",target="_blank")),
                              tabPanel(tags$a(href="https://github.com/mthane/Rscraping","Github Repository",target="_blank")),
                              tabPanel(tags$a(href="https://www.dropbox.com/sh/qxgrwp2wyae8i3x/AADNAv3jR6gOaKonMNWt94Kia?dl=0","Datasets",target="_blank")),
                              tabPanel("References")
                              )
                   
                   
                   ))
