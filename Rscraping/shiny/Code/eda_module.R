boxplotUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(4,
           wellPanel(
           selectInput(ns("value"),
                       "Select y-axis",
                       choices = c("Word proportion" ="values",
                                   "Number of comments" = "num_comments",
                                   "Score" = "score"
                                               )),
           checkboxInput(ns("rm_outliers"),"Remove Outliers"),
           uiOutput(ns("dateSlider"))
           )
    ),
    column(8,
           
           plotlyOutput(ns("redditBoxplot")),
           plotlyOutput(ns("redditBoxplotSubreddit"),height=800)
    )
  )}

boxplotServer <- function(id,data){
  moduleServer(
    id,
    function(input, output, session) {
      redditProcessed <- reactive({
        df <- data() %>%
          pivot_longer(c('wordprop_java',
                         'wordprop_cpp',
                         'wordprop_python',
                         'wordprop_r'
          ),
          names_to = "language",
          values_to = "values")%>%
          filter(languageprop !=0)%>%
          mutate(date = as.POSIXct(created_utc, origin="1970-01-01"))%>%
          group_by(id)%>%
          slice_max(values)
        if(input$rm_outliers){
          df[[input$value]] <- remove_outliers(df[[input$value]])
          
        }
        df
      })
      output$dateSlider <- renderUI({
        ns <- session$ns
        dateRangeInput(ns("daterange"), "Select time",
                       start = min(redditProcessed()$date),
                       end = max(redditProcessed()$date),
                       min = min(redditProcessed()$date),
                       max = max(redditProcessed()$date))
        
      })
      
      output$redditBoxplot <- renderPlotly({
        p <-redditProcessed() %>%
          filter(date>input$daterange[1]& date<input$daterange[2])%>%
          ggplot(aes_string(x="language",y=input$value))+
          geom_boxplot()#+
          #geom_quasirandom(alpha=0.5)
        ggplotly(p)
        
      })
      output$redditBoxplotSubreddit <- renderPlotly({
        p <-redditProcessed() %>%
          
          filter(date>input$daterange[1]& date<input$daterange[2])%>%
          ggplot(aes_string(x="language",y=input$value))+
          geom_boxplot()+
          facet_wrap(vars(subreddit),nrow=4,ncol=3)
        ggplotly(p)%>%
          layout(height=800)
        
      })
      

    }
  )
  
}


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}