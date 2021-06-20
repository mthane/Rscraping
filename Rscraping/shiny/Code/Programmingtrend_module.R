

trendUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(4,
           wellPanel(
            checkboxInput(ns("showPoints"),"Show points",F)
             
           )
    ),
    column(8,
           
           plotOutput(ns("redditLinePlot"))
    )
  )}

trendServer <- function(id,data) {
  moduleServer(
    id,
    function(input, output, session) {

      output$redditLinePlot <- renderPlot({
        plot_wordprop(data(),input$showPoints)
      
    })
    })
}


plot_wordprop <- function(redditData,showPoints=F){
  rd <- redditData%>%
    pivot_longer(c('wordprop_java',
                   'wordprop_cpp',
                   'wordprop_python',
                   'wordprop_r'
    ),
    names_to = "language",
    values_to = "values")%>%
    filter(languageprop !=0)%>%
    mutate(date = as.POSIXct(created_utc, origin="1970-01-01"))
  
  p<- ggplot(aes(x=date,y=values),data=rd)+
    geom_smooth()+
    facet_grid(vars(language), scales="free")
  
  if(showPoints){
    p<- p+geom_point(alpha=0.3)
  }
  p
}
