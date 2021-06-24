##Source files

source("shiny/Code/requirements.R")
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