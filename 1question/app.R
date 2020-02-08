library(ggplot2)
library(shiny)

message("reading and loading of the dataset,\n this operation can take sometimes the datasets are very big.")

if(!exists("accidents")){
  message("wrangling data...")
  source("../wrangling.R")
}

accidents <- na.omit(accidents)
#summary(accidents)
#accidents <- sample_n(accidents,5000)

accidents$Time <- factor(substr(accidents$Time, 0, 2))
 
all <- "all"

ui <- fluidPage(
  titlePanel(title=h4("In which moment of the day/week are there a greater number of accidents?", align="center")),
  sidebarPanel( 
    selectInput("year", "Choose a year",
                c(all,levels(factor(accidents$year)))
    ),
    selectInput("month", "Choose a month",
                c(all,month.name)
    ),
    selectInput("severity", "Choose a severity",
                c(all,levels(factor(accidents$severity)))
    ),
    selectInput("city", "Choose a city",
                c(all,levels(factor(accidents$city)))
    )
  ),
  mainPanel(plotOutput("plot2"))
)

server <- function(input,output){
  
  dat <- reactive({
    acc <- accidents
    if(input$year != all)
      acc <- subset(acc, year %in% input$year)
    if(input$month != all)
      acc <- subset(acc,month %in% input$month)
    if(input$severity != all)
      acc <- subset(acc,severity %in% input$severity)
    if(input$city != all)
      acc <- subset(acc,city %in% input$city)
    acc
  })
  
  output$plot2<-renderPlot({
    ggplot(dat(), aes(x = Time, y = Day_of_Week)) + 
      stat_bin2d(aes(fill = stat(count))) + 
      scale_fill_gradient(low="grey90", high="red")
  })
}

shinyApp(ui, server)
