library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(shiny)
library(GGally)

message("reading and loading of the dataset,\n this operation can take sometimes the datasets are very big.")

if(!exists("accidents")){
  message("wrangling data...")
  source("../wrangling.R")
}

savdf <- accidents

accidents <- na.omit(accidents)
#summary(accidents)
#accidents <- sample_n(accidents,5000)

accidents$Time <- factor(substr(accidents$Time, 0, 2))

all <- "all"

ui <- fluidPage(
  titlePanel(title=h4("In which cities decreases the number of people injured or dead in accidents?", align="center")),
  sidebarPanel(
    radioButtons("city", "Choose a city",
                c(all, "London", "Manchester", "Birmingham", "Wolverhampton", "Leeds", "Bradford",
                  "Southampton", "Liverpool", "Newcastle upon Tyne", "Nottingham", "Sheffield", "Bristol, City of", "Leicester"))
    ),
  selectInput("severity", "Choose a severity",
              c(all,levels(factor(accidents$severity)))
  ),
  selectInput("weather", "Choose wheather condition",
              c(all,levels(factor(accidents$Weather_Conditions)))
  ),
  mainPanel(plotOutput("plot2"))
)

server <- function(input,output){
  
  dat <- reactive({
    acc <- accidents
    if(input$severity != all)
      acc <- subset(acc,severity %in% input$severity)
    if(input$weather != all)
      acc <- subset(acc,Weather_Conditions %in% input$weather)
    if(input$city != all)
      acc <- subset(acc,city %in% input$city)
    acc
  })
  
  output$plot2<-renderPlot({
    ggplot(dat(), aes(x=factor(year))) + 
    geom_bar(fill="red")
  })
}

shinyApp(ui, server)

