# Load required packages.
library(leaflet)
library(rgdal)
library(geojsonio)
library(shiny)
library(tidyr)

message("reading and loading of the dataset,\n this operation can take some minutes the datasets are very big.")

if(!exists("accidents")){
  message("wrangling data...")
  source("../wrangling.R")
}

ukcounties <- geojson_read("../data/uk_map1.geojson", what = "sp")

count <- accidents %>% na.omit() %>%
  count(geo_code, Weather_Conditions, sort = TRUE) %>%
  spread(Weather_Conditions,n) %>%
  mutate(tot = Fine+Raining+Snowing+Fog) %>%
  mutate(Fine = round(Fine/tot, 2)*100, 
         Raining=round(Raining/tot, 2)*100, 
         Snowing=round(Snowing/tot, 2)*100, 
         Fog=round(Fog/tot, 2)*100)

#ukcounties@data <- merge(x = ukcounties@data, y = count,by.x="geo_code", by.y = "geo_code", all = TRUE)

#ukcounties@data <- ukcounties@data %>%
#  select(-c(geo_labelw)) %>%
#  na.omit()

ui <- fluidPage(
  titlePanel(title=h4("In which counties bad weather affects more the traffic?", align="center")),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/> Accident:%g <br/> BadWeather:%g &#37;",
      ukcounties$name, 
      ukcounties$total, 
      ukcounties$bad
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorBin("Reds", domain = ukcounties$bad)
    
    leaflet(ukcounties) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(bad),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~bad, opacity = 0.7, title = NULL,
                position = "bottomright")
    
  })
}

shinyApp(ui, server)
