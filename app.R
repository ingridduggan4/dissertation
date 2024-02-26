
library(magrittr)
library(rvest)
library(countrycode)
library(leaflet)
iso_codes = countrycode::codelist[, c("un.name.en", "iso3c")]
names(iso_codes) = c("Country", "ISO3")

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

library(maps)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(sf)
library(shinydashboard)
library(spData)
library(readxl)
library(tidyr)
library(bslib)
library(shinythemes)


world_map = map_data("world")

####### Loading in the Data #########
length_of_tt_data <- read.csv("Tenure Length and Tenure Clock - Sheet1.csv", stringsAsFactors=T)
gender_tt_data <- read.csv("Tenure Gender Data - Gender Stats.csv", stringsAsFactors=T)
str(gender_tt_data)

####### Loading the Map Data for Length of TT Data #########
mapData <- world[c(2,11)]
map_bounds <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  inner_join(length_of_tt_data, by = join_by(admin == Country)) %>% 
  st_transform(4326)

countries <- group_by(length_of_tt_data, Country) %>% 
  summarise(avgLength = round(mean(Length.of.TT..years.),digits=2))

countries <- left_join(countries, mapData, c("Country" = "name_long"))


####### Loading the Map Data for Gender Data #########
gender_map_bounds <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  inner_join(gender_tt_data, by = join_by(admin == Country)) %>% 
  st_transform(4326)

gender_countries <- group_by(gender_tt_data, Country)

gender_countries <- left_join(gender_countries, mapData, c("Country" = "name_long"))


##### Colour Palettes #####
tt_length_pal <- colorNumeric(palette = "YlOrRd", domain = countries$avgLength)
gender_pal <- colorNumeric(palette = "RdPu", domain = gender_map_bounds$Women.at.Full.Professor.Level)

##### creating the UI #####
ui = fluidPage(class="page",
               theme = shinytheme("superhero"),
  # # CSS
  # tags$head(
  #   tags$style(HTML("
  #     body { background-color: #f2efe9; }
  #     .container-fluid { background-color: #fff; width: 1500px; padding: 40px; }
  #     .title { text-align: center; font-family: arial black, sans serif;}
  #     .toprow { margin: 60px 0px; padding: 30px; background-color: #8080ff; }
  #     .mainrow {margin: 60px 0px; padding: 30px; background-color: #ffffff; }
  #     .filters { margin: 0px auto; }
  #     .shiny-input-container { width:100% !important; }
  #     .table { padding: 30px;}
  #     .bar { margin: 60px 0px; padding: 30px; background-color: #ffffff; }
  #     .leaflet-top { z-index:999 !important; }
  # 
  #     "))
  # ),
  
   # CSS
   tags$head(
     tags$style(HTML("
     .container-fluid {width: 1500px; padding: 40px; padding: 40px;}
     .title { text-align: center; font-family: arial black, sans serif;}
     .bar {background-color: #2b3e50}
     .divider {height: 100px}
        "))
      ),
  
  
  h1("Academic Tenure Track Data", class = "title"),

  
  fluidRow(class = "toprow",
           fluidRow (class = "filters",

                     column(6,
                            # Country menu
                            selectInput("country", "Country", levels(length_of_tt_data$Country) %>%
                                          append("All") %>% # Add "All" option
                                          sort()) # Sort options alphabetically

                     ),
                     column(6,
                            # Data Type menu
                            selectInput("data_type", "Data Type", c("Length of Tenure Track", "Gender Statistics") %>%
                                          sort()) # Sort options alphabetically

                     ),



           ),
           
           
    fluidRow(class = "mainrow",
             # separate the box by a column
              column(width = 6,
                  leaflet::leafletOutput(outputId = "myMap", height = 500)),
                    column(width = 6,
                           dataTableOutput(outputId = "table")),
             
    fluidRow(class="divider", column(width=12)),

    # place the contents inside a box
    fluidRow(class = "bar",
            
             column(width=12,
             # Bar Chart
             plotOutput("barChart"))
  
         
  )

           
  )
  
))

#################################################################################################################



server <- function( input, output, session ){
  
  initial_lat = 60
  initial_lng = -30
  initial_zoom = 2.3

    foundational.gender.map <- shiny::reactive({
      leaflet(gender_map_bounds) %>% 
        fitBounds(-20, 65, 20, 39) %>%
        setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(data = gender_countries$geom, 
                    #layerId = map_bounds$admin, 
                    # fillColor = ~pal(map_bounds),
                    fillColor = gender_pal(gender_tt_data$Women.at.Full.Professor.Level),
                    color = "blue", 
                    group = "click.list",
                    weight = 2, 
                    popup = paste("Country: ", gender_countries$Country, "<br>",
                                  "Women at Full Professor Level: ", gender_tt_data$Women.at.Full.Professor.Level, "%", "<br>"
                    ),
                    fillOpacity = 0.6, 
                    opacity = 1,
                    smoothFactor = 0.2) %>%
        addLegend(pal = gender_pal, values = gender_map_bounds$Women.at.Full.Professor.Level, opacity = 0.7, title = NULL,
                  position = "bottomright")
    })
    #}
    #else{
      foundational.length.map <- shiny::reactive({
        leaflet(map_bounds) %>% 
          fitBounds(-20, 65, 20, 39) %>%
          setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(data = countries$geom, 
                      #layerId = map_bounds$admin, 
                      # fillColor = ~pal(map_bounds),
                      fillColor = tt_length_pal(countries$avgLength),
                      color = "blue", 
                      group = "click.list",
                      weight = 2, 
                      popup = paste("Country: ", countries$Country, "<br>",
                                    "Average length of TT: ", countries$avgLength, " years", "<br>"
                      ),
                      fillOpacity = 0.6, 
                      opacity = 1,
                      smoothFactor = 0.2) %>%
          addLegend(pal = tt_length_pal, values = map_bounds$Length.of.TT..years, opacity = 0.7, title = NULL,
                    position = "bottomright")
      })
    #}
  
  
  output$myMap <- renderLeaflet({    
    if (input$country != "All") {
      length_of_tt_data <- filter(length_of_tt_data, Country == input$country)}
    if(input$data_type == "Gender Statistics"){
      foundational.gender.map()
    }
    else{
      foundational.length.map()
    }
    #foundational.map()
  }) 
  
  
  # Create bar chart of brands
  output$barChart <- renderPlot({
    if(input$data_type == "Gender Statistics"){
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
        validate (need(nrow(gender_tt_data) > 0, "No data available"))
        }
        
          gender_tt_data %>% 
          select(everything())%>%
            pivot_longer(.,
                         cols=c(Women.at.Full.Professor.Level..,
                                Women.at.Associate.Professor.Level..,
                                Women.at.Assistant.Professor.Level..),
                         names_to="Var",
                         values_to="Val") %>%
          ggplot(aes(y=Val,x=Country,fill=Var))+
          geom_bar(stat="identity", position="dodge")+
          scale_fill_brewer(palette = "Blues") + 
          scale_fill_discrete(name = "Legend", 
                              labels = c("Women at Assistant Professor Level", 
                                         "Women at Associate Professor Level", 
                                         "Women at Full Professor Level")) + 
          xlab("Country") + 
          ylab("%") 
        
          
     
      
      
    }
    
  },bg = "#313D52")

  
  #Display Data Table
  output$table <- renderDataTable({
    
    if(input$data_type == "Gender Statistics"){
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
      }
      validate (
        need(nrow(gender_tt_data) > 0, "")
      )
      gender_tt_data[,1:3]
    }
    else{
      if (input$country != "All") {
        length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
      }
      validate (
        need(nrow(length_of_tt_data) > 0, "No data available")
      )
      length_of_tt_data[,2:3]
    }
    
  },
  options = list(pageLength = 10)
  )
  
}


## run shinyApp ##
shiny::shinyApp(ui, server)

