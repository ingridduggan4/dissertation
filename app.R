
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

world_map = map_data("world")

####### Loading in the Data #########
length_of_tt_data <- read.csv("Tenure Length and Tenure Clock - Sheet1.csv", stringsAsFactors=T)
gender_tt_data <- read.csv("Tenure Gender Data - Gender Stats.csv", stringsAsFactors=T)


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
print(gender_countries)


##### Colour Palettes #####
tt_length_pal <- colorNumeric(palette = "YlOrRd", domain = countries$avgLength)
gender_pal <- colorNumeric(palette = "RdPu", domain = gender_map_bounds$Women.at.Full.Professor.Level)

##### creating the UI #####
ui = fluidPage(class="page",
  
  # CSS
  tags$head(
    tags$style(HTML("
      body { background-color: ##a3bdee; }
      .container-fluid { background-color: #fff; width: 1200px; padding: 30px; }
      .title { text-align: center; },
      .page { background-color: ##a3bdee; }
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
                                          append("All") %>% # Add "All" option
                                          sort()) # Sort options alphabetically

                     ),



           ),


  # place the contents inside a box
  fluidRow(class = "mainrow",
      # separate the box by a column
      column(width = 6,
             leaflet::leafletOutput(outputId = "myMap", height = 500)),
      
      column(width = 6,
        dataTableOutput(outputId = "table"))
         
  )
  
))

## THIS IS A WORKING SERVER: ONLY DELETE WHEN OTHER IS FULLY WORKING ##
# server <- function( input, output, session ){
#   foundational.map <- shiny::reactive({
#     leaflet(map_bounds) %>% 
#       
#       fitBounds(-20, 65, 20, 39) %>% 
#       addProviderTiles(providers$CartoDB.Positron) %>% 
#       addPolygons(data = countries$geom, 
#                   #layerId = map_bounds$admin, 
#                   # fillColor = ~pal(map_bounds),
#                   fillColor = pal(countries$avgLength),
#                   color = "blue", 
#                   group = "click.list",
#                   weight = 2, 
#                   popup = paste("Country: ", countries$Country, "<br>",
#                                "Average length of TT: ", countries$avgLength, " years", "<br>"
#                                ),
#                   fillOpacity = 0.6, 
#                   opacity = 1,
#                   smoothFactor = 0.2) %>%
#       addLegend(pal = pal, values = map_bounds$Length.of.TT..years, opacity = 0.7, title = NULL,
#                 position = "bottomright")
#   })
# 
#   output$myMap <- renderLeaflet({    
#     if (input$country != "All") {
#     length_of_tt_data <- filter(length_of_tt_data, Country == input$country)}
#     foundational.map()
#   }) 
# 
#   
#   # shiny::observeEvent(input$Selector, {
#   #   if(input$Selector == "Length of Tenure Track by Country"){
#   #     choice = map_bounds$average_tt_length
#   #     additional_text = " years"
#   #   }
#   #   else{
#   #     choice = map_bounds$random_number_yas
#   #     additional_text = " randoms"
#   #   }
#   #   leafletProxy("myMap") %>%
#   #     addPolygons(data = map_bounds, 
#   #                 layerId = map_bounds$admin, 
#   #                 fillColor = ~pal(choice),
#   #                 color = "blue", 
#   #                 group = "click.list",
#   #                 weight = 2, 
#   #                 popup = paste("Country: ", map_bounds$admin, "<br>",
#   #                               input$Selector,":", choice, additional_text, "<br>"),
#   #                 fillOpacity = 0.6, 
#   #                 opacity = 1,
#   #                 smoothFactor = 0.2)
#   # })
#   
#   #Display Data Table
#   output$table <- renderDataTable({
#     
#     # Filter data based on selected Country
#     if (input$country != "All") {
#       length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
#     }
#     
#     # Hide table when user has filtered out all data
#     validate (
#       need(nrow(length_of_tt_data) > 0, "")
#     )
#     
#     length_of_tt_data[,2:3]
#     
#   },
#     options = list(pageLength = 10)
#   )
#   
# }


#################################################################################################################



server <- function( input, output, session ){
  
  # selection <- shiny::reactive({
  #   input$data_type
  # })
  # 
  # print(selection)
  
  #if(selection != "Length of Tenure Track"){
    foundational.gender.map <- shiny::reactive({
      leaflet(gender_map_bounds) %>% 
        
        fitBounds(-20, 65, 20, 39) %>% 
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
  
  
  # shiny::observeEvent(input$Selector, {
  #    if(input$Selector == "Length of Tenure Track by Country"){
  #      choice = map_bounds$average_tt_length
  #      additional_text = " years"
  #    }
  #  else{
  #      choice = map_bounds$random_number_yas
  #      additional_text = " randoms"
  #    }
  #    leafletProxy("myMap") %>%
  #      addPolygons(data = map_bounds, 
  #                  layerId = map_bounds$admin, 
  #                  fillColor = ~pal(choice),
  #                  color = "blue", 
  #                  group = "click.list",
  #                  weight = 2, 
  #                  popup = paste("Country: ", map_bounds$admin, "<br>",
  #                                input$Selector,":", choice, additional_text, "<br>"),
  #                  fillOpacity = 0.6, 
  #                  opacity = 1,
  #                  smoothFactor = 0.2)
  #  })
  
  #Display Data Table
  output$table <- renderDataTable({
    
    if(input$data_type == "Gender Statistics"){
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
      }
      validate (
        need(nrow(gender_tt_data) > 0, "")
      )
      gender_tt_data[,1:5]
    }
    else{
      if (input$country != "All") {
        length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
      }
      validate (
        need(nrow(length_of_tt_data) > 0, "")
      )
      length_of_tt_data[,2:3]
    }
    
  },
  options = list(pageLength = 10)
  )
  
}


## run shinyApp ##
shiny::shinyApp(ui, server)

