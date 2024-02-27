
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
contract_data <- read.csv("Permanent vs Temp Contracts - Data.csv", stringsAsFactors=T)

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


####### Loading the Map Data for Contracts Data #########
contract_map_bounds <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  inner_join(contract_data, by = join_by(admin == Country)) %>% 
  st_transform(4326)

contract_countries <- group_by(contract_data, Country)
contract_countries <- left_join(contract_countries, mapData, c("Country" = "name_long"))


##### Colour Palettes #####
tt_length_pal <- colorNumeric(palette = "YlOrRd", domain = countries$avgLength)
gender_pal <- colorNumeric(palette = "RdPu", domain = gender_map_bounds$Women.at.Full.Professor.Level)
contract_pal <- colorNumeric(palette = "Reds", domain = contract_map_bounds$Fixed.Term.Contingent.Contracts)
contract_pal <- colorBin("Blues", contract_map_bounds$Fixed.Term.Contingent.Contracts, 12, pretty = FALSE)

##### creating the UI #####
ui = fluidPage(class="page",
               theme = shinytheme("superhero"),
  
   # CSS
   tags$head(
     tags$style(HTML("
     .container-fluid {width: 1500px; padding: 40px; padding: 40px;}
     .title { text-align: center; font-family: arial black, sans serif;}
     .bar {background-color: #2b3e50}
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
                            selectInput("data_type", "Data Type", c("Length of Tenure Track", 
                                                                    "Gender Statistics",
                                                                    "Contract Type") %>%
                                          sort()) # Sort options alphabetically

                     ),



           ),
           
           
    fluidRow(class = "mainrow",
             # separate the box by a column
              column(width = 6,
                  leaflet::leafletOutput(outputId = "myMap", height = 500)),
                    column(width = 6,
                           dataTableOutput(outputId = "table")),
             

    # place the contents inside a box
    fluidRow(class = "bar",
            
             column(width=12,
             # Bar Chart
             plotOutput("barChart"), style='padding: 20px;')
  
         
  )

           
  )
  
))

#################################################################################################################



server <- function( input, output, session ){
  
  #setting initial zoom for the map output
  initial_lat = 60
  initial_lng = -30
  initial_zoom = 2.3
  
  
  output$myMap <- renderLeaflet({  
    
    if(input$data_type == "Gender Statistics"){
      
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
        gender_countries <- filter(gender_countries, Country == input$country)
        gender_map_bounds <- filter(gender_map_bounds, admin == input$country)}
      
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
      
      foundational.gender.map()
    }
    
    
    else if(input$data_type == "Length of Tenure Track"){
      
      if (input$country != "All") {
        length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
        countries <- filter(countries, Country == input$country)
        map_bounds <- filter(map_bounds, admin == input$country)}
      
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
      
      foundational.length.map()
    }
    
    else{
      if (input$country != "All") {
        contract_countries <- filter(contract_countries, Country == input$country)
        contract_data <- filter(contract_data, Country == input$country)
        contract_map_bounds <- filter(contract_map_bounds, admin == input$country)}

      foundational.contract.map <- shiny::reactive({
        leaflet(contract_map_bounds) %>%
          fitBounds(-20, 65, 20, 39) %>%
          setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(data = contract_countries$geom,
                      fillColor = contract_pal(contract_data$Fixed.Term.Contingent.Contracts),
                      color = "blue",
                      group = "click.list",
                      weight = 2,
                      popup = paste("Country: ", contract_countries$Country, "<br>",
                                    "Percentage of academic staff on temporary contracts: ",
                                    contract_data$Fixed.Term.Contingent.Contracts, "%", "<br>"
                      ),
                      fillOpacity = 0.6,
                      opacity = 1,
                      smoothFactor = 0.2) %>%
          addLegend(pal = contract_pal, values = contract_map_bounds$Fixed.Term.Contingent.Contracts,
                    opacity = 0.7, title = NULL, position = "bottomright")
      })
      
      foundational.contract.map()
    }
  }) 
  
  
  # Create bar chart
  output$barChart <- renderPlot({
    if(input$data_type == "Gender Statistics"){
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
        validate (need(nrow(gender_tt_data) > 0, "No data available with current filters"))
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
          scale_fill_manual(values=c("#54396D", "#8A5CB3", "#B574EE"))+
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
    else if(input$data_type == "Length of Tenure Track"){
      if (input$country != "All") {
        length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
      }
      validate (
        need(nrow(length_of_tt_data) > 0, "No data available with current filters")
      )
      length_of_tt_data[,2:3]
    }
    else{
      if (input$country != "All") {
        contract_data <- filter(contract_data, Country == input$country)
      }
      validate (
        need(nrow(contract_data) > 0, "No data available with current filters")
      )
      contract_data[,1:6]
    }
    
  },
  options = list(pageLength = 10)
  )
  
}


## run shinyApp ##
shiny::shinyApp(ui, server)

