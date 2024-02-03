
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
# world_data <- ggplot2::map_data('world')
# world_data <- fortify(world_data)
# head(world_data)

#world <- ne_countries(scale = "medium", returnclass = "sf")

# Europe <- world[which(world$continent == "Europe"),]
# ggplot(Europe) +
#   geom_sf() +
#   coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)

world_map = map_data("world")



#print(map_bounds)




####### Length of TT Cleaned Load #########

length_of_tt_data <- read.csv("CLEANED_ Length of Tenure Track - Sheet1.csv", stringsAsFactors=T)
#print(length_of_tt_data)
mapData <- world[c(2,11)]
map_bounds <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  inner_join(length_of_tt_data, by = join_by(admin == Country)) %>% 
  st_transform(4326)
print(length_of_tt_data)

countries <- group_by(length_of_tt_data, Country) %>% 
  summarise(avgRating = round(mean(Length.of.TT..years.),digits=2))

countries <- left_join(countries, mapData, c("Country" = "name_long"))

print(countries)


bins <- c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, Inf)
#pal <- colorBin("YlOrRd", domain = map_bounds$Length.of.TT..years, bins = bins)
pal <- colorNumeric(palette = "YlOrRd", domain = countries$avgRating)

## create the UI ##
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
  
  # # Sidebar layout with input and output definitions
  # sidebarLayout(
  #   
  #   # Sidebar panel for inputs 
  #   sidebarPanel(
  #     
  #     # First input: Type of data
  #     selectInput(inputId = "Selector",
  #                 label = "Choose the type of data you want to see:",
  #                 choices = list("Length of Tenure Track by Country", 
  #                                "Random Number Yas"))
  #     
  #   ),
  
  fluidRow(class = "toprow",
           fluidRow (class = "filters",
                     
                     column(6,
                            # Country menu
                            selectInput("country", "Country", levels(length_of_tt_data$Country) %>% 
                                          append("All") %>% # Add "All" option
                                          sort()) # Sort options alphabetically
                            
                     )
           ),

  # place the contents inside a box
  fluidRow(class = "mainrow",
      # separate the box by a column
      column(width = 6,
             leaflet::leafletOutput(outputId = "myMap", height = 500)),
      
      column(width = 6,
        dataTableOutput(outputId = "table"))
         
  #,
  # 
  # fluidRow (class = "table",
  #           # Table
  #           dataTableOutput("table")
  )
  
  
))




## create the server ##
server <- function( input, output, session ){
  # create foundational map
  foundational.map <- shiny::reactive({
    leaflet(map_bounds) %>% 
      
      fitBounds(-20, 65, 20, 39) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = countries$geom, 
                  #layerId = map_bounds$admin, 
                  # fillColor = ~pal(map_bounds),
                  fillColor = pal(countries$avgRating),
                  color = "blue", 
                  group = "click.list",
                  weight = 2, 
                  popup = paste("Country: ", countries$Country, "<br>",
                               "Average length of TT: ", countries$avgRating, " years", "<br>"
                               ),
                  fillOpacity = 0.6, 
                  opacity = 1,
                  smoothFactor = 0.2) %>%
      addLegend(pal = pal, values = map_bounds$Length.of.TT..years, opacity = 0.7, title = NULL,
                position = "bottomright")
  })

  output$myMap <- renderLeaflet({    
    if (input$country != "All") {
    length_of_tt_data <- filter(length_of_tt_data, Country == input$country)}
    foundational.map()
  }) 

  # Clear the map button
  shiny::observeEvent( input$clearHighlight, { 
    output$myMap <- leaflet::renderLeaflet({
      click.list$ids <- NULL
      foundational.map()
    }) 
  })
  
  shiny::observeEvent(input$Selector, {
    if(input$Selector == "Length of Tenure Track by Country"){
      choice = map_bounds$average_tt_length
      additional_text = " years"
    }
    else{
      choice = map_bounds$random_number_yas
      additional_text = " randoms"
    }
    leafletProxy("myMap") %>%
      addPolygons(data = map_bounds, 
                  layerId = map_bounds$admin, 
                  fillColor = ~pal(choice),
                  color = "blue", 
                  group = "click.list",
                  weight = 2, 
                  popup = paste("Country: ", map_bounds$admin, "<br>",
                                input$Selector,":", choice, additional_text, "<br>"),
                  fillOpacity = 0.6, 
                  opacity = 1,
                  smoothFactor = 0.2)
  })
  
  output$table <- renderDataTable({
    
    # Filter data based on selected Country
    if (input$country != "All") {
      length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
    }
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(length_of_tt_data) > 0, "")
    )
    
    length_of_tt_data[,2:3]
    
    
  },
    options = list(pageLength = 10)
  )
  
#   # selected countries
#   output$selected_var <- renderText({ 
#     paste("You have selected", click.list$ids)
#   })
}


## run shinyApp ##
shiny::shinyApp(ui, server)

