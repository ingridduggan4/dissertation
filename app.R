
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
# world_data <- ggplot2::map_data('world')
# world_data <- fortify(world_data)
# head(world_data)

#world <- ne_countries(scale = "medium", returnclass = "sf")

# Europe <- world[which(world$continent == "Europe"),]
# ggplot(Europe) +
#   geom_sf() +
#   coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)

country_data <- tribble(
  ~Country, ~average_tt_length, ~random_number_yas,
 "Austria", 6, 0,
  "Belgium", 5, 0,
 "Denmark", 6, 2,
  "Finland", 8, 0,
 "France", NaN, 0,
  "Germany", 5.5, 9,
  "Ireland", 5, 0,
  "Italy", 3, 0,
  "Netherlands", 5.67, 0,
 "Portugal", 5, 7,
  "Spain", 6, 3,
  "Sweden", 6, 0,
  "Switzerland", 6, 0,
 "United Kingdom", NaN, 0,
  "United States of America", 5.7, 8,
  "Canada", 5.1, 0
)

world_map = map_data("world")

#print(world_map)

# map_bounds <- world_map %>% 
#   inner_join(country_data, by = join_by(region == Country)) #%>% 
#   #st_transform(4326)

#country_data$Country[country_data$Country == "USA"] <- "United States of America"

map_bounds <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  inner_join(country_data, by = join_by(admin == Country))

print(map_bounds)

  #left_join(country_data, by = join_by(region == Country)) #%>% st_transform(4326)

# ## map & data ##
# Europe <- readOGR(dsn = "https://data.opendatasoft.com/explore/dataset/european-union-countries@public/download/?format=geojson&timezone=Europe/Berlin", 
#                          layer = "OGRGeoJSON", 
#                          stringsAsFactors = FALSE)
# data <- data.frame("name" = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
#                               "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia",
#                               "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Norway", "Poland", "Portugal",
#                               "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom"),
#                    "polcapita" = c(0.0087928207, 0.0100464969, 0.0075375536, 0.0049040898, 0.0097860082, 0.0119440135, 0.0087740252, 
#                                    0.0080851042, 0.0063462331, 0.0064707328, 0.0107846429, 0.0085740997, 0.0059612600, 0.0409884683, 
#                                    0.0138830047, 0.0067616652, 0.0049423915, 0.0053782730, 0.0053461017, 0.0165884166, 0.0046052235, 
#                                    0.0116079951, 0.0052533159, 0.0100049243, 0.0075509189, 0.0047028415, 0.0067531703, 0.0077087169, 
#                                    0.0064795469, 0.0008881585, 0.0053907055, 0.0053085690, 0.0069728195))

# mypalette <- colorNumeric( palette="viridis", domain=map_bounds$average_tt_length, na.color="transparent")
# mypalette(c(45,43))

bins <- c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, Inf)
pal <- colorBin("YlOrRd", domain = map_bounds$average_tt_length, bins = bins)

## create the UI ##
ui = fluidPage(
  
  titlePanel("Academic Tenure Track Data"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # First input: Type of data
      selectInput(inputId = "Selector",
                  label = "Choose the type of data you want to see:",
                  choices = list("Length of Tenure Track by Country", 
                                 "Random Number Yas"))
      
    ),
    
  # place the contents inside a box
  fluidRow(
    box(
      width = 12, 
      # separate the box by a column
      # column(width = 2,
      #        shiny::actionButton(inputId = "clearHighlight",
      #                            icon = icon( name = "eraser"),
      #                            label = "Clear the Map",
      #                            style = "color: #fff; background-color: #D75453; border-color: #C73232")),
      
      # separate the box by a column
      column(width = 10, 
             leaflet::leafletOutput(outputId = "myMap", height = 850)),
      column(width = 5,
             textOutput("selected_var"))
    )
  )
))



## create the server ##
server <- function( input, output, session ){
  # create foundational map
  foundational.map <- shiny::reactive({
    leaflet(map_bounds) %>% 
      fitBounds(-20, 65, 20, 39) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = map_bounds, 
                  layerId = map_bounds$admin, 
                  fillColor = ~pal(average_tt_length),
                  color = "blue", 
                  group = "click.list",
                  weight = 2, 
                  popup = paste("Country: ", map_bounds$admin, "<br>",
                               "Average length of TT: ", map_bounds$average_tt_length, " years", "<br>"),
                  fillOpacity = 0.6, 
                  opacity = 1,
                  smoothFactor = 0.2) %>%
      addLegend(pal = pal, values = map_bounds$average_tt_length, opacity = 0.7, title = NULL,
                position = "bottomright")
  })

  output$myMap <- renderLeaflet({
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
#   # selected countries
#   output$selected_var <- renderText({ 
#     paste("You have selected", click.list$ids)
#   })
}


## run shinyApp ##
shiny::shinyApp(ui, server)

