
library(magrittr)
library(rvest)
library(countrycode)
library(leaflet)
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
library(rsconnect)

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


##### Creating Colour Palettes #####
tt_length_pal <- colorNumeric(palette = "YlOrRd", domain = countries$avgLength)
gender_pal <- colorNumeric(palette = "RdPu", domain = gender_map_bounds$Women.at.Full.Professor.Level)
contract_pal <- colorNumeric(palette = "Blues", domain = contract_map_bounds$Fixed.Term.Contingent.Contracts)
#contract_pal <- colorBin("Blues", contract_map_bounds$Fixed.Term.Contingent.Contracts, 12, pretty = FALSE)

##### creating the UI #####
ui = fluidPage(class="page",
               theme = shinytheme("superhero"),
  
   # CSS code
   tags$head(
     tags$style(HTML("
     .container-fluid {width: 1500px; padding: 40px; padding: 40px;}
     .title { text-align: center; font-family: arial black, sans serif;}
     .bar {background-color: #2b3e50}
        "))
      ),
  
  #title
  h1("Academic Tenure Track Data", class = "title"),

  
  fluidRow(class = "toprow",
           fluidRow (class = "filters",

                     column(6,
                            # Country menu
                            selectInput("country", "Country", levels(length_of_tt_data$Country) %>%
                                          append("All"), selected="All" %>% # Add "All" option
                                          sort()) # Sort options alphabetically

                     ),
                     column(6,
                            # Data Type menu
                            selectInput("data_set", "Data Set", c("Length of Tenure Track", 
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

#create the server
server <- function( input, output, session ){
  
  #setting initial view for the map output
  initial_lat = 60
  initial_lng = -30
  initial_zoom = 2.3
  
  #creating map output
  output$myMap <- renderLeaflet({  
    
    if(input$data_set == "Gender Statistics"){
      
      #filtering data if only one country is selected
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
        gender_countries <- filter(gender_countries, Country == input$country)
        gender_map_bounds <- filter(gender_map_bounds, admin == input$country)}
      
      #creating gender map
      foundational.gender.map <- shiny::reactive({
        leaflet(gender_map_bounds) %>% 
          fitBounds(-20, 65, 20, 39) %>%
          setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(data = gender_countries$geom, 
                      fillColor = gender_pal(gender_tt_data$Women.at.Full.Professor.Level),
                      color = "blue", 
                      group = "click.list",
                      weight = 2, 
                      #creating data popups when a country is selected
                      popup = paste("Country: ", gender_countries$Country, "<br>",
                                    "Women at Full Professor Level: ", gender_tt_data$Women.at.Full.Professor.Level, "%", "<br>"
                      ), 
                      fillOpacity = 0.6, 
                      opacity = 1,
                      smoothFactor = 0.2) %>%
          addLegend(pal = gender_pal, values = gender_map_bounds$Women.at.Full.Professor.Level, opacity = 0.7, title = NULL,
                    position = "bottomright") #adding a legend to the map
      })
      foundational.gender.map()
    }
    
    
    #if the "Length of Tenure Track" dataset is selected
    else if(input$data_set == "Length of Tenure Track"){
      
      #filtering data if only one country is selected
      if (input$country != "All") {
        length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
        countries <- filter(countries, Country == input$country)
        map_bounds <- filter(map_bounds, admin == input$country)}
      
      #creating length of TT map
      foundational.length.map <- shiny::reactive({
        leaflet(map_bounds) %>% 
          fitBounds(-20, 65, 20, 39) %>%
          setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(data = countries$geom, 
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
      
      #displaying map
      foundational.length.map()
    }
    
    else{
      
      #filtering data if only one country is selected
      if (input$country != "All") {
        contract_countries <- filter(contract_countries, Country == input$country)
        contract_data <- filter(contract_data, Country == input$country)
        contract_map_bounds <- filter(contract_map_bounds, admin == input$country)}
      
      #creating contract map
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
          addLegend(pal = contract_pal, values = round(contract_map_bounds$Fixed.Term.Contingent.Contracts,digits=2),
                    opacity = 0.7, title = NULL, position = "bottomright")
      })
      
      #displaying map
      foundational.contract.map()
    }
  }) 
  
  
  # Creating gender bar chart
  output$barChart <- renderPlot({
    if(input$data_set == "Gender Statistics"){
      
      #filtering data if only one country is selected
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
        validate (need(nrow(gender_tt_data) > 0, "No data available with current filters")) #checking if data exists for selected country
        }
        
          #generating the plot from the data
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
          scale_fill_manual(values=c("#54396D", "#8A5CB3", "#D295F5"))+ #colouring the bars
          xlab("Country") + 
          ylab("%") 
      
    }
  else if(input$data_set == "Contract Type"){
    if (input$country != "All") {
      contract_data <- filter(contract_data, Country == input$country)
      validate (need(nrow(contract_data) > 0, "No data available with current filters"))
      
      #creating a dataframe for the pie charts
      df = data.frame(subject <- c('contract_type','contract_type','contract_hours','contract_hours'), 
                      credit <- c('temporary','permanent','full-time','part-time'), 
                      value <- c(contract_data$Fixed.Term.Contingent.Contracts, contract_data$Permanent.Contracts, 
                                 contract_data$Full.Time, contract_data$Part.Time)) 
      
      df$subject <- factor(df$subject) 
      df$credit <- factor(df$credit)  
      
      #making the pie charts
      ggplot(data=df, aes(x=" ", y=value, group=credit, colour=credit, fill=credit)) + 
        geom_bar(width = 1, stat = "identity") + 
        coord_polar("y", start=0) +  
        facet_grid(.~ subject) +
        
        #applyig a colour theme
        theme(
          strip.text.x = element_text(
            size = 12, color = "black", face = "bold"
          ),
          strip.text.y = element_text(
            size = 12, color = "black", face = "bold"
          )
        )+
        #adding lavels
        geom_text(aes(label = value),
                    position = position_stack(vjust = 0.5),
                  size=10, colour='black') +
        scale_fill_brewer(palette="Blues")
      
    }
  }},bg = "#333a4f") #setting background colour

  #Display Data Tables
  output$table <- renderDataTable({
    
    if(input$data_set == "Gender Statistics"){
      #filtering data if only one country is selected
      if (input$country != "All") {
        gender_tt_data <- filter(gender_tt_data, Country == input$country)
      }
      
      #check that the selected filters don't result in an empty dataset
      validate (
        need(nrow(gender_tt_data) > 0, "")
      )
      
      #ordering the data alphabetically
      ordered_data <- gender_tt_data[order(gender_tt_data$Country),]
      
      names(ordered_data)[3] <- "% of Full Professors that are Women" #renaming this column
      ordered_data[,1:3]
    }
    else if(input$data_set == "Length of Tenure Track"){
      if (input$country != "All") {
        length_of_tt_data <- filter(length_of_tt_data, Country == input$country)
      }
      validate (
        need(nrow(length_of_tt_data) > 0, "No data available with current filters")
      )
      ordered_data <- length_of_tt_data[order(length_of_tt_data$University),]
      names(ordered_data)[3] <- "Length of TT (years)"
      round(ordered_data[3],1)
      ordered_data[,2:3]
    }
    else{
      if (input$country != "All") {
        contract_data <- filter(contract_data, Country == input$country)
      }
      validate (
        need(nrow(contract_data) > 0, "No data available with current filters")
      )
      ordered_data <- contract_data[order(contract_data$Country),]
      names(ordered_data)[3] <- "% Temporary Contracts"
      names(ordered_data)[4] <-"% Permanent Contracts"
      names(ordered_data)[5] <- "% Full Time Faculty"
      names(ordered_data)[6] <- "% Part Time Faculty"
      ordered_data[,1:6]
    }
    
  },
  #list 10 rows
  options = list(pageLength = 10)
  )
  
}

## run shinyApp ##
shiny::shinyApp(ui, server)

