
# Pacakgae used
library(tidyverse)
library(ggplot2)
library(tmap)
library(ggmap)
library(sf)
library(mapview)
library(leaflet)
library(stringr)


# Load dataset 
food_inspect<- read.csv("foodinspection.csv")
food_inspect$location<- str_remove_all(food_inspect$location, "\\(") 
food_inspect$location <- str_remove_all(food_inspect$location, "\\)")
food_inspect$resultdttm <- as.Date(food_inspect$resultdttm, na.rm = FALSE)


f_i<- food_inspect%>% separate(location,c("latitude", "longtitude"), sep = ",", fill = "right") 
f_i<- f_i %>% select(-dbaname)
f_i$latitude <- as.double(f_i$latitude)
f_i$longtitude <- as.double(f_i$longtitude)
f_i<- drop_na(f_i)

# select data for year 2022
food1 <- f_i %>% group_by(businessname) %>% filter(resultdttm> '2022-01-01') %>% mutate(rank = row_number(businessname)) %>%
  filter(rank == 1)

fi_sf<- st_as_sf(food1, coords=c("longtitude","latitude" ),crs=4326)

# Create maps
## use ggplot2
  
# ggplot(data = fi_sf)+
#   geom_sf(aes(color = licstatus))+
#   ggtitle(label = "Food Establishment Inspections", subtitle = "License Status of Business")
# 
# ## use mapview
# mapview(fi_sf,zcol="licstatus")
# 
# # use leaflet
# m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
# m %>% addTiles()
# m %>% addProviderTiles(providers$Stamen.Toner)
# 
# leaflet(data = food1) %>% addTiles() %>%
#   addMarkers(~longtitude, ~latitude, popup = ~as.character(licstatus), label = ~as.character(licstatus))


# The Shiny App
library(shiny)
library(shinythemes)
library(ggplot2)

ui <- fluidPage(navbarPage("Food Establishment Inspections", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
                            tabPanel("Overview", leafletOutput("map"), textOutput("text"), height = 700),
                            tabPanel("Restaurant Information", 
                                     fluidPage(
                                       tabsetPanel(
                                         tabPanel("Restuarant Information",
                                                  fluidPage(
                                                    fluidRow(
                                                      column(2,
                                                             selectInput(inputId = "fooda", label = "Restaurant1", food1$businessname,width = "220px"))
                                                    ),
                                                    fluidRow(
                                                      column(9,tableOutput("table1"))
                                                    ))
                                         ),
                                         tabPanel("Customer Evaluation",
                                                  fluidPage(
                                                    fluidRow(
                                                      column(2,
                                                             selectInput(inputId = "foodb", label = "Restaurant2", food1$businessname, width = "220px"))),
                                                    fluidRow(column(9,tableOutput("table2")))
                                                  )),
                                         tabPanel("License Information", 
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      selectInput("license", label = "License status of restaurants", choices = fi_sf$licstatus)),
                                                    mainPanel(
                                                      plotOutput("plot")))))
                                       )),
                           tabPanel("Address",
                                    fluidPage(
                                      fluidRow(
                                        column(2,
                                               selectInput(inputId = "address", label = "Zone", 
                                                           choices = food1 %>% select(address),
                                                           width = "220px"))),
                                      fluidRow(column(12,leafletOutput(outputId = "zonemap",height = 500), 
                                                      actionButton("search", "Search!")))
                                    ))
) 
)



server <- function(input, output, session) {
  # Tab: overview
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addMarkers(data = food1, ~longtitude, ~latitude,  clusterOptions = markerClusterOptions(), label = ~as.character(licstatus))})
 
   output$text <- renderText({
    "The data provides information about health inspections, which are performed annually,
    checking sanitary conditions of restaurants in Boston. Note that the majority of 
    inspections within the dataset were done between April 2006 to current. 
    We select data for year 2022 to explore."
  })

  # Tab: Restaurant License
   restaurant <- reactive({
     input$license 
     })
   
  output$plot <- renderPlot({
    if (restaurant() == "Active"){
      fi_sf %>% filter(licstatus == "Active") %>% 
        ggplot()+
        geom_sf(aes(colour = licstatus))+
        ggtitle(label = "Food Establishment Inspections", subtitle = "Active Restaurant")
      } else {fi_sf%>% filter(licstatus != "Active")%>% 
          ggplot()+
          geom_sf()+
          ggtitle(label = "Food Establishment Inspections", subtitle = "Inactive Restaurant")}}, res = 96)
 
   # Tab: Other Information
  output$table1 <- renderTable(food1 %>%filter(businessname == input$fooda) %>% dplyr::select(businessname,businessname, licenseno, licstatus, licensecat, descript, result, address, city, state, zip))
  output$table2 <- renderTable(food1 %>%filter(businessname == input$foodb) %>% dplyr::select(businessname,viollevel, violdesc, violstatus, comments))
  
  # Tab: Location
  point <- eventReactive(input$search,{
    subset(food1, address = as.character(input$address))
  })
  
  output$zonemap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addCircleMarkers(data = point(),~longtitude, ~latitude, label = ~as.character(businessname), fillOpacity = 0.8)
  })
}

shinyApp(ui, server)
