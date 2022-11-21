
# Pacakgae used
library(tidyverse)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)
library(ggmap)


# Load dataset 
# food_inspect<- read.csv("ftmpf12b2esk.csv")
# food_inspect$location<- str_remove_all(food_inspect$location, "\\(") 
# food_inspect$location <- str_remove_all(food_inspect$location, "\\)")
# food_inspect$resultdttm <- as.Date(food_inspect$resultdttm, na.rm = FALSE)
# f_i<- food_inspect%>% separate(location,c("latitude", "longtitude"), sep = ",", fill = "right") 
# f_i<- f_i %>% select(-dbaname)
# f_i$latitude <- as.double(f_i$latitude)
# f_i$longtitude <- as.double(f_i$longtitude)
# f_i<- drop_na(f_i) 
# 
# # select data for year 2022
# food1 <- f_i %>% group_by(businessname) %>% filter(resultdttm> '2022-01-01') %>% mutate(rank = row_number(businessname)) %>%
#   filter(rank == 1)




# *******
# We cleaned the data and rank the order according to the result dates and exported into food_clean.csv. 
# Please use food_clean.csv to run the following code and Shiny.

food1 <- read.csv("food_clean.csv")
fi_sf<- st_as_sf(food1, coords=c("longtitude","latitude" ),crs=4326)




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
                                               selectInput(inputId = "my_zip", 
                                                           label = "Zone", 
                                                           choices = unique(food1$zip),
                                                           width = "220px"))),
                                      fluidRow(column(12,plotOutput(outputId = "zonemap",
                                                                    height = 500)))
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
     input$license })
   
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
  page3 <- reactive({
    food1 %>% 
      filter(zip == input$my_zip)
  })
  
  output$zonemap <- renderPlot({
    qmplot(x = longtitude, 
           y = latitude,
           data = page3(),
           zoom = 15,
           f = 0.1)
  })
}

shinyApp(ui, server)
