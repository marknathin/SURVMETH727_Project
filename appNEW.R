# Setup

library(RSocrata)
library(tidyverse)
library(shiny)
library(DT)

library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(leaflet)
library(RColorBrewer)
library(readr)
library(dplyr)
library(leaflet.extras)
library(leaflet.minicharts)

# Data

ShinyAppDS <- read_csv("ShinyAppDS.csv")

ShinyAppDSTable <- ShinyAppDS %>% select(State, county_name, Percent_Democrat, Percent_Republican, 
                                         Never, Rarely, Sometimes, Frequently, Always)

codebook <- data.frame(Variable = c("State", "county_name", "lat", "lng", "Percent_Democrat",
                                    "Percent_Republican", "Difference", "Majority", "Never",
                                    "Rarely", "Sometimes", "Frequently",
                                    "Always", "Never_Rarely", "Frequently_Always"),
                       Description = c("State Name", "County Name",
                                       "Latitude coordinate of county",
                                       "Longitude coordinate of county",
                                       "Percent in county voting for a Democrat in 2020 Presidential election",
                                       "Percent in county voting for a Republican in 2020 Presidential election",
                                       "Difference between percent voting for a Democrat vs. Republican in 2020 Election",
                                       "Political party with more votes in county in 2020 Presidential election",
                                       "Percent reporting never wearing a mask in public",
                                       "Percent reporting rarely wearing a mask in public",
                                       "Percent reporting sometimes wearing a mask in public",
                                       "Percent reporting frequently wearing a mask in public",
                                       "Percent reporting always wearing a mask in public",
                                       "Percent reporting never OR rarely wearing a mask in public",
                                       "Percent reporting frequently OR always wearing a mask in public"),
                       Notes = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", ""))

# Define UI
ui <- dashboardPage(
    dashboardHeader(title = "SURVMETH 727 Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("th"))
        )
    ),
    dashboardBody(
        
        tabsetPanel(
            tabPanel("Map",
                     # Sidebar with a slider input for number of bins 
                     sidebarLayout(
        
                         # Inputs
                         sidebarPanel(
                             h3("Mask Wearing and Presidential Election Votes Data Explorer"),
                             
                             br(),
                             
                             p("Explore the relationship between mask wearing tendencies and presidential election votes for
                  counties throughout the USA."),
                             
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(width = 8, id = "mainpanel",
                                   # Create a space to display the map
                                   leafletOutput(outputId = "mymap"),
                                   DT::dataTableOutput(outputId = "mapdata")
                         )
                     ),
                     checkboxInput("legend", "Show legend", TRUE)
            ),
            
            tabPanel("Data",
                     
                     h3("County Full Data"),
                     
                     p("Here you can view the full sample dataset used in our analysis"),
                     
                     br(),
                     
                     DT::dataTableOutput(outputId = "mapData")
            ),
            
            tabPanel("Plot",
                     
                     h3("Plotting Parameters"),
                     
                     # Select variable for x-axis 
                     
                     selectInput(inputId = "x", 
                                 label = "x-axis (specify your x variable):",
                                 choices = c("Percent_Democrat", "Percent_Republican", "Never", "Rarely", "Sometimes", "Frequently", "Always"), 
                                 selected = "Percent_Democrat"),
                     
                     # Select variable for y-axis 
                     
                     selectInput(inputId = "y", 
                                 label = "y-axis (specify your y variable):",
                                 choices = c("Percent_Democrat", "Percent_Republican", "Never", "Rarely", "Sometimes", "Frequently", "Always"), 
                                 selected = "Percent_Democrat"),
                     plotOutput(outputId = "scatterplot"),
                     
                     # Select smooth method
                     selectInput(inputId = "method", 
                                 label = "Smooth method",
                                 choices = c("lm", "loess"), 
                                 selected = "l"),
                     
              #       br(), br()
            ),
            
            tabPanel("Documentation",
                     
                     h3("Team"),
                     
                     h5("Mark Nathin & Anqi Liu"),
                     
                     h3("Codebook"),
                     
                     tableOutput("codebook")),
            
        )
    )
)

# Define server function required to create the map
server <- function(input, output, session) {

    pal <- colorFactor(c("green", "orange", "red"), domain = ShinyAppDS$Percent_Democrat)
    
    #create the map
    output$mymap <- renderLeaflet({

        leaflet(ShinyAppDS) %>% 
            setView(lng = -99, lat = 45, zoom = 3)  %>%  #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = ShinyAppDS, popup = (paste("Percent Democrat:", sep = " ", ShinyAppDS$Percent_Democrat, "<br>", 
                                                         "Percent Republican:", 
                                                         ShinyAppDS$Percent_Republican)),
                       label = ~as.character(paste0("County:", sep = " ", ShinyAppDS$county_name)), radius = 35000, 
                       fillOpacity = 0.7, stroke = FALSE)
    })
    
    # Create scatterplot object the plotOutput function is expecting 
    output$scatterplot <- renderPlot({
        x <- input$x
        y <- input$y 
        smoothmethod <- input$method
        
        ggplot(data = ShinyAppDS, aes_string(x = x, y = y)) + 
            geom_point() + geom_smooth(method = smoothmethod)
    })
    
    output$mapData <- DT::renderDataTable(server = FALSE,
                                          DT::datatable(data = ShinyAppDSTable,
                                                        options = list(pageLength = 10), 
                                                        rownames = FALSE,
                                                        colnames = c("State",
                                                                     "County_Name",
                                                                     "Percent Democrat",
                                                                     "Percent Republican",
                                                                     "Never",
                                                                     "Rarely",
                                                                     "Sometimes",
                                                                     "Frequently",
                                                                     "Always"))
    )
    
    output$codebook <- function() {
        codebook %>%
            knitr::kable("html") %>%
            kableExtra::kable_styling("striped", full_width = TRUE)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
