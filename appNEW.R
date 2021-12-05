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

CombinedCleanApp <- read_csv("CombinedCleanApp.csv")

ShinyAppDSTable <- CombinedCleanApp %>% select(State, county_name, Percent_Democrat, Percent_Republican, 
                                         Never, Rarely, Sometimes, Frequently, Always, White, Black, Asian, UnemploymentRate,
                                         Med_Household_Income, LessHS, HSGrad, Bachelors, GradProfDegree)

codebook <- data.frame(Variable = c("State", "county_name", "lat", "lng", "Percent_Democrat",
                                    "Percent_Republican", "Difference", "Majority", "Never",
                                    "Rarely", "Sometimes", "Frequently",
                                    "Always", "Never_Rarely", "Frequently_Always", "White", "Black", "UnemploymentRate",
                                    "Med_Household_Income", "LessHS", "HSGrad", "Bachelors", "GradProfDegree"),
                       Description = c("State Name", "County Name",
                                       "Latitude coordinate of county",
                                       "Longitude coordinate of county",
                                       "Proportion in county voting for a Democrat in 2020 Presidential election",
                                       "Proportion in county voting for a Republican in 2020 Presidential election",
                                       "Difference between percent voting for a Democrat vs. Republican in 2020 Election",
                                       "Political party with more votes in county in 2020 Presidential election",
                                       "Proportion reporting never wearing a mask in public",
                                       "Proportion reporting rarely wearing a mask in public",
                                       "Proportion reporting sometimes wearing a mask in public",
                                       "Proportion reporting frequently wearing a mask in public",
                                       "Proportion reporting always wearing a mask in public",
                                       "Proportion reporting never OR rarely wearing a mask in public",
                                       "Proportion reporting frequently OR always wearing a mask in public", 
                                       "Proportion of the population that is white",
                                       "Proportion of the population that is black",
                                       "County Unemployment Rate (as of 2019)",
                                       "Median Household Income",
                                       "Proportion of adults without a high school degree",
                                       "Proportion of adults who are high school graduates",
                                       "Proportion of adults with a Bachelors degree",
                                       "Proportion of adults with a Graduate or Professional Degree"))

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
                             
                             br(),
                             selectInput("Majority", "2020 Election Party Vote", choices = c("All",
                                                                                               "Democrat" = "Democrat",
                                                                                               "Republican" = "Republican"),
                                         selected = "All"),
                             
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
                                 choices = c("Percent_Democrat", "Percent_Republican", "Never", "Rarely", "Sometimes", "Frequently", "Always",
                                             "White", "Black", "Asian", "UnemploymentRate", "Med_Household_Income", "LessHS", 
                                             "HSGrad", "Bachelors", "GradProfDegree"), 
                                 selected = "Percent_Democrat"),
                     
                     # Select variable for y-axis 
                     
                     selectInput(inputId = "y", 
                                 label = "y-axis (specify your y variable):",
                                 choices = c("Percent_Democrat", "Percent_Republican", "Never", "Rarely", "Sometimes", "Frequently", "Always",
                                             "White", "Black", "Asian", "UnemploymentRate", "Med_Household_Income", "LessHS", 
                                             "HSGrad", "Bachelors", "GradProfDegree"), 
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

    pal <- colorFactor(c("blue", "orange", "red"), domain = ShinyAppDS$Majority)
    
    #create the map
    output$mymap <- renderLeaflet({
        
        if (input$Majority == "All"){
            ShinyAppDS <- ShinyAppDS
        }
        else if(input$Majority != "All"){
            ShinyAppDS <- ShinyAppDS %>% filter(Majority == input$Majority)
        }

        leaflet(ShinyAppDS) %>% 
            setView(lng = -99, lat = 45, zoom = 3)  %>%  # Setting the view over center of North America
            addTiles() %>% 
            addCircles(data = ShinyAppDS, popup = (paste("Proportion Democrat:", sep = " ", ShinyAppDS$Percent_Democrat, "<br>", 
                                                         "Proportion Republican:", 
                                                         ShinyAppDS$Percent_Republican, "<br>", 
                                                         "Proportion Never or Rarely Masking:", ShinyAppDS$Never_Rarely, "<br>",
                                                         "Proportion Frequently or Always Masking:", ShinyAppDS$Frequently_Always)),
                       label = ~as.character(paste0("County:", sep = " ", ShinyAppDS$county_name)), radius = 35000, color = ~pal(ShinyAppDS$Majority),
                       fillOpacity = 0.7, stroke = FALSE)
    })
    
    # Add Legend  
    observe ({
        proxy <- leafletProxy("mymap", data = ShinyAppDS)
        proxy %>% clearControls()
        if (is.null(input$lengend)){
            proxy %>% addLegend(position = "bottomleft", pal = pal, values = ShinyAppDS$majority,
                                title = "Political Party",
                                opacity = 1)
        }
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
                                                                     "Always",
                                                                     "White",
                                                                     "Black",
                                                                     "Asian",
                                                                     "Unemployment Rate",
                                                                     "Median Household Income",
                                                                     "LessHS",
                                                                     "HSGrad",
                                                                     "Bachelors",
                                                                     "GradProfDegree"))
                                         
                                          
    )
    
    output$codebook <- function() {
        codebook %>%
            knitr::kable("html") %>%
            kableExtra::kable_styling("striped", full_width = TRUE)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
