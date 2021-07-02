# Shiny app for plotting population changes of cities from 2010-2019 in the US
library(shiny)
library(ggmap)
library(dplyr)
library(lubridate)

popnData <- readRDS(file = "~/GitHub/usCityPopulationTrends/US_census_cityPopnData_latlonAdded_070121.Rds")

us_map_watercolor <- readRDS(file = "~/GitHub/usCityPopulationTrends/backgroundMap_stamenWatercolor.Rds")

# Define the UI
ui <- fluidPage(
    titlePanel(title = "Population changes in U.S. cities from 2010-2019"),
    # Can't get the year to display as e.g. '2010', it always shows as '2,010' instead unless I include the month/day too
    # (even if using dates formatted with lubridate)
    fluidRow(
        column(9, "slider holder", sliderInput(inputId = "yearSlider", 
                                               label = h4("Year range for plotting"),
                                               min = as.Date(x = "2010-07-01"), 
                                               max = as.Date(x = "2019-07-01"),
                                               value = c(as.Date(x = "2010-07-01"), as.Date(x = "2019-07-01")), step = 365)),
        column(3, "show holder"),
        fluidRow(
            column(12, "show", offset = 9)
        ), 
        fluidRow(
            column(12, "high/low", offset = 9)
        ),
        fluidRow(
            column(12, "cities", offset = 9)
        )
        
    ),
    fluidRow(
        column(9, "plot", plotOutput(outputId = "map")),
        column(3, "table", textOutput(outputId = "selection"))
        
    )
    
    
)

# Define the server logic
server <- function(input, output){
    
    output$map <- renderPlot({
        
        # Make a column for the population growth percentage from 2010 to 2019
        popnData <- popnData %>% mutate(perc_change_decade = ((Jul_1_2019 - Jul_1_2010)/Jul_1_2010) * 100)
        
        # Make a color code column to be pink for popn losses, and green for popn gains
        # pink #8e0152
        # green #276419
        
        popnData <- popnData %>% mutate(colorHolder = case_when(perc_change_decade < 0 ~ "Negative",
                                                                perc_change_decade >= 0 ~ "Positive"))
        
        plotColors <- c("#8e0152", "#276419")
        
        ggmap(us_map_watercolor) + 
        geom_point(data = popnData, mapping = aes(x = longitude, y = latitude, color = colorHolder, size = perc_change_decade), alpha = 0.6) + 
        scale_color_manual(values = plotColors)
    })
    
    
    output$selection <- renderText({
        
        firstYear <- lubridate::year(input$yearSlider[1])
        lastYear <- lubridate::year(input$yearSlider[2])
        paste("You've selected: ", firstYear, lastYear)
    })
    
}

# Run the app!
shinyApp(ui = ui, server = server)




