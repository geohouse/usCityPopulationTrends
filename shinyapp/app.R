# Shiny app for plotting population changes of cities from 2010-2019 in the US
library(shiny)
library(ggmap)
library(dplyr)
library(lubridate)
library(DT)
library(here)

popnData <- readRDS(file = here("US_census_cityPopnData_latlonAdded_070121.Rds"))

us_map_watercolor <- readRDS(file = here("backgroundMap_stamenWatercolor.Rds"))

us_map_toner_background <- readRDS(file = here("backgroundMap_stamenTonerBackground.Rds"))

# Define the UI
ui <- fluidPage(
    titlePanel(title = "Population changes in U.S. cities from 2010-2019"),
    # Can't get the year to display as e.g. '2010', it always shows as '2,010' instead unless I include the month/day too
    # (even if using dates formatted with lubridate)
    fluidRow(
        column(8, offset =  1, sliderInput(inputId = "yearSlider", 
                                               label = h4("Year range for plotting"),
                                               min = as.Date(x = "2010-07-01"), 
                                               max = as.Date(x = "2019-07-01"),
                                               value = c(as.Date(x = "2010-07-01"), as.Date(x = "2019-07-01")), step = 365)),
        column(3, checkboxInput(inputId = "useStateMap", label = h4("Map with state borders?"), value = FALSE))
        # fluidRow(
        #     column(12, "show", offset = 9)
        # ), 
        # fluidRow(
        #     column(12, "high/low", offset = 9)
        # ),
        # fluidRow(
        #     column(12, "cities", offset = 9)
        # )
        
    ),
    fluidRow(
        column(8, plotOutput(outputId = "map")),
        column(4, dataTableOutput(outputId = "selection"))
        
    )
    
    
)

# Define the server logic
server <- function(input, output){
    
    output$map <- renderPlot({
        
        # Get the user selected start and end year for the plot from the slider
        startYear <- as.character(lubridate::year(input$yearSlider[1]))
        endYear <- as.character(lubridate::year(input$yearSlider[2]))
        
        # Make the years into the corresponding column names in the data to use for calculating the percent
        # change column for the user selected year combination
        startYearColName <- paste0("Jul_1_", startYear)
        endYearColName <- paste0("Jul_1_", endYear)
        
        popnData <- popnData %>% mutate(perc_change := (((!!as.name(endYearColName)) - (!!as.name(startYearColName)))/(!!as.name(startYearColName))) * 100)
        
               
        # Make a color code column to be pink for popn losses, and green for popn gains
        # pink #8e0152
        # green #276419
        
        popnData <- popnData %>% mutate(colorHolder = case_when(perc_change < 0 ~ "Negative",
                                                                perc_change >= 0 ~ "Positive"))
        
        plotColors <- c("#8e0152", "#276419")
        if(input$useStateMap == TRUE){
            ggmap(us_map_toner_background) + 
            geom_point(data = popnData, mapping = aes(x = longitude, y = latitude, color = colorHolder, size = perc_change), alpha = 0.6) + 
            scale_color_manual(values = plotColors)
        } else{
            ggmap(us_map_watercolor) + 
                geom_point(data = popnData, mapping = aes(x = longitude, y = latitude, color = colorHolder, size = perc_change), alpha = 0.6) + 
                scale_color_manual(values = plotColors)
        }
    })
    
    
    output$selection <- DT::renderDataTable({
        # Get the user selected start and end year for the plot from the slider
        startYear <- as.character(lubridate::year(input$yearSlider[1]))
        endYear <- as.character(lubridate::year(input$yearSlider[2]))
        
        # Make the years into the corresponding column names in the data to use for calculating the percent
        # change column for the user selected year combination
        startYearColName <- paste0("Jul_1_", startYear)
        endYearColName <- paste0("Jul_1_", endYear)
        
        popnData <- popnData %>% mutate(perc_change := round(x = (((!!as.name(endYearColName)) - (!!as.name(startYearColName)))/(!!as.name(startYearColName))) * 100, digits = 1)) %>%
            select(`Geographic Area`, perc_change)
        
        popnData
    })
    
}

# Run the app!
shinyApp(ui = ui, server = server)




