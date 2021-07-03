# Plotting the data on a map after the data clean-up and the lat/lon lookups

library("ggmap")
library("dplyr")

popnData <- readRDS(file = "~/GitHub/usCityPopulationTrends/US_census_cityPopnData_latlonAdded_070121.Rds")

# Given as lower left lon, lower left lat, upper right lon, upper right lat
us_boundBox <- c(-126, 24, -66, 50)

# Working but commented out
# ======

# # Toner with no labels, but does have state boundaries
# us_map_toner_background <- ggmap::get_stamenmap(bbox = us_boundBox, crop = T, zoom = 5, color = "color", maptype = "toner-background", where = "~/GitHub/usCityPopulationTrends/backgroundMap_stamen_watercolor")
# # Watercolor with no state boundaries
# us_map_watercolor <- ggmap::get_stamenmap(bbox = us_boundBox, crop = T, zoom = 5, color = "color", maptype = "watercolor", where = "~/GitHub/usCityPopulationTrends/backgroundMap_stamen_watercolor")
# 
# 
# saveRDS(object = us_map_toner_background, file = "~/GitHub/usCityPopulationTrends/backgroundMap_stamenTonerBackground.Rds")
# saveRDS(object = us_map_watercolor, file = "~/GitHub/usCityPopulationTrends/backgroundMap_stamenWatercolor.Rds")

# ========

us_map_toner_background <- readRDS(file = "~/GitHub/usCityPopulationTrends/backgroundMap_stamenTonerBackground.Rds")

us_map_watercolor <- readRDS(file = "~/GitHub/usCityPopulationTrends/backgroundMap_stamenWatercolor.Rds")


ggmap(us_map_toner_background)

# Make a column for the population growth percentage from 2010 to 2019
popnData <- popnData %>% mutate(perc_change_decade = ((Jul_1_2019 - Jul_1_2010)/Jul_1_2010) * 100)

# Make a color code column to be pink for popn losses, and green for popn gains
# pink #8e0152
# green #276419

popnData <- popnData %>% mutate(colorHolder = case_when(perc_change_decade < 0 ~ "Negative",
                                   perc_change_decade >= 0 ~ "Positive"))

plotColors <- c("#8e0152", "#276419")

mappedPoints <- ggmap(us_map_watercolor) + 
    geom_point(data = popnData, mapping = aes(x = longitude, y = latitude, color = colorHolder, size = perc_change_decade), alpha = 0.6) + 
    scale_color_manual(values = plotColors)


#test <- "2011"
#test2 <- paste0("Jul_1_", test)
#selected <- popnData %>% select(all_of(test2))
