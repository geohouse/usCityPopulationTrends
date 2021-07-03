# Import the data
library(readr)
#library(ggmap)
library(tmaptools)
popnData_raw <- readr::read_csv(file = "~/GitHub/usCityPopulationTrends/US_census_cityPopnData_forR_070221.csv")

assignLatLon <- function(cityQuery){
    # Geocoding of lat/lon using openStreetMap
    OSM_returned <- tmaptools::geocode_OSM(cityQuery)
    if(!is.null(OSM_returned)){
        OSM_lat <- OSM_returned$coords[2]
        OSM_lon <- OSM_returned$coords[1]
    
        dataReturn <- data.frame("lat" = OSM_lat, "lon" = OSM_lon)
    } else{
        print(paste0("Lookup failed for query: ", cityQuery))
        dataReturn <- data.frame("lat" = NA, "lon" = NA)
    }
    return(dataReturn)
}

popnData_raw$latitude <- vector(mode="numeric", length = nrow(popnData_raw))
popnData_raw$longitude <- vector(mode="numeric", length = nrow(popnData_raw))

# Geocode the city names to convert into lat/lon
for(rowNum in seq(1, nrow(popnData_raw))){
    #print(rowNum)
    
    currCity <- popnData_raw$`Geographic Area`[rowNum]
    returnedGeocode <- assignLatLon(currCity)
    popnData_raw$latitude[rowNum] <- returnedGeocode$lat
    popnData_raw$longitude[rowNum] <- returnedGeocode$lon
    
    # Not supposed to exceed 1 query per second for open streets mapping
    Sys.sleep(2)
    
}

# Clean up the entries for the ones failing the lookup
popnData_raw$`Geographic Area`[which(popnData_raw$`Geographic Area` == "Chapel Hill town, North Carolina")] <- "Chapel Hill, North Carolina"
popnData_raw$`Geographic Area`[which(popnData_raw$`Geographic Area` == "Urban Honolulu CDP, Hawaii")] <- "Honolulu, Hawaii"
popnData_raw$`Geographic Area`[which(popnData_raw$`Geographic Area` == "Summerville town, South Carolina")] <- "Summerville, South Carolina"
popnData_raw$`Geographic Area`[which(popnData_raw$`Geographic Area` == "Methuen Town city, Massachusetts")] <- "Methuen, Massachusetts"

missingFirstRound <- which(is.na(popnData_raw$latitude))

for(missingRow in missingFirstRound){
    
    currCity <- popnData_raw$`Geographic Area`[missingRow]
    returnedGeocode <- assignLatLon(currCity)
    popnData_raw$latitude[missingRow] <- returnedGeocode$lat
    popnData_raw$longitude[missingRow] <- returnedGeocode$lon
    
    # Not supposed to exceed 1 query per second for open streets mapping
    Sys.sleep(2)
    
}

write.table(x = popnData_raw, file = "~/GitHub/usCityPopulationTrends/US_census_cityPopnData_latlonAdded_070121.csv", sep = ",", row.names = F)
saveRDS(object = popnData_raw, file = "~/GitHub/usCityPopulationTrends/US_census_cityPopnData_latlonAdded_070121.Rds")




