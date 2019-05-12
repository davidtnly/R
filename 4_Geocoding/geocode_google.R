# Geocoding a csv column of "Address" in R

# Load ggmap
library(ggmap)

# Load data
dataset <- read.csv("R Projects/Geocoding/geodata.csv", stringsAsFactors = FALSE)

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(dataset))
{
  # Print("Working...")
  result <- geocode(dataset$FullAddress[1], output = "latlona", source = "dsk", override_limit = "TRUE") # source = "google"
  dataset$lon[i] <- as.numeric(result[1])
  dataset$lat[i] <- as.numeric(result[2])
  dataset$geoAddress[i] <- as.character(result[3])

}
geocodeQueryCheck()
dataset
# Write a CSV file containing origAddress to the working directory
write.csv(dataset, "R Projects/Geocoding/geocoded.csv", row.names=FALSE)