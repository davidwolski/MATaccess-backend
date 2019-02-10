# This script pulls raw data on locations for medication-assisted treatment 
# facilities and digests it into a tidy data frame.

library(tidyverse)
library(tigris)



# Read in treatment facility data -----------------------------------------

treatment <- read.csv("rawdata/treatment-facilities/treatment-facilities.csv")



# Clean data --------------------------------------------------------------

# Clean up treatment facility data. First, we select the columns we are 
# interested, like Name, City, State, Zip Code and Latitude and Longitude. Next,
# we pad the Zip code strings and create shortened lat and lon variables to an 
# accuracy of ~111.32m, i.e. 3 digits. Any facility entries with the same name
# that lie within that distance of each other will be considered a single 
# facility for our analysis purposes.

treatment_clean <- treatment %>% 
  # Select columns
    select("Name" = 1,
         "City" = city,
         "State Abbreviated" = state,
         "Zip" = zip,
         "Latitude" = latitude,
         "Longitude" = longitude) %>%
  # Pad Zip code strings and truncate lat/long for removing of duplicates
  mutate("Zip" = str_pad(Zip, 5, "left", pad = "0"),
         "Latitude_short" = trunc(Latitude*1000)/1000,
         "Longitude_short" = trunc(Longitude*1000)/1000) %>% 
  # Remove duplicate entries
  distinct(City, 
           `State Abbreviated`, 
           Zip, 
           Latitude_short, 
           Longitude_short, 
           .keep_all = TRUE) %>% 
  # Group data by state and city and calculate the number of facilities per city
  group_by(`State Abbreviated`, City) %>% 
  mutate(`Facility Count` = n())



# Write out data ----------------------------------------------------------


# Save treatment facility data
save(treatment_clean, file = "output/treatment-locations.RData")




# Cities data -------------------------------------------------------------

# Pull data on cities and their population sizes to get an estimate of how great
# the need for improved treatment access is for any given city in a county. This
# code isn't implemented in the model (yet), and should therefore be removed
# from the master branch.

cities <- read.csv("~/Downloads/uscitiesv1.4.csv") %>%
  filter(incorporated == "True") %>% 
  select("City" = city,
         "State Abbreviated" = state_id,
         "State" = state_name,
         "FIPS Code" = county_fips,
         "County Short" = county_name,
         "Latitude City" = lat,
         "Longitude City" = lng,
         "City Population" = population,
         "City Population Proper" = population_proper,
         "Zips" = zips) %>% 
  mutate("FIPS Code" = str_pad(`FIPS Code`, 5, "left", pad = "0"))


merged_city_treat <- cities %>% 
  left_join(treatment, by = c("City", "State Abbreviated")) %>% 
  mutate("Facilities per capita" = `Facility Count`/log(`City Population`))

