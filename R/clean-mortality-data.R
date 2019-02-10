# This script pulls raw mortality data from the CDC for each year and digests it
# into a tidy data frame.

library(tidyverse)
options(stringsAsFactors = FALSE)



# Pull mortality data -----------------------------------------------------


mortality <- read.table("rawdata/cdc-wonder/wonder-data.txt", 
                        header = TRUE, sep = "\t") 



# Clean data --------------------------------------------------------------

# Clean up mortality data and separate counties from state abbreviations.
# County suffixes vary in number of words (eg. County, Census area), so we 
# leave them untouched to simplify. After that, we rename the state variable
# to fit our merging schema and recode deaths and population numbers to use
# NAs for missing and suppressed data points. Finally, we remove some leading
# whitespaces and fix the FIPS code string padding.
mortality_clean <-  mortality %>% 
  as_tibble() %>%
  # Split county and state names
  separate(county, into = c("county", "state_postal"), sep = ",") %>% 
  rename(state_full = state) %>% 
  # Recode deaths and population variables
  mutate(deaths = as.numeric(ifelse(deaths == "Suppressed", NA, 
                         ifelse(deaths == "Missing", NA, deaths))),
         population = as.numeric(ifelse(population == "Missing", 
                                        NA, population))) %>% 
  # Remove whitespace
  mutate(state_postal = str_trim(state_postal, "left")) %>% 
  # Fix padding
  mutate(fips_county_code = str_pad(fips_county_code, 5, "left", pad = "0")) 
  


# Write out data ----------------------------------------------------------

# To preserve variable classes, we'll save mortality data as an RData file.
save(mortality_clean, file = "output/mortality_clean.RData")

