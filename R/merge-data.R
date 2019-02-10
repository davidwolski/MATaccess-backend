# This script merges mortality, prescribing rates and sociodemographic data and
# performs some feature engineering.

library(tidyverse)
options(stringsAsFactors = FALSE)


# Load cleaned data sources ------------------------------------------------

load("output/mortality_clean.RData")
load("output/prescribing_rates_clean.RData")
load("output/acs_data_clean.RData")


# Merge data --------------------------------------------------------------

# Let's merge all three data sets by their overlapping features, which are year,
# FIPS county code, abbreviated state names (and full state names for
# census/mortality data). We ignore county names for matching, since they're
# inconsistently coded. After that we subset the data to years that are common
# to all data sets (Census data is the most restricted) and calculate of
# prescriptions from prescribing rate, death rate from deaths, and ethnic
# majority based on race percentages. Finally we drop the county name columns we
# don't want to keep.

merged <- mortality_clean %>% 
  # Join
  left_join(prates_clean, 
            by = c("year", "fips_county_code", "state_postal")) %>% 
  rename(county_mortality = county.x, county_prescribing = county.y) %>% 
  left_join(acs_data_clean, 
            by = c("year", "fips_county_code", "state_full", "state_postal")) %>%
  rename(county_census = county) %>% 
  # Filter
  filter(year %in% unique(acs_data_clean$year)) %>% 
  # Mutate
  dplyr::mutate(
    prescriptions = round(prescribing_rate / 100 * population),
    death_rate = (deaths / population) * 10e3,
    ethnic_majority = ifelse(race_white_perc >=50, "white",
                             ifelse(race_black_perc >= 50, "black",
                                    ifelse((race_white_perc + race_black_perc) < 50, 
                                           "other", "mixed")))
  ) %>%
  # drop columns
  select(-county_prescribing, -county_census)


# Write out data ----------------------------------------------------------

# To preserve variable classes, we save merged data as an RData file.
save(merged, file = "output/merged-data.RData")

