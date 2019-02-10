# This script pulls raw IQVIA prescribing rate data (per 100 persons, scaled to
# county population) obtained from the CDC for each year and digests it into a
# tidy data frame.

library(tidyverse)
options(stringsAsFactors = FALSE)


# Pull data for individual years out of csv files -------------------------

prates_2006 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2006.txt", header = TRUE, sep = "\t")
prates_2007 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2007.txt", header = TRUE, sep = "\t")
prates_2008 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2008.txt", header = TRUE, sep = "\t")
prates_2009 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2009.txt", header = TRUE, sep = "\t")
prates_2010 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2010.txt", header = TRUE, sep = "\t")
prates_2011 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2011.txt", header = TRUE, sep = "\t")
prates_2012 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2012.txt", header = TRUE, sep = "\t")
prates_2013 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2013.txt", header = TRUE, sep = "\t")
prates_2014 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2014.txt", header = TRUE, sep = "\t")
prates_2015 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2015.txt", header = TRUE, sep = "\t")
prates_2016 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2016.txt", header = TRUE, sep = "\t")
prates_2017 <- read.table("rawdata/prescribing-iqvia/prescribing_rates_2017.txt", header = TRUE, sep = "\t")


# Stitch data together ----------------------------------------------------

# Combine prescribing rate data. Since all files have identical variable names
# we simply bind the data frames together, fix the FIPS code string padding
# and rename the state variable to state_postal to fit formatting for merging.
prates_clean <- bind_rows(prates_2006, prates_2007, prates_2008, prates_2009,
                          prates_2010, prates_2011, prates_2012, prates_2013,
                          prates_2014, prates_2015, prates_2016, prates_2017) %>%
  mutate(fips_county_code = str_pad(fips_county_code, 5, "left", pad = "0")) %>% 
  rename(state_postal = state)



# Write out data ----------------------------------------------------------

# To preserve variable classes, we'll save prescribing rates as an RData file.
save(prates_clean, file = "output/prescribing_rates_clean.RData")