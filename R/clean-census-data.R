# This script pulls raw sociodemographic data from the American Community Survey
# (5-year estimates) for each year and digests it into a tidy data frame.

library(tidyverse)
options(stringsAsFactors = FALSE)


# Pull and merge data for individual years out of csv files -------------------

# 2017 pull
dp02_2017 <- read.csv("rawdata/census/census-2017/ACS_17_5YR_DP02/ACS_17_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC95)
dp03_2017 <- read.csv("rawdata/census/census-2017/ACS_17_5YR_DP03/ACS_17_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC12, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC176)
dp05_2017 <- read.csv("rawdata/census/census-2017/ACS_17_5YR_DP05/ACS_17_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC24, 
                race_white_perc = HC03_VC54, 
                race_black_perc = HC03_VC55)  

# 2017 merge 
acs_data_2017 <- dp02_2017 %>% 
  left_join(dp03_2017) %>% 
  left_join(dp05_2017) %>%  
  dplyr::mutate(year = 2017) %>% 
  .[-1,]


# 2016 pull
dp02_2016 <- read.csv("rawdata/census/census-2016/ACS_16_5YR_DP02/ACS_16_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC95)
dp03_2016 <- read.csv("rawdata/census/census-2016/ACS_16_5YR_DP03/ACS_16_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC12, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC176)
dp05_2016 <- read.csv("rawdata/census/census-2016/ACS_16_5YR_DP05/ACS_16_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC23, 
                race_white_perc = HC03_VC49, 
                race_black_perc = HC03_VC50)  

# 2016 merge
acs_data_2016 <- dp02_2016 %>% 
  left_join(dp03_2016) %>% 
  left_join(dp05_2016) %>% 
  dplyr::mutate(year = 2016) %>% 
  .[-1,]


# 2015 pull
dp02_2015 <- read.csv("rawdata/census/census-2015/ACS_15_5YR_DP02/ACS_15_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC95)
dp03_2015 <- read.csv("rawdata/census/census-2015/ACS_15_5YR_DP03/ACS_15_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC12, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC176)
dp05_2015 <- read.csv("rawdata/census/census-2015/ACS_15_5YR_DP05/ACS_15_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC23, 
                race_white_perc = HC03_VC49, 
                race_black_perc = HC03_VC50)  

# 2015 merge 
acs_data_2015 <- dp02_2015 %>% 
  left_join(dp03_2015) %>% 
  left_join(dp05_2015) %>% 
  dplyr::mutate(year = 2015) %>% 
  .[-1,]


# 2014 pull
dp02_2014 <- read.csv("rawdata/census/census-2014/ACS_14_5YR_DP02/ACS_14_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC95)
dp03_2014 <- read.csv("rawdata/census/census-2014/ACS_14_5YR_DP03/ACS_14_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC12, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC176)
dp05_2014 <- read.csv("rawdata/census/census-2014/ACS_14_5YR_DP05/ACS_14_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC23, 
                race_white_perc = HC03_VC49, 
                race_black_perc = HC03_VC50)  

# 2014 merge
acs_data_2014 <- dp02_2014 %>% 
  left_join(dp03_2014) %>% 
  left_join(dp05_2014) %>% 
  dplyr::mutate(year = 2014) %>% 
  .[-1,]


# 2013 pull
dp02_2013 <- read.csv("rawdata/census/census-2013/ACS_13_5YR_DP02/ACS_13_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC95)
dp03_2013 <- read.csv("rawdata/census/census-2013/ACS_13_5YR_DP03/ACS_13_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC12, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC176)
dp05_2013 <- read.csv("rawdata/census/census-2013/ACS_13_5YR_DP05/ACS_13_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC23, 
                race_white_perc = HC03_VC49, 
                race_black_perc = HC03_VC50)  

# 2013 merge
acs_data_2013 <- dp02_2013 %>% 
  left_join(dp03_2013) %>% 
  left_join(dp05_2013) %>% 
  dplyr::mutate(year = 2013) %>% 
  .[-1,]


# 2012 pull
dp02_2012 <- read.csv("rawdata/census/census-2012/ACS_12_5YR_DP02/ACS_12_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC93)
dp03_2012 <- read.csv("rawdata/census/census-2012/ACS_12_5YR_DP03/ACS_12_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC13, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC171)
dp05_2012 <- read.csv("rawdata/census/census-2012/ACS_12_5YR_DP05/ACS_12_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC21, 
                race_white_perc = HC03_VC43, 
                race_black_perc = HC03_VC44)  

# 2012 merge
acs_data_2012 <- dp02_2012 %>% 
  left_join(dp03_2012) %>% 
  left_join(dp05_2012) %>% 
  dplyr::mutate(year = 2012) %>% 
  .[-1,]


# 2011 pull
dp02_2011 <- read.csv("rawdata/census/census-2011/ACS_11_5YR_DP02/ACS_11_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC93)
dp03_2011 <- read.csv("rawdata/census/census-2011/ACS_11_5YR_DP03/ACS_11_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC13, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC171)
dp05_2011 <- read.csv("rawdata/census/census-2011/ACS_11_5YR_DP05/ACS_11_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC21, 
                race_white_perc = HC03_VC43, 
                race_black_perc = HC03_VC44)  

# 2011 merge
acs_data_2011 <- dp02_2011 %>% 
  left_join(dp03_2011) %>% 
  left_join(dp05_2011) %>% 
  dplyr::mutate(year = 2011) %>% 
  .[-1,]


# 2010 pull
dp02_2010 <- read.csv("rawdata/census/census-2010/ACS_10_5YR_DP02/ACS_10_5YR_DP02_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                education_perc = HC03_VC93)
dp03_2010 <- read.csv("rawdata/census/census-2010/ACS_10_5YR_DP03/ACS_10_5YR_DP03_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                unemployed_perc = HC03_VC13, 
                income_median = HC01_VC85, 
                poverty_perc = HC03_VC171)
dp05_2010 <- read.csv("rawdata/census/census-2010/ACS_10_5YR_DP05/ACS_10_5YR_DP05_with_ann.csv") %>% 
  dplyr::select(GEO.id2, GEO.display.label, 
                sex_male_perc = HC03_VC04, 
                age_median = HC01_VC21, 
                race_white_perc = HC03_VC43, 
                race_black_perc = HC03_VC44)  

# 2010 merge
acs_data_2010 <- dp02_2010 %>% 
  left_join(dp03_2010) %>% 
  left_join(dp05_2010) %>% 
  dplyr::mutate(year = 2010) %>% 
  .[-1,]


# Stitch data together ----------------------------------------------------

# Read in mapping file for names and abbreviations
state_map <- read.csv("rawdata/state-mapping.csv")

# Combine year-specific data and separate counties from state abbreviations.
# County suffixes vary in number of words (eg. County, Census area), so we leave
# them untouched to simplify. We also rename some variables, fix the padding on
# the FIPS county codes, remove some leading whitespace, and change the variable
# type to numeric for all numeric variables. Finally, we join the data with the
# state mappings.
acs_data_clean <- bind_rows(acs_data_2010,
                            acs_data_2011,
                            acs_data_2012,
                            acs_data_2013,
                            acs_data_2014,
                            acs_data_2015,
                            acs_data_2016,
                            acs_data_2017) %>%
  # Split county and state names
  separate(GEO.display.label, into = c("county", "state_full"), sep = ",") %>%
  # Rename
  dplyr::rename(fips_county_code = GEO.id2) %>% 
  # String padding and whitespace trimming
  dplyr::mutate(fips_county_code = str_pad(fips_county_code, 5, "left", pad = "0")) %>% 
  dplyr::mutate(state_full = str_trim(state_full, "left")) %>% 
  # Adjust type for numeric variables
  mutate_at(4:11, funs(as.numeric)) %>% 
  # Join state mapping
  left_join(state_map)

# Call describe on data to get some summary stats on all variables
Hmisc::describe(acs_data_clean)


# Write out data ----------------------------------------------------------

# To preserve variable classes, we'll save ACS variables as an RData file.
save(acs_data_clean, file = "output/acs_data_clean.RData")
