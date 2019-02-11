library(tidyverse)
library(sf)
library(USAboundaries)


# Select which states to subset the shapefiles to -------------------------

# IF we don't want to run the analysis on all states we can subset to a few.
# Note that the selection in include_states should be the same as specfied in
# the prophet-model.R script!

# Either use all states
include_states <- state.abb %>%
  .[!(. %in% c("AK", "HI"))]

# Or subset to a few states
# include_states <- c("MA", "OH", "FL", "CA")


# Load and subset shapefile -----------------------------------------------

# Load county and state data

all_states <-  as_Spatial(us_states(resolution = "high", 
                                    states = include_states))
all_counties <-  as_Spatial(us_counties(resolution = "high",
                                        states = include_states))


# Write out reduced shapefiles --------------------------------------------

save(all_states, all_counties, file = "output/state-county-shapefiles.RData")
