library(tidyverse)
library(data.table)
library(ggrepel)
options(stringsAsFactors = FALSE)


# Map states to postal abbreviations
state_map <- read.csv("rawdata/state-map.csv")

# Read in CDC data
wonder_data <- read.table("rawdata/wonder-data.txt", header = TRUE, fill = TRUE)

merged_data <- read.csv("output/merged-data.csv")

# Clean and summarise CDC data at state level
merged_data_state <- merged_data %>%
  as_tibble() %>% 
  mutate(Deaths = as.numeric(Deaths),
         Population = as.numeric(Population),
         State = as.character(State)) %>%  
  filter(Year == 2016 & Deaths != "Suppressed") %>% 
  group_by(State) %>% 
  summarise(total.state.deaths = sum(Deaths),
            total.state.population = sum(Population)) %>% 
  left_join(state_map, by = c("State" = "state.full"))

# Get first 10 rows of medicare data
cms_head <- read.csv("~/Downloads/medicare-2016.csv", nrows = 10)
colnames(cms_head)

# Read in interesting columns from medicare data
cms_data <- fread("~/Downloads/medicare-2016.csv", 
                  select = c("nppes_provider_state", 
                             "drug_name", 
                             "total_day_supply"))  

# Quick and dirty assessment of drug 
cms_data_2 <- cms_data %>% 
  rename(state.postal = nppes_provider_state,
         drug.name = drug_name,
         total.day.supply = total_day_supply) %>% 
  group_by(state.postal, drug.name) %>% 
  summarise(sum.total.day.supply = sum(total.day.supply)) %>% 
  filter(grepl("CODONE",drug.name)) %>% 
  ungroup() %>% 
  group_by(state.postal) %>% 
  summarise(codone.sum.total = sum(sum.total.day.supply))

# Combine data
combo_data <- left_join(wonder_data, cms_data_2) %>% 
  mutate(day.supply.per.person = codone.sum.total/total.state.population,
         codone.sum.total.in.k = codone.sum.total/10e5)

# Plot the data
gg <- combo_data %>% 
  ggplot(aes(codone.sum.total.in.k, total.state.deaths)) +
  geom_point(color = "grey20", size = 2) +
  theme(panel.background = element_rect(fill = "grey90")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10)) +
  # Create labels for genes
  geom_text_repel(aes(label = state.postal), size = 2, fontface = "bold",
                  color = "grey20") +
  # Create plot labels
  labs( x = "Day's supply of 'codone' drugs in million", 
        y = "Total overdose-related deaths")
ggsave("plots/pills-vs-deaths-by-state.png",gg, 
      width = 5, height = 5)
