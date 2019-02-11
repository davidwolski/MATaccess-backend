library(tidyverse)
library(data.table)
library(RColorBrewer)
library(ggrepel)
library(corrplot)
options(stringsAsFactors = FALSE)



# Load data ---------------------------------------------------------------

load("output/merged-data.RData")



# Plot prescribing rates vs mortality -------------------------------------


merged_state <- merged %>%
  filter(year == 2017) %>% 
  mutate(prescriptions = (prescribing_rate/100)*population) %>% 
  group_by(state_postal) %>% 
  summarise(state.deaths = sum(deaths, na.rm = TRUE),
            state.population = sum(population, na.rm = TRUE),
            state.prescriptions = sum(prescriptions, na.rm = TRUE)) %>% 
  mutate(state.prescription.rate = (state.prescriptions/state.population)*100,
         state.death.rate = (state.deaths/state.population)*10000)

  
# Plot the data
gg <- merged_state %>% 
  ggplot(aes(state.prescription.rate, state.death.rate)) +
  geom_point(color = "grey20", size = 2) +
  theme(panel.background = element_rect(fill = "grey90")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text = element_text(size = 10)) +
  # Create labels for genes
  geom_text_repel(aes(label = state_postal), size = 2, fontface = "bold",
                  color = "grey20") +
  # Create plot labels
  labs( x = "Prescribing rate per 100", 
        y = "Mortality rate per 10,000")
ggsave("plots/pills-vs-deaths-by-state.pdf",gg,
       width = 5, height = 5)



# Plot correlations of variables ------------------------------------------

merged_2017 <- merged %>% 
  # filter(year == 2017) %>% 
  select(6:18) %>% 
  select(-deaths, -population)
  

m <- cor(merged_2017, use = "na.or.complete")

cols <- colorRampPalette(rev(brewer.pal(9, "RdBu")))
corrplot(m, method = "color", order = "hclust", col = cols(100))
corrplot(m, 
         method="color", 
         col=cols(200),  
         type="upper", 
         order="hclust", 
         # addCoef.col = "black", # Add coefficient of correlation
         tl.col="black",
         tl.srt=45, #Text label color and rotation
         # Combine with significance
         # p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


