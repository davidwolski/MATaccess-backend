library(tidyverse)
library(RColorBrewer)



# Load data ---------------------------------------------------------------

# Load nida data on overdose-related deaths
nida_mortality <- read.csv("rawdata/NIDA/nida-deaths.csv") %>% 
  gather(year, deaths, 2:20) %>% 
  mutate(year = as.numeric(str_remove(year, "[X]")),
         deaths = as.numeric(str_remove(deaths, "[,]")))
  


# Plot data ---------------------------------------------------------------

gg <- nida_mortality %>% 
  ggplot(aes(year, deaths)) +
  # geom_area(aes(fill = Type), position = "stack") +
  # scale_fill_manual(values = c("limegreen", "dodgerblue", "goldenrod1"))+
  geom_line(aes(color = Type), size = 3) +
  scale_color_manual(values = c("goldenrod1", "dodgerblue", "limegreen"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 19)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(size = 24)) +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.line = element_line(colour = "grey20")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 30000)+
  # Create plot labels
  labs( x = "Year", 
        y = "Deaths")



# Save plot ---------------------------------------------------------------
ggsave("plots/deaths-by-drug-type.pdf",gg,
       width = 12, height = 6)

