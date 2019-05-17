# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)


# Read data ---------------------------------------------------------------

df <- read_csv(here("Data/TinaV/Bottle/POP", "P_bottle_TinaV.csv"))


# Data in long format -----------------------------------------------------

df.long <- df %>% 
  select(-c(filtered, P)) %>% 
  gather("parameter", "value", 4:6)



# Check plots -------------------------------------------------------------

df.long %>% 
  filter(station %in% c("P07", "P10"),
         dep < 10) %>%
  ggplot(aes(ymd(ID), value, fill=dep))+
  geom_point(shape=21)+
  scale_fill_viridis_c(name = "Depth (m)")+
  labs(x="Date", y="Concentration (µM)")+
  facet_grid(parameter~station, scales = "free", labeller = label_both)+
  theme_bw()

#### Write summary data file ####

write_csv(df, here("Data/_summarized_data_files", "TinaV_bottle_POP.csv"))

