# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)


# Read data ---------------------------------------------------------------

POP <- read_csv(here("Data/_summarized_data_files", "TinaV_bottle_POP.csv")) %>% 
  select(-c(P, filtered)) %>% 
  filter(!is.na(TP)) %>% 
  mutate(PP = if_else(PP < 0, NaN, PP))

DOC <- read_csv(here("Data/_summarized_data_files", "TinaV_bottle_DOC-DN.csv")) %>% 
  select(-Comment)

POC <- read_csv(here("Data/_summarized_data_files", "TinaV_bottle_POC.csv"))

df <- full_join(DOC, POC)
df <- full_join(df, POP)

df <- df %>% 
  filter(station %in% c("P07", "P10"))

df <- df %>% 
  mutate(POC_to_P = POC/PP,
         POC_to_N = POC/PON)

# Data in long format -----------------------------------------------------

df.long <- df %>% 
  gather("parameter", "value", 5:13)



# Check plots -------------------------------------------------------------

df.long %>% 
  filter(station %in% c("P07", "P10")) %>%
  ggplot(aes(ymd(ID), value, fill=as.factor(dep), col=as.factor(dep)))+
  geom_point(shape=21)+
  geom_line()+
  scale_fill_viridis_d(name = "Depth (m)")+
  scale_color_viridis_d(name = "Depth (m)")+
  labs(x="Date", y="Concentration (µM)")+
  facet_grid(parameter~station, scales = "free", labeller = label_both)+
  theme_bw()

#### Write summary data file ####

write_csv(df, here("Data/_merged_data_files", "BloomSail_TinaV_Bottle_nuts.csv"))

