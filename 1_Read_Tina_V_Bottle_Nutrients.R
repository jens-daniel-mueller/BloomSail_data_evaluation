#### load required packages ####

library(tidyverse)
library(lubridate)
library(here)


#### read data set from relative path ####
df <- read_csv(here("Data/Tina_V/Bottle/Nutrients", "nut_jens_JM.csv"))

df <-
df %>% 
  mutate(date = substr(datelabel, 1, 6),
         label = substr(datelabel, 7, 9),
         datelabel = NULL,
         date = ymd(date))


df.long <-
  df %>% 
  gather("parameter", "value", 2:6)


df.long %>% 
  filter(label %in% c("P07", "P10"),
         parameter %in% c("NO23", "PO4")) %>% 
  ggplot(aes(date, value, fill=dep))+
  geom_point(shape=21)+
  scale_fill_viridis_c(name = "Depth (m)")+
  labs(x="Date", y="Concentration (µM)")+
  facet_grid(parameter~label, scales = "free")+
  theme_bw()

ggsave(here("Plots/Tina_V/Bottle/Nutrients", "BloomSail_Nuts_P07-P10.jpg"))

