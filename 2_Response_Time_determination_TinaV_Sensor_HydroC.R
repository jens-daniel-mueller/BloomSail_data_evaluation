### load required packages ####

library(tidyverse)
library(here)
#library(lubridate)
#library(tibbletime)

#### load data ####

RT1 <- read_csv2(here::here("Data/TinaV/Sensor/HydroC-pCO2/RTs_Contros", "RT1_new.txt"),
                 col_types = cols(`time stamp` = col_datetime(format = "%d.%m.%Y %H:%M:%S"))) %>% 
  mutate(phase = "1")

RT2 <- read_csv2(here::here("Data/TinaV/Sensor/HydroC-pCO2/RTs_Contros", "RT2_new.txt"),
                 col_types = cols(`time stamp` = col_datetime(format = "%d.%m.%Y %H:%M:%S"))) %>% 
  mutate(phase = "2")

RT3 <- read_csv2(here::here("Data/TinaV/Sensor/HydroC-pCO2/RTs_Contros", "RT3_new.txt"),
                 col_types = cols(`time stamp` = col_datetime(format = "%d.%m.%Y %H:%M:%S"))) %>% 
  mutate(phase = "3")

RT <- bind_rows(
  RT1, RT2, RT3
)

rm(RT1, RT2, RT3)

RT <- RT %>% 
  filter(t_63 < 150) %>% 
  mutate(flag = "good",
         flag = case_when(t_63 > 100 ~ "bad: t63",
                          `Stand.dev._t_63` > 4 ~ "bad: StDev",
                          `pCO2_stop/µatm` > 200 ~ "bad: pCO2",
                          TRUE ~ "good"),
         pCO2_stop_int = cut(`pCO2_stop/µatm`, seq(50,5000,100)))

RT %>%
  ggplot(aes(`time stamp`, t_63, col=pCO2_stop_int, shape=phase))+
  geom_point()+
  scale_color_viridis_d()+
  scale_y_continuous(breaks = seq(0,200,20))+
  theme_bw()

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "t63_vs_time_pCO2.jpg"),
       width = 8, height = 4, dpi = 300)

RT %>% 
  ggplot(aes(t_63, fill=pCO2_stop_int))+
  geom_histogram()+
  facet_wrap(~phase, ncol = 1, labeller = label_both)+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks = seq(0,200,20))+
  theme_bw()

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "t63_histogram.jpg"),
       width = 5, height = 5, dpi = 300)

# RT_clean <- RT %>% 
#   filter(t_63 < 100,
#          Stand.dev._t_63 < 4,
#          `pCO2_stop/µatm` < 200)
# 
# 
# RT %>%
#   ggplot(aes(`time stamp`, t_63, col=flag, shape=phase))+
#   geom_point()+
#   scale_color_brewer(palette = "Set1")
# 
# 
# RT_clean %>%
#   ggplot(aes(Stand.dev._t_63, t_63, col=`pCO2_stop/µatm`, shape=phase))+
#   geom_point()+
#   scale_color_viridis_c()+
#   facet_wrap(~phase)
# 
# RT %>%
#   ggplot(aes(`pCO2_stop/µatm`, t_63))+
#   geom_point()+
#   scale_color_viridis_c()+
#   facet_wrap(~phase)



