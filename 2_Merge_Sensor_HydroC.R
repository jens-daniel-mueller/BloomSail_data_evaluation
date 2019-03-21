#### Load required libraries ####

library(tidyverse)
library(here)



#### Load summarized Sensor and HydroC Data ####

Sensor <- read_csv( here("Data/_summarized_data_files", "Tina_V_Sensor_Profiles_Transects.csv"))
HC <- read_csv( here("Data/_summarized_data_files", "Tina_V_Sensor_HydroC.csv"))


#### Merge Sensor and HydroC data ####

df <- full_join(Sensor, HC) %>% 
  arrange(date_time)


# #### Interpolate track data to times of other observations ####
# 
# df <-
#   df %>% 
#   mutate(Lat = na.approx(Lat, na.rm = FALSE),
#          Lon = na.approx(Lon, na.rm = FALSE))



#### plots to check succesful merging and data comparability ####

theme_set(theme_bw())

df %>% 
  filter(Zero.x == 1) %>% 
  ggplot()+
  geom_point(aes(date_time, pCO2_corr.x, col="raw"))+
  geom_point(aes(date_time, pCO2_corr.y, col="corr"))+
  scale_color_brewer(palette = "Set1", name="Dataset")+
  ylim(-5,30)+
  labs(x="Date",y="pCO2 (µatm)",
       title="Zeroing pre and post drift correction | y-scale limits -5 -> 30 µatm")

ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_Zeroing.jpg"),
       width = 10, height = 6)

df %>% 
  filter(Zero.x == 0, Flush.x == 0,
         Zero.y == 0, Flush.y == 0,
         date_time>ymd_h("2018-07-24T01"),
         date_time<ymd_h("2018-07-24T06")) %>% 
  ggplot()+
  geom_point(aes(date_time, pCO2_corr.x, col="raw"))+
  geom_point(aes(date_time, pCO2_corr.y, col="corr"))+
  scale_color_brewer(palette = "Set1", name="Dataset")+
  labs(x="Date",y="pCO2 (µatm)",
       title="pCO2 measurements pre and post drift correction")

ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2.jpg"),
       width = 10, height = 6)




write_csv(df, here("Data/_merged_data", "Track_TS_Equi_pH.csv"))

