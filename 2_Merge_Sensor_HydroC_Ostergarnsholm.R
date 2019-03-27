#### Load required libraries ####

library(tidyverse)
library(lubridate)
library(here)
library(zoo)


#### Load summarized Sensor and HydroC Data ####

Sensor <- read_csv(here::here("Data/_merged_data_files", "BloomSail_Ostergarnsholm_Sensor_track.csv"))

HC <- read_csv(here::here("Data/_summarized_data_files", "Tina_V_Sensor_HydroC.csv"))


#### Merge Sensor and HydroC data ####

df <- full_join(Sensor, HC) %>% 
  arrange(date_time)


# #### Interpolate observations to common timestamp ####

df <-
  df %>%
  mutate(pCO2_corr_int = na.approx(pCO2_corr, na.rm = FALSE),
         p_NDIR_int = na.approx(p_NDIR, na.rm = FALSE),
         T_gas_int = na.approx(T_gas, na.rm = FALSE)) %>% 
  fill(Flush, Zero) %>% 
  filter(!is.na(ID)) 



#### plots to check succesful merging and data comparability ####

theme_set(theme_bw())

df %>% 
  ggplot()+
  geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
  geom_point(aes(date_time, pCO2, col="analog"))+
  scale_color_brewer(palette = "Set1", name="Dataset")+
  labs(x="Date",y="pCO2 (µatm)",
       title="pCO2 measurements analog output and post drift correction")+
  facet_wrap(~ID, scales = "free_x")

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2_analog_corr_OGB.jpg"),
       width = 20, height = 16)


df <- df %>% 
  filter(!(ID=="180718" & date_time < ymd_hm("2018-07-18 T 1910")))

# df %>% 
#   filter(ID=="180718") %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")


df <- df %>% 
  filter(!(ID=="180723" & date_time < ymd_hm("2018-07-23 T 1255")))

# df %>% 
#   filter(ID=="180723") %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")


df <- df %>% 
  filter(!(ID=="180730" & date_time < ymd_hm("2018-07-30 T 1100")))

# df %>%
#   filter(ID=="180730") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")


df <- df %>% 
  filter(!(ID=="180802"& date_time < ymd_hm("2018-08-02 T 1037")))
 
# df %>% 
#   filter(ID=="180802") %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")
  

df <- df %>%
  filter(!(ID=="180806" & date_time < ymd_hm("2018-08-06 T 1441")))

# df %>%
#   filter(ID=="180806") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")


df <- df %>%
  filter(!(ID=="180815" & date_time < ymd_hm("2018-08-15 T 1026")))

# df %>%
#   filter(ID=="180815") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")

df <- df %>%
  filter(!(ID=="180820" & date_time < ymd_hm("2018-08-20 T 1005")))

# df %>%
#   filter(ID=="180820") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")




df <- df %>%
  filter(ID!="180823",
         Flush != 1,
         Zero != 1)





#### Filter remaining effects of of Zeroings, equilibrium not reached in flush period ####

df <- df %>%
  filter(ID!="180706" | 
           date_time < ymd_hm("2018-07-07 T 0117") |
             date_time > ymd_hm("2018-07-07 T 0130"))

# df %>%
#   filter(ID=="180706") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")


df <- df %>%
  filter(ID!="180707" |
           date_time < ymd_hm("2018-07-07 T 0621") |
             date_time > ymd_hm("2018-07-07 T 0626"))

# df %>%
#   filter(ID=="180707") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")


df <- df %>%
  filter(ID!="180709" |
           date_time < ymd_hm("2018-07-09 T 1646") |
             date_time > ymd_hm("2018-07-09 T 1653")) %>% 
  filter(ID!="180709" | date_time > ymd_hm("2018-07-09 T 1210"))

# df %>%
#   filter(ID=="180709") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")



df <- df %>%
  filter(ID!="180725" | date_time < ymd_hm("2018-07-25 T 1700"))

# df %>%
#   filter(ID=="180725") %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2, col="analog"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")+
#   facet_wrap(~ID, scales = "free")



df %>% 
  ggplot()+
  geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
  geom_point(aes(date_time, pCO2, col="analog"))+
  scale_color_brewer(palette = "Set1", name="Dataset")+
  labs(x="Date",y="pCO2 (µatm)",
       title="pCO2 measurements Ostergarnsholm | analog output and post drift correction")+
  facet_wrap(~ID, scales = "free_x")

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2_analog_corr_OGB_clean.jpg"),
       width = 20, height = 16)




df <- df %>% 
  filter(sal>6) %>% 
  select(date_time, ID, lat, lon, station, cast, dep, sal, tem, O2, 
         pCO2 = pCO2_corr_int)


write_csv(df, here::here("Data/_merged_data_files", "BloomSail_Ostergarnsholm_Sensor_track_HydroC.csv"))

