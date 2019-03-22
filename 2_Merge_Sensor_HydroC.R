#### Load required libraries ####

library(tidyverse)
library(lubridate)
library(here)
library(zoo)


#### Load summarized Sensor and HydroC Data ####

Sensor <- read_csv(here::here("Data/_summarized_data_files", "Tina_V_Sensor_Profiles_Transects.csv")) %>% 
  mutate(pCO2 = as.numeric(pCO2))

HC <- read_csv(here::here("Data/_summarized_data_files", "Tina_V_Sensor_HydroC.csv"))


#### Merge Sensor and HydroC data ####

df <- full_join(Sensor, HC) %>% 
  arrange(date_time)

# %>% 
#   slice(seq(1,n(),12))


# #### Interpolate observations to common timestamp ####

df <-
  df %>%
  mutate(pCO2_corr_int = na.approx(pCO2_corr, na.rm = FALSE),
         p_NDIR_int = na.approx(p_NDIR, na.rm = FALSE),
         T_gas_int = na.approx(T_gas, na.rm = FALSE)) %>% 
  filter(!is.na(ID))



#### plots to check succesful merging and data comparability ####

theme_set(theme_bw())

df %>% 
  filter(date_time>ymd_hm("2018-07-23T1630"),
         date_time<ymd_hm("2018-07-23T1930")) %>% 
  ggplot()+
  geom_point(aes(date_time, pCO2_corr_int, col="corr_int"))+
  geom_point(aes(date_time, pCO2, col="analog"))+
  scale_color_brewer(palette = "Set1", name="Dataset")+
  labs(x="Date",y="pCO2 (µatm)",
       title="pCO2 measurements analog output and post drift correction")

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2_analog_corr.jpg"),
       width = 10, height = 6)

df %>% 
  filter(date_time>ymd_hm("2018-07-23T1630"),
         date_time<ymd_hm("2018-07-23T1930")) %>% 
  ggplot()+
  geom_point(aes(date_time, pCO2_corr_int, col=type))+
  scale_color_brewer(palette = "Set1", name="Mode")+
  labs(x="Date", title="pCO2 post drift correction | Operation mode offset")+
  ylim(60,90)

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Operation_mode_pCO2.jpg"),
       width = 10, height = 6)

df %>% 
  filter(date_time>ymd_hm("2018-07-23T1630"),
         date_time<ymd_hm("2018-07-23T1930")) %>% 
  ggplot()+
  geom_point(aes(date_time, p_NDIR_int, col=type))+
  scale_color_brewer(palette = "Set1", name="Mode")+
  labs(x="Date", title="pCO2 post drift correction | Operation mode offset")

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Operation_mode_pNDIR.jpg"),
       width = 10, height = 6)


df %>% 
  filter(date_time>ymd_hm("2018-07-23T1630"),
         date_time<ymd_hm("2018-07-23T1930")) %>% 
  ggplot()+
  geom_point(aes(date_time, T_gas_int, col=type, shape="T_gas"))+
  geom_point(aes(date_time, tem, col=type, shape="T_insitu"))+
  scale_color_brewer(palette = "Set1", name="Mode")+
  scale_shape(name="T sensor")+
  labs(x="Date", y="Temp (degC)", title="pCO2 post drift correction | Operation mode offset")

ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Operation_mode_T_gas.jpg"),
       width = 10, height = 6)


df <- df %>% 
  select(date_time, ID, station, cast, dep, sal, tem, pH, O2, Chl,
         Zero, Flush, pCO2 = pCO2_corr_int)


write_csv(df, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv"))

