# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)


# Load Sensor and HydroC data ---------------------------------------------

Sensor <- read_csv(here::here("Data/_summarized_data_files",
                              "Tina_V_Sensor_Profiles_Transects.csv"),
                   col_types = list("pCO2" = col_double())) %>% 
  rename(pCO2_analog = pCO2)

HC <- read_csv(here::here("Data/_summarized_data_files", "Tina_V_HydroC.csv"))

# Time offset correction ----------------------------------------------

# Time offset was determined by comparing zeroing reads from Sensor and HC
# in the plots produced in the section Time stamp synchronzity below
# before applying this correction

Sensor <- Sensor %>% 
  mutate(day = yday(date_time),
         date_time = if_else(day >= 206 & day <= 220,
                             date_time - 80, date_time - 10)) %>% 
  select(-day)

# Merge Sensor and HydroC data --------------------------------------------

df <- full_join(Sensor, HC) %>% 
  arrange(date_time)

rm(HC, Sensor)


# Interpolate observations to common timestamp ----------------------------

# Interpolate Sensor data to HydroC timestamp

df <-
  df %>%
  mutate(dep = na.approx(dep, na.rm = FALSE, maxgap = 15),
         sal = na.approx(sal, na.rm = FALSE, maxgap = 15),
         tem = na.approx(tem, na.rm = FALSE, maxgap = 15),
         pCO2_analog = na.approx(pCO2_analog, na.rm = FALSE, maxgap = 15),
         pH = na.approx(pH, na.rm = FALSE, maxgap = 15),
         V_pH = na.approx(V_pH, na.rm = FALSE, maxgap = 15),
         O2 = na.approx(O2, na.rm = FALSE, maxgap = 15),
         Chl = na.approx(Chl, na.rm = FALSE, maxgap = 15)) %>% 
  filter(!is.na(dep)) %>% #remove Sensor readings between deployments identified in HC data
  fill(ID, type, station, cast) %>% 
  filter(!is.na(deployment) | is.na(pCO2_analog))

# Time stamp synchronzity -------------------------------------------------
# 
# df <- df %>% 
#   mutate(day = yday(date_time))
# 
# for (dayID in unique(df$day)) {
#   
#   df %>%
#     filter(day == dayID) %>% 
#       ggplot()+
#       geom_point(aes(date_time, pCO2, col="HC"))+
#       geom_point(aes(date_time, dep, col="dep"))+
#       geom_point(aes(date_time, pH, col="pH"))+
#       geom_point(aes(date_time, pCO2_analog, col="Sensor_int"))
#       
#     ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Timing/day",
#                       paste(dayID,"_day_HydroC_merged.jpg", sep="")),
#            width = 10, height = 4)
# }
# 
# 
# for (depID in unique(df$deployment)) {
#   
#   df_dep <- df %>%
#     filter(deployment == depID, Zero == 1)
#   
#   for (zerID in unique(df_dep$Zero_ID)) {
#     
#     df_dep %>%
#       filter(Zero_ID == zerID) %>% 
#       ggplot()+
#       geom_point(aes(date_time, pCO2, col="HC"))+
#       geom_point(aes(date_time, pCO2_analog, col="Sensor_int"))
#     
#     ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Timing/Zeroing",
#                       paste(depID,"_deployment_",zerID,"_Zero_ID_HydroC.jpg", sep="")),
#            width = 10, height = 4)
#     
#   }
# }




# Safe merged data file ---------------------------------------------------

write_csv(df, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv"))



# XXXX Stopped here -------------------------------------------------------








df %>% 
  filter(date_time>ymd_hm("2018-07-23T1800"),
         date_time<ymd_hm("2018-07-23T1830")) %>% 
  ggplot()+
  geom_point(aes(date_time, dep_int, col="dep"))+
  geom_point(aes(date_time, tem_int, col="tem"))+
  geom_point(aes(date_time, pCO2, col="pCO2"))+
  geom_point(aes(date_time, pCO2_RT_median, col="pCO2_RT_median"))

df %>% 
  filter(date_time>ymd_hm("2018-07-23T1800"),
         date_time<ymd_hm("2018-07-23T1830")) %>% 
  ggplot()+
  geom_point(aes(pCO2, dep_int, col="pCO2"))+
  geom_point(aes(pCO2_RT_median, dep_int, col="pCO2_RT_median"))+
  scale_y_reverse()


i <- 7
for (i in seq(2, length(unique(df_HC$deployment)),1)) {

# df_HC %>%
#   filter(deployment == unique(df_HC$deployment)[i]) %>%
#   ggplot()+
#   geom_point(aes(date_time, pCO2))+
#   geom_point(aes(date_time, tem_int, col=station))
# 
#   ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", paste(i,"_deployment_HydroC_Sensor_timeseries.jpg", sep="")),
#                   width = 8, height = 4)

  df %>%
  filter(deployment == unique(df$deployment)[i],
         Zero == 1) %>%
  ggplot()+
  geom_point(aes(date_time, pCO2_analog, col="analog"))+
  geom_point(aes(date_time, pCO2, col="HC"))

  ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", paste(i,"_deployment_HydroC_Sensor_Zeroings.jpg", sep="")),
                  width = 8, height = 4)

}



# Interpolate HydroC data to Sensor timestamp

df_Sensor <-
  df %>%
  mutate(pCO2_int = na.approx(pCO2_corr, na.rm = FALSE)
         # ,
         # pCO2_RT_int = na.approx(pCO2_RT, na.rm = FALSE),
         # pCO2_RT_mean_int = na.approx(pCO2_RT_mean, na.rm = FALSE),
         # pCO2_RT_median_int = na.approx(pCO2_RT_median, na.rm = FALSE)
         ) %>%
  fill(Flush, Zero, deployment) %>%
  filter(!is.na(ID)) 

df_Sensor %>% 
  filter(date_time > ymd_h("2018-07-05T18"),
         date_time < ymd_h("2018-07-07T08")) %>% 
  ggplot()+
  geom_point(aes(date_time, tem, col="tem"))+
  geom_point(aes(date_time, pCO2_int, col="pCO2"))


# for (i in seq(2, length(unique(df_Sensor$deployment)),1)) {
# 
# df_Sensor %>% 
#   filter(deployment == unique(df_Sensor$deployment)[i]) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_int))+
#   geom_point(aes(date_time, tem, col=station))
# 
#   ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", paste(i,"_deployment_Sensor_HydroC_timeseries.jpg", sep="")),
#                   width = 8, height = 4)
#   
# }




# Safe merged data files --------------------------------------------------

write_csv(df_HC, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv"))
write_csv(df_HC, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC_RTcorr_long.csv"))
write_csv(df_Sensor, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv"))
write_csv(df_Sensor, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC_RTcorr_short.csv"))




# ___Old skript parts -----------------------------------------------------

# 
# 
# #### plots to check succesful merging and data comparability ####
# 
# theme_set(theme_bw())
# 
# df %>% 
#   filter(date_time>ymd_hm("2018-07-23T1630"),
#          date_time<ymd_hm("2018-07-23T1930")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_RT_int, col="corr_int"))+
#   geom_point(aes(date_time, pCO2_corr_int, col="RT_int"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements analog output and post drift correction")
# 
# ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2_analog_corr.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(date_time>ymd_hm("2018-07-23T1630"),
#          date_time<ymd_hm("2018-07-23T1930")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr_int, col=type))+
#   scale_color_brewer(palette = "Set1", name="Mode")+
#   labs(x="Date", title="pCO2 post drift correction | Operation mode offset")+
#   ylim(60,90)
# 
# ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Operation_mode_pCO2.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(date_time>ymd_hm("2018-07-23T1630"),
#          date_time<ymd_hm("2018-07-23T1930")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, p_NDIR_int, col=type))+
#   scale_color_brewer(palette = "Set1", name="Mode")+
#   labs(x="Date", title="pCO2 post drift correction | Operation mode offset")
# 
# ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Operation_mode_pNDIR.jpg"),
#        width = 10, height = 6)
# 
# 
# df %>% 
#   filter(date_time>ymd_hm("2018-07-23T1630"),
#          date_time<ymd_hm("2018-07-23T1930")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, T_gas_int, col=type, shape="T_gas"))+
#   geom_point(aes(date_time, tem, col=type, shape="T_insitu"))+
#   scale_color_brewer(palette = "Set1", name="Mode")+
#   scale_shape(name="T sensor")+
#   labs(x="Date", y="Temp (degC)", title="pCO2 post drift correction | Operation mode offset")
# 
# ggsave(here::here("Plots/TinaV/Sensor/HydroC_diagnostics", "Operation_mode_T_gas.jpg"),
#        width = 10, height = 6)
# 
# 
# df <- df %>% 
#   select(date_time, ID, type, station, cast, dep, sal, tem, pH, O2, Chl,
#          Zero, Flush, pCO2 = pCO2_corr_int, pCO2_RT = pCO2_RT_int)
# 
# 
# write_csv(df, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv"))

