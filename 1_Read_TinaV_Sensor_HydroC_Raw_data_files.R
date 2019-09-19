# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Read Contros corrected data file ----------------------------------------

df <-
  read_csv2(here::here("Data/TinaV/Sensor/HydroC-pCO2/corrected_Contros",
                       "parameter&pCO2s(method 43).txt"),
            col_names = c("date_time", "Zero", "Flush", "p_NDIR",
                          "p_in", "T_control", "T_gas", "%rH_gas",
                          "Signal_raw", "Signal_ref", "T_sensor",
                          "pCO2", "Runtime", "nr.ave")) %>% 
  mutate(Flush = as.factor(as.character(Flush)),
         Zero = as.factor(as.character(Zero))) %>% 
  select(date_time, Zero, Flush, pCO2)

# Deployments: Identification ---------------------------------------------

df <- df %>% 
  mutate(date_time = dmy_hms(date_time),
         deployment = cumsum(c(TRUE,diff(date_time)>=30)))


# Deployments: Plots ------------------------------------------------------

# for (i in unique(df$deployment)) {
# 
#   df %>%
#     filter(deployment == i) %>%
#     ggplot(aes(date_time, pCO2_corr, col=as.factor(Zero)))+
#     geom_line()
# 
#   ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Deployments",
#                     paste(i,"_deployment_HydroC_timeseries.jpg", sep="")),
#          width = 15, height = 4)
# 
# }
# 
# df %>%
#   ggplot(aes(date_time, pCO2_corr, col=as.factor(deployment)))+
#   geom_line()
# 
# ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Deployments",
#                   "all_deployment_HydroC_timeseries.jpg"),
#        width = 40, height = 4)




# Deployments: Subset relevant periods ------------------------------------

# Subset deployment 29 for high resolution response time determination

# df %>%
#   filter(deployment == 29) %>%
#   write_csv(here::here("Data/_summarized_data_files",
#                        "Tina_V_Sensor_HydroC_RT-experiment_29.csv"))

df <- df %>% 
  filter(deployment %in% c(2,6,9,14,17,21,23,27,29,31,33,34,35,37))
         

# Zeroing ID labelling ----------------------------------------------------

df <- df %>% 
  group_by(Zero) %>% 
  mutate(Zero_ID = as.factor(cumsum(c(TRUE,diff(date_time)>=30)))) %>% 
  ungroup()


# Flush: Identification ---------------------------------------------------

df <- df %>% 
  mutate(Flush = 0) %>% 
  group_by(Zero, Zero_ID) %>% 
  mutate(start = min(date_time),
         duration = date_time - start,
         Flush = if_else(Zero == 0 & duration < 600, "1", "0")) %>% 
  ungroup()


#  Flush: Identify equilibration and internal gas mixing periods ----------

df <- df %>% 
  mutate(mixing = if_else(duration < 20, "mixing", "equilibration"))

Flush <- df %>% 
  filter(Flush == 1, duration <=300)


# Flush: Plot individual periods ------------------------------------------

for (i in unique(Flush$Zero_ID)) {

  Flush %>%
    filter(Zero_ID == i) %>%
    ggplot(aes(duration, pCO2, col=mixing))+
    geom_point() +
    scale_color_brewer(palette = "Set1")

  ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Flush",
                    paste(i,"_Zero_ID_HydroC_Flush.jpg", sep="")),
         width = 10, height = 4)

}

for (i in unique(df$Zero_ID)) {

  df %>%
    filter(Flush == 1, Zero_ID == i) %>%
    ggplot(aes(duration, pCO2, col=mixing))+
    geom_point() +
    scale_color_brewer(palette = "Set1")

  ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Flush",
                    paste(i,"_Zero_ID_HydroC_df.jpg", sep="")),
         width = 10, height = 4)

}


# Clean data: Plot deployments --------------------------------------------

# for (i in unique(df$deployment)) {
# 
#   df %>%
#     filter(Zero ==0, Flush == 0, deployment == i) %>%
#     ggplot(aes(date_time, pCO2_corr, col=Zero_ID))+
#     geom_line()
# 
#   ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Deployments_clean", paste(i,"_deployment_only_HydroC_timeseries.jpg", sep="")),
#          width = 15, height = 4)
# 
# }


# Write summarized data files ----------------------------------------------

df %>% 
  write_csv(here::here("Data/_summarized_data_files",
                       "Tina_V_HydroC.csv"))

Flush %>%
  write_csv(here::here("Data/_summarized_data_files",
                       "Tina_V_HydroC_Flush.csv"))



# X -----------------------------------------------------------------------
# X -----------------------------------------------------------------------
# Old script parts --------------------------------------------------------
# X -----------------------------------------------------------------------
# X -----------------------------------------------------------------------


# #### Read data file as downloaded from HydroC ####
# 
# setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/TinaV/Sensor/HydroC-pCO2")
# files <- list.files(pattern = "*.txt")
# 
# raw <- files %>%
#   map(read.delim, sep=";", dec=",", skip=4, col.names = seq(1,23,1),
#       colClasses = c(rep("character",2), rep("numeric", 21))) %>%
#   reduce(rbind) %>%
#   select(seq(1,22,1))
# 
# rm(files)
# 
# names(raw) <- c("Date",	"Time",	"Weekday",	"P_pump",
#            "p_NDIR",	"p_in",	"I_total",	"U_supply",
#           "Zero",	"Flush", "external_pump",	"Runtime",
#           "Signal_raw",	"Signal_ref",	"T_sensor",
#           "Signal_proc",	"Conc_estimate",	"pCO2_corr",
#           "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")
# 
# raw <- raw %>%
#   mutate(date_time = ymd_hms(paste(Date, Time)))
# 
# 
# #### Check logging frequency and pump switch from Seabird XX? (Power ~1W) to 5T (Power ~8W) ####
# 
# test <- raw %>%
#   filter(date_time > ymd("2018-07-01"),
#          date_time < ymd("2018-07-30"))
# 
# #Pump Switch
# test %>% 
#   filter(date_time > ymd("2018-07-16"),
#          date_time < ymd("2018-07-18")) %>% 
#   ggplot(aes(date_time, P_pump))+
#   geom_point()
# 
# #Logging frequency
# 
# test <- test %>%
#   mutate(frequency = c(0,diff(date_time)))
#   
# test %>%
#   filter(date_time > ymd_hm("2018-07-16 T 12:00"),
#          date_time < ymd_hm("2018-07-19 T 15:00")) %>%
#   ggplot(aes(date_time, frequency, shape=as.factor(Zero), col=as.factor(Flush)))+
#   geom_point()+
#   ylim(0,20)




# #### merge raw and corrected data frames ####
# 
# df = full_join(raw, corr, by="date_time") %>% 
#   arrange(date_time) %>% 
#   slice(seq(1,n(),12))
# 
# 
# #### plots to check succesful read-in and data-quality ####
# 
# theme_set(theme_bw())
# 
# df %>% 
#   filter(Zero.x == 1) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x, col="raw"))+
#   geom_point(aes(date_time, pCO2_corr.y, col="corr"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   ylim(-5,30)+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="Zeroing pre and post drift correction | y-scale limits -5 -> 30 µatm")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_Zeroing.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0,
#          date_time>ymd_h("2018-07-24T01"),
#          date_time<ymd_h("2018-07-24T06")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x, col="raw"))+
#   geom_point(aes(date_time, pCO2_corr.y, col="corr"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0,
#          date_time>ymd_h("2018-07-24T01"),
#          date_time<ymd_h("2018-07-24T06")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x - pCO2_corr.y))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="Delta pCO2 (µatm)",
#        title="Offset pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2_offset.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x - pCO2_corr.y, col=pCO2_corr.x))+
#   scale_color_viridis_c(name="pCO2_corr (uatm)")+
#   labs(x="Date",y="Delta pCO2 (µatm)",
#        title="Offset pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_all_pCO2_offset.jpg"),
#        width = 10, height = 6)
# 
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0) %>% 
#   ggplot()+
#   geom_point(aes(pCO2_corr.x, pCO2_corr.x - pCO2_corr.y, col=date_time),
#              alpha=0.5)+
#   labs(x="pCO2 (µatm)",y="Delta pCO2 (µatm)",
#        title="Offset pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "pCO2_offset.jpg"),
#        width = 10, height = 6)
# 
# #### Determine Pump switch ####
# 
# 
# 
# #### Safe Contros corrected data file ####
# 
# write_csv(corr, here::here("Data/_summarized_data_files",
#                      "Tina_V_Sensor_HydroC.csv"))



