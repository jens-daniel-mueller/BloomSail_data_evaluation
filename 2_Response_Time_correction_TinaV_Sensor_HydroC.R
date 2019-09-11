# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tibbletime)


# Load Contros corrected data set -----------------------------------------

HC <- read_csv(here::here("Data/_summarized_data_files", "Tina_V_Sensor_HydroC.csv"))

# Identify deployments and remove irrelevant data -------------------------

# HC <- HC %>% 
#   mutate(deployment = cumsum(c(TRUE,diff(date_time)>=30)),
#          week = week(date_time))
# 
# HC_test <- HC %>% 
#   filter(Flush == 1) %>% 
#   mutate(deployment = cumsum(c(TRUE,diff(date_time)>=30)))
# 
# HC.test %>% 
#   ggplot(aes(date_time, pCO2_corr))+
#   geom_point()+
#   facet_wrap(~deployment, scales = "free")

#i <- unique(HC$week)[1]
# for (i in unique(HC$week)) {
#  
#   HC %>% 
#     filter(week(date_time) == i) %>%
#     ggplot(aes(date_time, pCO2_corr, col=as.factor(deployment)))+
#     geom_line()
#   
#   ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", paste(i,"_HydroC_weekly_timeseries.jpg", sep="")),
#          width = 8, height = 4)
#    
# } 

# HC %>% 
#   ggplot(aes(date_time, pCO2_corr, col=as.factor(deployment)))+
#   geom_line()
# 
# ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_timeseries.jpg"),
#        width = 40, height = 4)

HC %>% 
  filter(deployment == 29) %>%
  ggplot(aes(date_time, pCO2_corr, col=as.factor(deployment)))+
  geom_line()

HC_RT_study <- HC %>% 
  filter(deployment == 29)

HC <- HC %>% 
  filter(deployment %in% c(2,6,9,14,17,21,23,27,31,33,34,35,37))

# for (i in unique(HC_clean$week)) {
#   
#   HC_clean %>% 
#     filter(week(date_time) == i) %>%
#     ggplot(aes(date_time, pCO2_corr, col=as.factor(deployment)))+
#     geom_line()
#   
#   ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", paste(i,"_HydroC_weekly_timeseries_clean.jpg", sep="")),
#          width = 8, height = 4)
#   
# } 

# HC_clean %>%
#   ggplot(aes(date_time, pCO2_corr, col=as.factor(deployment)))+
#   geom_line()
# 
# ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_timeseries_clean.jpg"),
#        width = 40, height = 4)



# Define response time correction approach after Bittig -------------------

RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}



# Apply RT correction -----------------------------------------------------


tau_low <- 79.9
tau_high <- 55.9


HC <- HC %>% 
  arrange(date_time) %>% 
  select(date_time, Zero, Flush, deployment, pCO2 = pCO2_corr) %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))),
         tau = if_else(date_time < ymd_hms("2018-07-17;13:08:34"),
                       80, 55))


# Apply RT correction to entire data set ----------------------------------

# Define functions and window width for rolling mean
window <- 15
rolling_mean   <- rollify(~mean(.x, na.rm = TRUE), window = window)
rolling_median <- rollify(~median(.x, na.rm = TRUE), window = window)

HC <- HC %>%
  group_by(deployment) %>% 
  mutate(pCO2_RT = RT_corr(pCO2, lag(pCO2), dt, tau),
         pCO2_RT = if_else(pCO2_RT %in% c(Inf, -Inf), NaN, pCO2_RT),
         pCO2_RT_mean = rolling_mean(pCO2_RT),
         pCO2_RT_median = rolling_median(pCO2_RT)) %>% 
  ungroup()

# time shift RT corrected data
shift <- as.integer(as.character(window/2))
HC <- HC %>%
  mutate(pCO2_RT_mean = lead(pCO2_RT_mean, shift),
         pCO2_RT_median = lead(pCO2_RT_median, shift))

rm(rolling_median, rolling_mean, shift)
      

unique(HC$deployment)

HC %>% 
  filter(pCO2_RT_mean == Inf)

HC %>% 
  filter(deployment == unique(HC$deployment)[8]) %>% 
  ggplot()+
  geom_line(aes(date_time, pCO2, col="Raw"))+
  geom_line(aes(date_time, pCO2_RT_median, col="median"))+
  geom_line(aes(date_time, pCO2_RT_mean, col="mean"))


# Write RT corrected data to file -----------------------------------------

write_csv(HC, here::here("Data/_summarized_data_files",
                             "Tina_V_Sensor_HydroC_RTcorr.csv"))    
    
    




# ___old part: Testing for restricted time intervals ----------------------


# Subset test data with high and low measurement frequency ----------------

HC %>% 
  filter(date_time > ymd_hm("2018-07-24T1800"),
         date_time < ymd_hm("2018-07-25T0000")) %>%
  ggplot(aes(date_time, pCO2_corr))+
  geom_line()

HC %>% 
  filter(date_time > ymd_hm("2018-07-10T0400"),
         date_time < ymd_hm("2018-07-10T1700")) %>%
  ggplot(aes(date_time, pCO2_corr))+
  geom_line()

high_freq <- HC %>% 
  filter(date_time > ymd_hm("2018-07-24T1800"),
         date_time < ymd_hm("2018-07-25T0000")) %>% 
  mutate(freq = "high")

low_freq <- HC %>% 
  filter(date_time > ymd_hm("2018-07-10T0400"),
         date_time < ymd_hm("2018-07-10T1700")) %>%
  mutate(freq = "low")

HC <- bind_rows(low_freq, high_freq) %>% 
  select(freq, date_time, pCO2 = pCO2_corr)

rm(low_freq, high_freq)



# Define response time correction approach after Bittig -------------------

RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}



# Apply RT correction -----------------------------------------------------

HC <- HC %>% 
  arrange(date_time) %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))))

for (freq in c("high", "low")) {
 for (window in seq(4,34,20)) {
  for (tau in seq(30,105,25)) {

  rolling_mean   <- rollify(mean, window = window)
  rolling_median <- rollify(median, window = window)
    
  temp <- HC %>%
    filter(freq == freq) %>% 
    mutate(tau = tau,
           pCO2_RT = RT_corr(pCO2, lag(pCO2), dt, tau),
           window = window,
           pCO2_RT_mean = rolling_mean(pCO2_RT),
           pCO2_RT_median = rolling_median(pCO2_RT))
  
  shift <- as.integer(as.character(window/2))
  
  temp <- temp %>%
    mutate(pCO2_RT_mean = lead(pCO2_RT_mean, shift),
           pCO2_RT_median = lead(pCO2_RT_median, shift))
   
  if (exists("HC_RT", inherits = FALSE)){
    HC_RT <- bind_rows(HC_RT, temp)
  } else{HC_RT <- temp}
  
  rm(rolling_median, rolling_mean, temp, shift)
  
  }
 }
}


HC_RT %>% 
  filter(date_time > ymd_hm("2018-07-24T2220"),
         date_time < ymd_hm("2018-07-24T2250"),
         pCO2_RT > 0) %>%
  ggplot()+
  geom_point(aes(date_time, pCO2, col="raw"), size=0.5)+
  geom_point(aes(date_time, pCO2_RT, col="RT"), size=0.5)+
  geom_line(aes(date_time, pCO2_RT_mean, col="RT_mean"))+
  geom_line(aes(date_time, pCO2_RT_median, col="RT_median"))+
  scale_y_continuous(breaks = seq(0,2000,20))+
  scale_color_viridis_d()+
  facet_grid(window~tau, scales = "free_x", labeller = label_both)+
  theme_bw()

ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_RTcorr_Bittig_2018-07-24_shallow.pdf"),
       width = 18, height = 12)

HC_RT %>% 
  filter(date_time > ymd_hm("2018-07-24T2020"),
         date_time < ymd_hm("2018-07-24T2110"),
         pCO2_RT > 0) %>%
  ggplot()+
  geom_point(aes(date_time, pCO2, col="raw"), size=0.5)+
  geom_point(aes(date_time, pCO2_RT, col="RT"), size=0.5)+
  geom_line(aes(date_time, pCO2_RT_mean, col="RT_mean"))+
  geom_line(aes(date_time, pCO2_RT_median, col="RT_median"))+
  scale_y_continuous(breaks = seq(0,2000,20))+
  scale_color_viridis_d()+
  facet_grid(window~tau, scales = "free_x", labeller = label_both)+
  theme_bw()

ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_RTcorr_Bittig_2018-07-24_deep.pdf"),
       width = 18, height = 12)

HC_RT %>% 
  filter(date_time > ymd_hm("2018-07-10T0900"),
         date_time < ymd_hm("2018-07-10T1100"),
         pCO2_RT > 0) %>%
  ggplot()+
  geom_point(aes(date_time, pCO2, col="raw"), size=0.5)+
  geom_point(aes(date_time, pCO2_RT, col="RT"), size=0.5)+
  geom_line(aes(date_time, pCO2_RT_mean, col="RT_mean"))+
  geom_line(aes(date_time, pCO2_RT_median, col="RT_median"))+
  scale_y_continuous(breaks = seq(0,2000,20))+
  scale_color_viridis_d()+
  facet_grid(window~tau, scales = "free_x", labeller = label_both)+
  theme_bw()

ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_RTcorr_Bittig_2018-07-10_deep.pdf"),
       width = 18, height = 12)

HC_RT %>% 
  filter(date_time > ymd_hm("2018-07-10T1600"),
         date_time < ymd_hm("2018-07-10T1640"),
         pCO2_RT > 0) %>%
  ggplot()+
  geom_point(aes(date_time, pCO2, col="raw"), size=0.5)+
  geom_point(aes(date_time, pCO2_RT, col="RT"), size=0.5)+
  geom_line(aes(date_time, pCO2_RT_mean, col="RT_mean"))+
  geom_line(aes(date_time, pCO2_RT_median, col="RT_median"))+
  scale_y_continuous(breaks = seq(0,2000,20))+
  scale_color_viridis_d()+
  facet_grid(window~tau, scales = "free_x", labeller = label_both)+
  theme_bw()

ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_RTcorr_Bittig_2018-07-10_shallow.pdf"),
       width = 18, height = 12)


write_csv(HC, here::here("Data/_summarized_data_files",
                     "Tina_V_Sensor_HydroC_RTcorr.csv"))

