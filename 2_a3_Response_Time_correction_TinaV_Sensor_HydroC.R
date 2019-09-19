# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tibbletime)


# Load Contros corrected data set -----------------------------------------

df <- read_csv(here::here("Data/_merged_data_files",
                          "BloomSail_Sensor_HydroC.csv"))


# Response time correction approach after Bittig --------------------------

RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}


# Response time (tau) values ----------------------------------------------

tau_low <- 79.9 *1.2
tau_high <- 55.9 *1.2
# tau_low <- 79.9
# tau_high <- 55.9


# Prepare data set for RT correction --------------------------------------

df_full <- df %>% 
  select(date_time, deployment, Zero_ID, Zero, Flush, pCO2 = pCO2_corr)

df <- df %>% 
  filter(Zero == 0, Flush == 0) %>% 
  select(date_time, deployment, Zero_ID, Zero, Flush, pCO2 = pCO2_corr) %>% 
  arrange(date_time) %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))),
         tau = if_else(date_time < ymd_hms("2018-07-17;13:08:34"),
                       tau_low, tau_high))


# Apply RT correction to entire data set ----------------------------------

# Define functions and window width for rolling mean
window <- 15
rolling_mean   <- rollify(~mean(.x, na.rm = TRUE), window = window)
rolling_median <- rollify(~median(.x, na.rm = TRUE), window = window)


df <- df %>%
  group_by(deployment) %>% 
  mutate(pCO2_RT = RT_corr(pCO2, lag(pCO2), dt, tau),
         pCO2_RT = if_else(pCO2_RT %in% c(Inf, -Inf), NaN, pCO2_RT),
         pCO2_RT_mean = rolling_mean(pCO2_RT),
         pCO2_RT_median = rolling_median(pCO2_RT)) %>% 
  ungroup()

# time shift RT corrected data
shift <- as.integer(as.character(window/2))
df <- df %>%
  mutate(pCO2_RT_mean = lead(pCO2_RT_mean, shift),
         pCO2_RT_median = lead(pCO2_RT_median, shift))

rm(rolling_median, rolling_mean, shift, tau_high, tau_low, window, RT_corr)
     
df <- full_join(df_full, df)
rm(df_full)

# Plot RT corrected data by deployment and Zero_ID ------------------------

# for (depID in unique(df$deployment)) {
#   
#   df_dep <- df %>%
#     filter(deployment == depID)
#   
#   for (zerID in unique(df_dep$Zero_ID)) {
#     
#     df_dep %>%
#       filter(Zero_ID == zerID) %>% 
#       ggplot()+
#       geom_line(aes(date_time, pCO2, col="Raw"))+
#       geom_line(aes(date_time, pCO2_RT_median, col="median"))+
#       geom_line(aes(date_time, pCO2_RT_mean, col="mean"))
#     
#     ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Response_time_corection",
#                       paste(depID,"_deployment_",zerID,"_Zero_ID_HydroC_pCO2_RTcorr.jpg", sep="")),
#            width = 10, height = 4)
#     
#   }
# }
#     

# Write RT corrected data to file -----------------------------------------

write_csv(df, here::here("Data/_summarized_data_files",
                             "Tina_V_Sensor_HydroC_RTcorr.csv"))    
    
    



# X -----------------------------------------------------------------------
# X -----------------------------------------------------------------------
# ___old part: Testing for restricted time intervals ----------------------
# X -----------------------------------------------------------------------
# X -----------------------------------------------------------------------


# Subset test data with high and low measurement frequency ----------------

df %>% 
  filter(date_time > ymd_hm("2018-07-24T1800"),
         date_time < ymd_hm("2018-07-25T0000")) %>%
  ggplot(aes(date_time, pCO2_corr))+
  geom_line()

df %>% 
  filter(date_time > ymd_hm("2018-07-10T0400"),
         date_time < ymd_hm("2018-07-10T1700")) %>%
  ggplot(aes(date_time, pCO2_corr))+
  geom_line()

high_freq <- df %>% 
  filter(date_time > ymd_hm("2018-07-24T1800"),
         date_time < ymd_hm("2018-07-25T0000")) %>% 
  mutate(freq = "high")

low_freq <- df %>% 
  filter(date_time > ymd_hm("2018-07-10T0400"),
         date_time < ymd_hm("2018-07-10T1700")) %>%
  mutate(freq = "low")

df <- bind_rows(low_freq, high_freq) %>% 
  select(freq, date_time, pCO2 = pCO2_corr)

rm(low_freq, high_freq)



# Define response time correction approach after Bittig -------------------

RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}



# Apply RT correction -----------------------------------------------------

df <- df %>% 
  arrange(date_time) %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))))

for (freq in c("high", "low")) {
 for (window in seq(4,34,20)) {
  for (tau in seq(30,105,25)) {

  rolling_mean   <- rollify(mean, window = window)
  rolling_median <- rollify(median, window = window)
    
  temp <- df %>%
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
   
  if (exists("df_RT", inherits = FALSE)){
    df_RT <- bind_rows(df_RT, temp)
  } else{df_RT <- temp}
  
  rm(rolling_median, rolling_mean, temp, shift)
  
  }
 }
}


df_RT %>% 
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

df_RT %>% 
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

df_RT %>% 
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

df_RT %>% 
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


write_csv(df, here::here("Data/_summarized_data_files",
                     "Tina_V_Sensor_HydroC_RTcorr.csv"))

