# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tibbletime)
library(scico)


# Load Contros corrected data set -----------------------------------------

df <- read_csv(here::here("Data/_merged_data_files",
                          "BloomSail_Sensor_HydroC.csv"),
               col_types = cols(pCO2 = col_double()))

#unique(df$station)


# Load profile meta data --------------------------------------------------

meta <- read_csv(here::here("Data/_summarized_data_files",
                          "BloomSail_profile_meta.csv"))

# Select profile data only ------------------------------------------------

df <- df %>% 
  filter(type == "P",
         !(station %in% c("PX1", "PX2"))) %>% 
  select(date_time, ID, station, cast, Zero, Flush, dep, pCO2)


# Merge data and meta information -----------------------------------------

df <- full_join(df, meta)
rm(meta)

# Distinct ID and station combinations ------------------------------------

# df %>% 
#   select(ID, station) %>% 
#   distinct() %>% 
#   ggplot(aes(ymd(ID), station))+
#   geom_point()
# 
# df %>% 
#   select(ID, station) %>% 
#   distinct() %>% 
#   write_csv(here::here("Data/_summarized_data_files",
#                        "BloomSail_Stations_IDs.csv"))
#   



# Label disturbed profiles ------------------------------------------------


# df <- df %>%
#   mutate(Q_profiling = 0,
#          Q_profiling = if_else(ID == "180705" & station == "P02" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180709" & station == "P08" & cast == "up"  , 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180716" & station == "P01" & cast == "up"  , 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180718" & station == "P04" & cast == "up"  , 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180718" & station == "P07" & cast == "up"  , 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180718" & station == "P13" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180723" & station == "P04" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180723" & station == "P06" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180723" & station == "P07" & cast == "up"  , 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180723" & station == "P09" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180723" & station == "P13" & cast == "up"  , 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180730" & station == "P08" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180802" & station == "P02" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180815" & station == "P09" & cast == "down", 1, Q_profiling), 
#          Q_profiling = if_else(ID == "180815" & station == "P11" & cast == "up"  , 1, Q_profiling),
#          Q_profiling = as.factor(Q_profiling))
 


df <- df %>%
  group_by(ID, station) %>% 
  mutate(duration = as.numeric(date_time - min(date_time))) %>%
  arrange(date_time)

df <- df %>% 
  mutate(phase = "standby",
         phase = if_else(duration >= start & duration < down & !is.na(down) & !is.na(start),   "down", phase),
         phase = if_else(duration >= down  & duration < lift & !is.na(lift) & !is.na(down ),   "low",  phase),
         phase = if_else(duration >= lift   & duration < up   & !is.na(up  ) & !is.na(lift  ), "mid",  phase),
         phase = if_else(duration >= up    & duration < end  & !is.na(end ) & !is.na(up   ),   "up",   phase))


# Plot profiling depth vs time --------------------------------------------


cast_dep <- df %>% 
  pivot_longer(c(dep, pCO2), names_to = "parameter", values_to = "value")


# cast_dep %>%
#   filter(cast == "down",
#          duration <= 1500) %>% 
#   ggplot(aes(duration, dep))+
#   geom_point()+
#   scale_y_reverse()+
#   #labs(y="Depth [m]", title = str_c("Date: ",i_ID," | Station: ",i_station))+
#   theme_bw()+
#   geom_smooth(method = "lm", formula=y~x-1)


# for(i_ID in unique(cast_dep$ID)){
#   for(i_station in unique(cast_dep$station)){
#     
#     if (nrow(cast_dep %>% filter(ID == i_ID, station == i_station)) > 0){
#       
#       cast_dep %>%
#         filter(ID == i_ID,
#                station == i_station) %>%
#         ggplot(aes(duration, value, shape=Q_profiling, col=phase))+
#         geom_point(size=0.5)+
#         scale_y_reverse()+
#         scale_x_continuous(breaks = seq(0,6000,100))+
#         labs(title = str_c("Date: ",i_ID," | Station: ",i_station))+
#         facet_grid(parameter~., scales = "free_y")+
#         theme_bw()
#       
#       print(str_c(i_ID,"_",i_station,"_.jpg"))
#       ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Response_time_optimization",
#                         str_c(i_ID,"_",i_station,"_cast.jpg")),
#              width = 10, height = 5)
#       
#     }
#     
#   }
# }




   
# Response time correction approach after Bittig --------------------------

RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}


# Response time (tau) values ----------------------------------------------

tau_low <- 79.9
tau_high <- 55.9


# Prepare data set for RT correction --------------------------------------

df <- df %>% 
  filter(Zero == 0, Flush == 0) %>%  
  arrange(date_time) %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))),
         tau = if_else(date_time < ymd_hms("2018-07-17;13:08:34"),
                       tau_low, tau_high))

df <- expand_grid(df, tau_factor = seq(0.7, 1.6, 0.1))

df <- df %>%
  mutate(tau_test = tau*tau_factor)

# Apply RT correction to entire data set ----------------------------------

# Define functions and window width for rolling mean
window <- 15
rolling_mean   <- rollify(~mean(.x, na.rm = TRUE), window = window)
#rolling_median <- rollify(~median(.x, na.rm = TRUE), window = window)


df <- df %>%
  group_by(ID, station, tau_factor) %>% 
  mutate(pCO2_RT = RT_corr(pCO2, lag(pCO2), dt, tau_test),
         pCO2_RT = if_else(pCO2_RT %in% c(Inf, -Inf), NaN, pCO2_RT),
         pCO2_RT_mean = rolling_mean(pCO2_RT)
         #pCO2_RT_median = rolling_median(pCO2_RT)
         ) %>% 
  ungroup()

# time shift RT corrected data
shift <- as.integer(as.character(window/2))
df <- df %>%
  group_by(ID, station, tau_factor) %>% 
  mutate(pCO2_RT_mean = lead(pCO2_RT_mean, shift)) %>% 
  ungroup()#,
         #pCO2_RT_median = lead(pCO2_RT_median, shift))

rm(rolling_mean, shift, tau_high, tau_low, window, RT_corr)
     


# Plot profiles with variable tau factor ----------------------------------

# for(i_ID in unique(df$ID)){
#   for(i_station in unique(df$station)){
# 
#     if (nrow(df %>% filter(ID == i_ID, station == i_station)) > 0){
# 
#       df %>%
#         filter(ID == i_ID,
#                station == i_station,
#                phase %in% c("up", "down")) %>%
#         ggplot()+
#         geom_path(aes(pCO2, dep, linetype = cast))+
#         geom_path(aes(pCO2_RT_mean, dep, linetype = cast), col="red")+
#         scale_y_reverse()+
#         #scale_color_viridis_d()+
#         labs(y="Depth [m]", title = str_c("Date: ",i_ID," | Station: ",i_station))+
#         theme_bw()+
#         facet_wrap(~tau_factor)
# 
#       print(str_c(i_ID,"_",i_station,"_.jpg"))
#       ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Response_time_optimization",
#                         str_c(i_ID,"_",i_station,"_pCO2_RTC.jpg")),
#              width = 15, height = 12)
# 
#     }
# 
#   }
# }




# Offset corrected profiles diagnosis -------------------------------------

RT_diff <- df %>% 
  filter(phase %in% c("down", "up")) %>% 
  mutate(dep_int = as.numeric(as.character( cut(dep, seq(0,40,2),seq(1,39,2))))) %>% 
# ,
#          tau_factor = as.factor(tau_factor)) %>%
  select(ID, station, tau_factor, p_type, dep_int, phase, pCO2, pCO2_RT_mean) %>% 
  group_by(ID, station, tau_factor, dep_int, phase) %>%
  summarise_all("mean", na.rm = TRUE) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(pCO2, pCO2_RT_mean), names_to = "correction") %>% 
  pivot_wider(names_from = phase, values_from = value) %>% 
  mutate(d_pCO2 = up - down,
         mean_pCO2 = (down + up)/2,
         d_pCO2_rel = 100 * d_pCO2 / mean_pCO2)


# for(i_ID in unique(RT_diff$ID)){
#   for(i_station in unique(RT_diff$station)){
# 
#     if (nrow(RT_diff %>% filter(ID == i_ID, station == i_station)) > 0){
# 
#       RT_diff %>%
#         filter(ID == i_ID,
#                station == i_station,
#                correction == "pCO2_RT_mean") %>%
#         arrange(dep_int) %>%
#         ggplot(aes(d_pCO2, dep_int, col=tau_factor))+
#         geom_path()+
#         geom_point()+
#         scale_y_reverse(breaks=seq(0,40,2))+
#         labs(y="Depth [m]", title = str_c("Date: ",i_ID," | Station: ",i_station))+
#         geom_vline(xintercept = 0)+
#         geom_vline(xintercept = c(-10,10), col="red")+
#         theme_bw()
# 
#       print(str_c(i_ID,"_",i_station,"_.jpg"))
#       ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Response_time_optimization",
#                         str_c(i_ID,"_",i_station,"_d_pCO2.jpg")),
#              width = 15, height = 12)
# 
#     }
# 
#   }
# }

# for(i_ID in unique(RT_diff$ID)){
#   for(i_station in unique(RT_diff$station)){
# 
#     if (nrow(RT_diff %>% filter(ID == i_ID, station == i_station)) > 0){
# 
#       RT_diff %>%
#         filter(ID == i_ID,
#                station == i_station,
#                correction == "pCO2_RT_mean") %>%
#         arrange(dep_int) %>% 
#         ggplot()+
#         geom_path(aes(d_pCO2_rel, dep_int, col=tau_factor))+
#         scale_y_reverse()+
#         labs(y="Depth [m]", title = str_c("Date: ",i_ID," | Station: ",i_station))+
#         geom_vline(xintercept = 0)+
#         geom_vline(xintercept = c(-10,10), col="red")+
#         theme_bw()
# 
#       print(str_c(i_ID,"_",i_station,"_.jpg"))
#       ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Response_time_optimization",
#                         str_c(i_ID,"_",i_station,"_d_pCO2_rel.jpg")),
#              width = 15, height = 12)
# 
#     }
# 
#   }
# }


df %>%
  ggplot(aes(pCO2_RT_mean, dep))+
  geom_point()+
  scale_y_reverse()+
  scale_color_viridis_c()

RT_diff %>%
  group_by(ID, station) %>% 
  mutate(max_pCO2 = max(mean_pCO2)) %>% 
  ungroup() %>% 
  filter(tau_factor == 1) %>%
  ggplot(aes(max_pCO2, d_pCO2, col=as.factor(p_type)))+
  geom_point()+
  #scale_color_viridis_d()+
  facet_wrap(~correction)

RT_diff %>%
  filter(tau_factor == 1) %>%
  ggplot(aes(mean_pCO2, d_pCO2, col=dep_int))+
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~correction)






RT_diff_sum <- RT_diff %>% 
  group_by(tau_factor, dep_int, correction, p_type) %>% 
  summarise(mean = mean(d_pCO2, na.rm = TRUE),
            mean_abs = mean(abs(d_pCO2), na.rm = TRUE),
            mean_rel = mean(d_pCO2_rel, na.rm = TRUE),
            mean_rel_abs = mean(abs(d_pCO2_rel), na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = 5:8, names_to = "estimate", values_to = "dpCO2")



RT_diff_sum %>% 
  filter(correction == "pCO2_RT_mean") %>% 
  ggplot()+
  geom_path(aes(dpCO2, dep_int, col=tau_factor))+
  scale_y_reverse()+
  coord_cartesian(xlim = c(-30,30))+
  geom_vline(xintercept = 0)+
  theme_bw()+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = c(-10,10), col="red")+
  facet_grid(estimate~p_type)

RT_diff_sum %>% 
  filter(dep_int < 20, tau_factor == 1, p_type != 4) %>% 
  group_by(estimate, p_type, correction) %>% 
  summarise(mean_dpCO2 = mean(dpCO2)) %>% 
  ungroup() %>% 
  ggplot(aes(correction, mean_dpCO2, fill=estimate))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,40))+
  theme_bw()

RT_diff %>% 
  filter(dep_int < 20) %>% 
  ggplot(aes(as.factor(tau_factor), d_pCO2, fill=correction))+
  geom_violin()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1)+
  facet_grid(.~p_type)+
  theme_bw()

RT_diff_sum %>% 
  filter(dep_int < 20) %>% 
  group_by(estimate, p_type, correction, tau_factor) %>% 
  summarise(mean_dpCO2 = mean(dpCO2)) %>% 
  ungroup() %>% 
  ggplot(aes(tau_factor, mean_dpCO2, col=correction))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1)+
  facet_grid(estimate~p_type)+
  theme_bw()





