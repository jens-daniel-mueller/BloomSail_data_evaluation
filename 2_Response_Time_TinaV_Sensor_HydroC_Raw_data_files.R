### load required packages ####

library(tidyverse)
library(here)
library(lubridate)

#### load data ####

HC <- read_csv(here::here("Data/_summarized_data_files", "Tina_V_Sensor_HydroC.csv"))

# HC <- HC %>% 
#   filter(date_time > ymd_hm("2018-07-10T0900"),
#          date_time < ymd_hm("2018-07-10T1630"))

# HC %>% 
# ggplot(aes(date_time, pCO2_corr))+
#   geom_line()


#### define response time parameters ####

tau <- 55

RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}


#### Apply RT correction ####

HC <- HC %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))),
         pCO2_RT = RT_corr(pCO2_corr, lag(pCO2_corr), dt, tau))

# HC %>% 
#   filter(dt < 20) %>% 
#   ggplot(aes(date_time, dt))+
#   geom_point()

HC %>% 
  filter(date_time > ymd_hm("2018-07-24T0200"),
        date_time < ymd_hm("2018-07-24T0500"),
        pCO2_RT > 0) %>%
  ggplot()+
  #geom_line(aes(date_time, pCO2_RT, col="RT_corr"))+
  geom_point(aes(date_time, pCO2_RT, col="RT_corr"), size=0.5)+
  geom_line(aes(date_time, pCO2_corr, col="raw"))+
  scale_color_brewer(palette = "Set1")+
  theme_bw()

ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_RTcorr_Bittig_2018-07-24.jpg"),
       width = 13, height = 3)

HC %>% 
    filter(date_time > ymd_hm("2018-07-10T0900"),
           date_time < ymd_hm("2018-07-10T1300"),
           pCO2_RT > 0) %>%
  ggplot()+
  #geom_line(aes(date_time, pCO2_RT, col="RT_corr"))+
  geom_point(aes(date_time, pCO2_RT, col="RT_corr"), size=0.5)+
  geom_line(aes(date_time, pCO2_corr, col="raw"))+
  scale_color_brewer(palette = "Set1")+
  theme_bw()

ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics", "HydroC_RTcorr_Bittig_2018-07-10.jpg"),
       width = 13, height = 3)


write_csv(HC, here::here("Data/_summarized_data_files",
                     "Tina_V_Sensor_HydroC_RTcorr.csv"))

