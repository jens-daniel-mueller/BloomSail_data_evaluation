library(tidyverse)
library(lubridate)
library(tibbletime)

# Read Contros corrected data file ----------------------------------------

df <- read_csv("Data/HydroC_RTC/SD_datafile_20190523_140437CO2-0618-001_strip_HB.csv",
               skip = 4,
               col_names = c("date", "time", "Zero", "Flush",
                             "Signal_raw", "pCO2", "tlc_pCO2_corr_Henry"),
               col_types = cols(date = col_character(),
                                time = col_character()))


df <- df %>% 
  mutate(Flush = as.factor(as.character(Flush)),
         Zero = as.factor(as.character(Zero)),
         date_time =dmy_hms(str_c(date," T ", time)),
         tau = 62) %>% 
  arrange(date_time) %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))))



RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}


# Apply RT correction to entire data set ----------------------------------

# Define functions and window width for rolling mean
window <- 15
rolling_mean   <- rollify(~mean(.x, na.rm = TRUE), window = window)
rolling_median <- rollify(~median(.x, na.rm = TRUE), window = window)


df <- df %>%
  mutate(pCO2_RT = RT_corr(pCO2, lag(pCO2), dt, tau),
         pCO2_RT = if_else(pCO2_RT %in% c(Inf, -Inf), NaN, pCO2_RT),
         pCO2_RT_mean = rolling_mean(pCO2_RT),
         pCO2_RT_median = rolling_median(pCO2_RT))


# time shift RT corrected data
shift <- as.integer(as.character(window/2))
df <- df %>%
  mutate(pCO2_RT_mean = lead(pCO2_RT_mean, shift),
         pCO2_RT_median = lead(pCO2_RT_median, shift))

rm(rolling_median, rolling_mean, shift, tau_high, window, RT_corr)

df_long <- df %>% 
  pivot_longer(cols = c(pCO2, tlc_pCO2_corr_Henry, pCO2_RT, pCO2_RT_mean, pCO2_RT_median),
               names_to = "estimate", values_to = "pCO2")


df_long %>% 
  ggplot(aes(date_time, pCO2, col=estimate))+
  #geom_line()+
  geom_point(size = 0.4)+
  coord_cartesian(ylim = c(0,4500))+
  theme_bw()+
  scale_color_brewer(palette = "Set1")

ggsave(here::here("/Plots/RT_Test", "HydroC_Bittig_reprocessed.jpg"),
       width = 15, height = 5, dpi = 300)


df %>% 
  write_csv(here::here("Data/HydroC_RTC", "SD_datafile_20190523_140437CO2-0618-001_strip_JM.csv"))
