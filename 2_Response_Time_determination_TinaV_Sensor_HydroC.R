# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)
library(lubridate)


# inspired by http://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/

# Read and prepare data ---------------------------------------------------

df <- read_csv(here::here("Data/_summarized_data_files",
                          "Tina_V_Sensor_HydroC_Flush.csv"))

df <- df %>%
  filter(FlushZeroID != 53)

df <- df %>% 
  group_by(FlushZeroID, mixing) %>% 
  mutate(duration_equi = duration- min(duration))


# Plot individual Flush periods with exponential fit ----------------------

# for (i in unique(df$FlushZeroID)) {
# 
# df_ID <- df %>%
#   filter(FlushZeroID == i)
# 
# fit <- df_ID %>%
#   filter(mixing == "equilibration") %>%
#   nls(pCO2_corr ~ SSasymp(duration_equi, yf, y0, log_alpha), data = .)
# 
# tau <- as.numeric(exp(-tidy(fit)[3,2]))
# #RSS <- sum(resid(fit)^2)
# 
# augment(fit) %>% 
#   ggplot(aes(duration_equi, pCO2_corr))+
#   geom_point()+
#   geom_line(aes(y = .fitted))+
#   geom_vline(xintercept = tau)
# 
# ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Response_time_fits",
#                   paste(i,"_FlushZeroID_HydroC_RT.jpg", sep="")),
#          width = 10, height = 4)
# }


# Response time fitting ---------------------------------------------------

RT <- df %>% 
  filter(mixing == "equilibration") %>% 
  group_by(FlushZeroID) %>% 
  do(fit = nls(pCO2_corr ~ SSasymp(duration_equi, yf, y0, log_alpha), data = .)) %>% 
  tidy(fit) %>% 
  select(FlushZeroID, term, estimate) %>% 
  spread(term, estimate) %>% 
  select(1,2) %>% 
  mutate(tau = exp(-log_alpha))


# Residuals from fit ------------------------------------------------------

augmented <- df %>% 
  filter(mixing == "equilibration") %>% 
  group_by(FlushZeroID) %>% 
  do(fit = nls(pCO2_corr ~ SSasymp(duration_equi, yf, y0, log_alpha), data = .)) %>% 
  augment(fit)

# qplot(duration_equi, pCO2_corr, data = augmented, geom = 'point', colour = as.factor(FlushZeroID)) +
#   geom_line(aes(y=.fitted))
# 
# qplot(duration_equi, .resid, data = augmented, geom = 'point', colour = as.factor(FlushZeroID))

augmented_sum <- augmented %>% 
  group_by(FlushZeroID) %>% 
  summarise(mean_resid = mean(abs(.resid)),
            mean_resid_rel = mean(abs(.resid))/max(pCO2_corr),
            max_pCO2_corr = max(pCO2_corr))



# Standard error of tau ---------------------------------------------------

St_Err <- df %>% 
  filter(mixing == "equilibration") %>% 
  group_by(FlushZeroID) %>% 
  do(fit = nls(pCO2_corr ~ SSasymp(duration_equi, yf, y0, log_alpha), data = .)) %>% 
  tidy(fit) %>% 
  select(FlushZeroID, term, std.error) %>% 
  spread(term, std.error) %>% 
  select(1,2) %>% 
  rename(tau_st_err = log_alpha)


# Merge RT, mean residuals and St error -----------------------------------

RT <- full_join(RT, augmented_sum)
RT <- full_join(RT, St_Err)

rm(augmented, augmented_sum, St_Err)



# Identify residual threshold ---------------------------------------------

RT %>% 
  ggplot(aes(mean_resid_rel, FlushZeroID, label=FlushZeroID)) +
  geom_point(shape=21)+
  scale_fill_viridis_c()+
  geom_label()

RT %>% 
  filter(mean_resid_rel >= 0.0065)

RT %>% 
  filter(mean_resid_rel < 0.0065) %>% 
  ggplot(aes(FlushZeroID, tau, label=round(mean_resid_rel,4))) +
  geom_label(data=RT, aes(FlushZeroID, tau, label=round(mean_resid_rel,4)), shape=21, col="red") +
  geom_point(shape=21)+
  scale_fill_viridis_c()+
  geom_label()



# Mean tau ----------------------------------------------------------------

max(unique(df[df$date_time < ymd_hms("2018-07-17;13:08:34"),]$FlushZeroID))
unique(df[df$date_time > ymd_hms("2018-07-17;13:08:34"),]$FlushZeroID)


RT %>% 
  filter(mean_resid_rel < 0.0065) %>% 
  mutate(pump_power = if_else(FlushZeroID <= 20, "1W", "8W")) %>% 
  group_by(pump_power) %>% 
  summarise(tau = mean(tau))







