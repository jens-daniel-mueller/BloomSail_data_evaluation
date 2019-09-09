# Packages ----------------------------------------------------------------

library(tidyverse)
library(broom)


# Determine RT by exponential fit -----------------------------------------
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
# 
# qplot(duration_equi, pCO2_corr, data = augment(fit)) +
#   geom_line(aes(y = .fitted))+
#   geom_vline(xintercept = tau)
# 
# ggsave(here::here("/Plots/TinaV/Sensor/HydroC_diagnostics/Response_time_fits",
#                   paste(i,"_FlushZeroID_HydroC_RT.jpg", sep="")),
#          width = 10, height = 4)
# }



# Determine response times ------------------------------------------------

RT <- df %>% 
  filter(mixing == "equilibration") %>% 
  group_by(FlushZeroID) %>% 
  do(fit = nls(pCO2_corr ~ SSasymp(duration_equi, yf, y0, log_alpha), data = .)) %>% 
  tidy(fit) %>% 
  select(FlushZeroID, term, estimate) %>% 
  spread(term, estimate) %>% 
  select(1,2) %>% 
  mutate(tau = exp(-log_alpha))



# Determine mean absolute residual from fit -------------------------------

augmented <- df %>% 
  filter(mixing == "equilibration") %>% 
  group_by(FlushZeroID) %>% 
  do(fit = nls(pCO2_corr ~ SSasymp(duration_equi, yf, y0, log_alpha), data = .)) %>% 
  augment(fit)


qplot(duration_equi, pCO2_corr, data = augmented, geom = 'point', colour = as.factor(FlushZeroID)) +
  geom_line(aes(y=.fitted))

qplot(duration_equi, .resid, data = augmented, geom = 'point', colour = as.factor(FlushZeroID))

augmented_sum <- augmented %>% 
  group_by(FlushZeroID) %>% 
  summarise(mean_resid = mean(abs(.resid)))


# Determine standard error of tau -----------------------------------------

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



# Plot RTs over fielt trip period -----------------------------------------

RT %>% 
  filter(mean_resid < 0.4) %>% 
  ggplot(aes(FlushZeroID, tau, col=mean_resid)) +
  geom_point()+
  scale_color_viridis_c()

RT %>% 
  filter(tau_st_err < 0.025) %>% 
  ggplot(aes(FlushZeroID, tau, col=tau_st_err)) +
  geom_point()+
  scale_color_viridis_c()

RT %>% 
  #filter(tau < 100) %>% 
  ggplot(aes(tau, tau_st_err)) +
  geom_point()

RT %>% 
  #filter(tau < 100) %>% 
  ggplot(aes(mean_resid, tau_st_err)) +
  geom_point()






