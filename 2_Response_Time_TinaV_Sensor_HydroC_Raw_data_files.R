### load required packages ####

library(tidyverse)
library(here)
library(lubridate)

#### load data ####

HC <- read_csv(here::here("Data/_summarized_data_files", "Tina_V_Sensor_HydroC.csv"))

HC <- HC %>% 
  filter(date_time > ymd_hm("2018-07-10T0900"),
         date_time < ymd_hm("2018-07-10T1630"))

HC %>% 
ggplot(aes(date_time, pCO2_corr))+
  geom_line()


#### define response time parameters ####

tau <- 55

RT_corr <- function(c1, c0, dt, tau) {
  (1/(2*(1+2*(tau/(dt))))) * (c1 - (1-(2*(1+2*(tau/(dt))))) * c0)
}

RT_corr(0,1,2,3)

#### Apply RT correction ####

HC <- HC %>% 
  mutate(dt = as.numeric(as.character(date_time - lag(date_time))),
         pCO2_RT = RT_corr(pCO2_corr, lag(pCO2_corr), dt, tau))

HC %>% 
  filter(date_time > ymd_hm("2018-07-10T1500"),
         date_time < ymd_hm("2018-07-10T1600")) %>%
  ggplot()+
  geom_line(aes(date_time, pCO2_corr, col="raw"))+
  geom_line(aes(date_time, pCO2_RT, col="RT_corr"))


#### Subset and plot Zeroing data ####

df[, Zero_id := rleid(Zero)]

Zero.all <- df %>% 
  filter(Zero == 1)

Zero <- df %>% 
  filter(Zero == 1) %>% 
  group_by(Zero_id) %>%
  slice(50:n())


ggplot(Zero.all, aes(date.time, pCO2_corr))+
  geom_point(alpha=0.2)+
  ylim(0,40)

ggplot()+
  geom_point(data=Zero.all, aes(date.time, pCO2_corr, col="All"))+
  geom_point(data=Zero, aes(date.time, pCO2_corr, col="-50 first rows"))+
  ylim(0,40)+
  scale_color_brewer(palette = "Set1", name="Zeroing subset")

ggplot()+
  geom_point(data=Zero.all, aes(date.time, pCO2_corr, col="All"))+
  geom_point(data=Zero, aes(date.time, pCO2_corr, col="-50 first rows"))+
  ylim(0,40)+
  scale_color_brewer(palette = "Set1", name="Zeroing subset")+
  facet_wrap(~Zero_id, scales = "free_x")



#### Subset and plot Flush data


df[, Flush_id := rleid(Flush)]

Flush.all <- df %>% 
  filter(Flush == 1) %>% 
  mutate(date.time.sec = as.numeric(date.time))

Flush <- Flush.all %>%
  filter(Flush == 1 & !Flush_id %in% c(2,4,10,12,18,20,44,64,74,90,102,206)) %>%
  group_by(Flush_id) %>%
  slice(20:200) %>%
  mutate(sec = date.time.sec - first(date.time.sec)) %>% 
  ungroup()

# library(tidyr)
# 
# Flush.stats <- Flush %>%
#   group_by(Flush_id) %>%
#   do(fit = nls(pCO2_corr ~ SSasymp(Date, yf, y0, log_alpha), data = .)) %>% 
#   tidy(fit) %>% 
#   ungroup()


Flush.one <- Flush %>% 
  filter(Flush_id == 98) %>% 
  select(sec, pCO2_corr)

ggplot(Flush.one, aes(sec, pCO2_corr))+
  geom_point()

qplot(sec, pCO2_corr, data = Flush.one)

# nls(y ~ yf + (y0 - yf) * exp(-alpha * t), 
#     data = sensor1,
#     start = list(y0 = 54, yf = 25, alpha = 1))

fit <- nls(pCO2_corr ~ SSasymp(sec, yf, y0, log_alpha), data = Flush.one)
fit


qplot(sec, pCO2_corr, data = augment(fit)) + geom_line(aes(y = .fitted))






df %>% 
  group_by(sensor) %>% 
  do(fit = nls(y ~ SSasymp(t, yf, y0, log_alpha), data = .)) %>% 
  tidy(fit) %>% 
  select(sensor, term, estimate) %>% 
  spread(term, estimate) %>% 
  mutate(alpha = exp(log_alpha))




Fig <- 
  ggplot()+
  geom_point(data = Flush.all, aes(date, pCO2_corr, col="All"), alpha=0.2)+
  geom_point(data = Flush, aes(date, pCO2_corr, col="20-200"), alpha=0.2)+
  #geom_smooth(data = Flush, aes(date, pCO2_corr, col="20-200"), method = "lm", formula = y ~ log(x))+
  scale_color_brewer(palette = "Set1", name="Flush subset")+
  facet_wrap(~Flush_id, scales = "free")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_individual_Flush.tiff", width = 800, height = 800, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <- 
  ggplot()+
  geom_point(data = Flush, aes(sec, pCO2_corr, col=as.factor(Flush_id)), alpha=0.2)+
  scale_color_viridis_d(name="Flush ID")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_individual_Flush_overleigth.tiff", width = 300, height = 300, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

