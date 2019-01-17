library(tidyverse)
library(lubridate)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Ostergarnsholm/Tower")
M6 <- read_csv("Oestergarnsholm_Atmospheric_data_2018_6.csv")
M7 <- read_csv("Oestergarnsholm_Atmospheric_data_2018_7.csv")
M8 <- read_csv("Oestergarnsholm_Atmospheric_data_2018_8.csv")

df <- bind_rows(M6, M7, M8)
rm(M6, M7, M8)

df <-
df %>% 
  mutate(date = ymd_hms( paste(paste(year, month, day, sep = "/"), paste(hour, min, sec, sep = ":")))) %>% 
  select("date",
         "CO2 12m [ppm]",
         "w_c [ppm m/s]",
         "WS 12m [m/s]",
         "WD 12m [degrees]",
         "T 12m [degrees C]",
         "Specific humidity [g/kg]",
         "RIS [W/m^2]"
         )

df.long <-
  df %>% 
  gather("parameter", "value", 2:8)



df %>% 
  ggplot(aes(date, `T 12m [degrees C]`))+
  geom_line()+
  theme_bw()

df %>% 
  ggplot(aes(date, `CO2 12m [ppm]`))+
  geom_line()+
  theme_bw()

df %>% 
  filter(!is.na(`w_c [ppm m/s]`)) %>% 
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_point(aes(date, `w_c [ppm m/s]`, col=`w_c [ppm m/s]` > 0))+
  theme_bw()


df %>% 
  filter(!is.na(`w_c [ppm m/s]`)) %>%
  mutate(yday = yday(date)) %>% 
  group_by(yday) %>% 
    summarise(date=mean(date),
              `daily mean w_c [ppm m/s]` = mean(`w_c [ppm m/s]`)) %>% 
  ggplot()+
  geom_hline(yintercept = 0)+
  geom_point(aes(date, `daily mean w_c [ppm m/s]`, col=`daily mean w_c [ppm m/s]` > 0))+
  theme_bw()


df.long.daily.mean <-
  df.long %>%
  filter(!is.na(value)) %>%
  mutate(yday = yday(date)) %>% 
  group_by(yday, parameter) %>% 
  summarise(date = mean(date),
            daily_mean_value = mean(value)) %>% 
  ungroup()


#ggpubr::ggarrange(Tem, , ncol=1, nrow=2, common.legend = TRUE, legend="right", align = "v", heights = c(1.5,1))

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")

df.long %>% 
  ggplot(aes(date, value))+
  geom_line(aes(col="raw reading"))+
  geom_line(data=df.long.daily.mean, aes(date, daily_mean_value, col="daily mean"))+
  facet_grid(parameter~., scales = "free_y")+
  scale_color_brewer(palette = "Set1", name="")+
  theme_bw()

ggsave("Ostergarnsholm_tower_Summer2018.pdf", height = 400, width = 250, units = "mm")






