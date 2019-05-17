# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)


# Read Finnmaid data: time series in subareas -----------------------------

df <- read_csv(here::here("Data/Finnmaid", "Finnmaid_mean_area_CT_2019.csv"),
               col_types = cols(cO2_mean = col_double()))


# Subset relevant summer month --------------------------------------------


df <- df %>% 
  select(Area, date = date_mean, Sal=Sal_mean, Tem=Tem_mean, pCO2=pCO2_mean, 
         CT = CT_mean, cO2 = cO2_mean) %>% 
  mutate(mon = month(date),
         yr = year(date)) %>% 
  filter(mon %in% seq(6,8,1)) %>% 
  mutate(day_date = as.POSIXct(strptime(paste(2000,yday(date)), format = "%Y %j",tz="GMT"))) %>% 
  select(Area, yr, day_date, Tem, CT, cO2) %>% 
  gather(key = "parameter", value = "value", 4:6)


#i_Area <- unique(df$Area)[4]
for (i_Area in unique(df$Area)){

df %>%
  filter(Area == i_Area,
         !(yr %in% c(2005, 2006))) %>% 
  ggplot(aes(day_date, value))+
  geom_path()+
  geom_point(size=0.5)+
  facet_grid(parameter~yr, scales = "free_y")+
  theme_bw()
  
ggsave(here::here("Plots/Finnmaid/all_years",
                  paste(i_Area,"_CT_Tem_timeseries_all_years.jpg", sep = "")),
                  width = 40, height = 6, dpi=300)

}



df %>%
  filter(Area %in% unique(df$Area)[4:6],
         !(yr %in% c(2005, 2006))) %>% 
  ggplot(aes(day_date, value, col=Area))+
  geom_path()+
  geom_point(size=0.5)+
  facet_grid(parameter~yr, scales = "free_y")+
  theme_bw()
  
ggsave(here::here("Plots/Finnmaid/all_years",
                  "Northern_Areas_CT_Tem_timeseries_all_years.jpg"),
                  width = 42, height = 4, dpi=300)

