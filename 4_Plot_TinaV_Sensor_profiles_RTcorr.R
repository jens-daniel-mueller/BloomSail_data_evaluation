# Packages ----------------------------------------------------------------

library(tidyverse)
#library(lubridate)

# Read merged data file ---------------------------------------------------

Sensor <- read_csv(here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv"),
                   col_types = cols(
                     .default = col_double(),
                     date_time = col_datetime(format = ""),
                     ID = col_character(),
                     type = col_character(),
                     station = col_character(),
                     cast = col_character()))


# Subset profile data -----------------------------------------------------

Sensor <- Sensor %>% 
  filter(type == "P",
         !(station %in% c("PX1", "PX2")),
         Zero != 1,
         Flush != 1)


# Remove disturbed profiles -----------------------------------------------

Sensor <- Sensor %>% 
  filter(ID != "180709" | station != "P08" | cast != "up") %>% 
  filter(ID != "180716" | station != "P01" | cast != "up") %>% 
  filter(ID != "180718" | station != "P07" | cast != "up") %>% 
  filter(ID != "180723" | station != "P09" | cast != "down") %>% 
  filter(ID != "180815" | station != "P09" | cast != "down") %>% 
  filter(ID != "180815" | station != "P11" | cast != "up")



# Plot vertical profiles individually -------------------------------------


# # i_ID <- unique(Sensor$ID)[1]
# # i_station <- unique(Sensor$station)[3]
# 
# for(i_ID in unique(Sensor$ID)){
#   for(i_station in unique(Sensor$station)){
# 
# if (nrow(Sensor %>% filter(ID == i_ID, station == i_station)) > 0){
# 
# Sensor %>%
#   filter(ID == i_ID,
#          station == i_station) %>%
#   ggplot(aes(tem, dep, col=cast))+
#   geom_path()+
#   scale_y_reverse()+
#   scale_color_brewer(palette = "Set1")+
#   labs(y="Depth [m]", title = str_c("Date: ",i_ID," | Station: ",i_station))+
#   coord_cartesian(xlim = c(0,25))+
#   theme_bw()
# 
# print(str_c(i_ID,"_",i_station,"_.jpg"))
# ggsave(here::here("/Plots/TinaV/Sensor/all_profiles_RT", str_c(i_ID,"_",i_station,"_Tem.jpg")),
#        width = 5, height = 5)
# }
# 
#     }
# }

# i_ID <- unique(Sensor$ID)[1]
# i_station <- unique(Sensor$station)[3]

for(i_ID in unique(Sensor$ID)){
  for(i_station in unique(Sensor$station)){

if (nrow(Sensor %>% filter(ID == i_ID, station == i_station)) > 0){

Sensor %>%
  filter(ID == i_ID,
         station == i_station) %>%
  ggplot()+
  geom_path(aes(pCO2, dep, linetype = cast, col = "raw"))+
  geom_path(aes(pCO2_analog, dep, linetype = cast, col = "analog"))+
  geom_path(aes(pCO2_RT_median, dep, linetype = cast, col = "RT_median"))+
  geom_path(aes(pCO2_RT_mean, dep, linetype = cast, col = "RT_mean"))+
  scale_y_reverse()+
  scale_color_manual(values = c("lightgrey", "black", "green", "red"))+
  labs(y="Depth [m]", title = str_c("Date: ",i_ID," | Station: ",i_station))+
  theme_bw()

print(str_c(i_ID,"_",i_station,"_.jpg"))
ggsave(here::here("/Plots/TinaV/Sensor/all_profiles_RT", str_c(i_ID,"_",i_station,"_pCO2_RT120.jpg")),
       width = 5, height = 5)

}

    }
}



# i_ID <- unique(Sensor$ID)[6]
# i_station <- unique(Sensor$station)[10]

for(i_ID in unique(Sensor$ID)){
  for(i_station in unique(Sensor$station)){

if (nrow(Sensor %>% filter(ID == i_ID, station == i_station)) > 0){

p_pCO2 <-
Sensor %>%
  filter(ID == i_ID,
         station == i_station) %>%
  ggplot()+
  geom_path(aes(date_time, pCO2, col=cast, linetype = "raw"))+
  geom_path(aes(date_time, pCO2_RT_median, col=cast, linetype = "RT_median"))+
  scale_color_brewer(palette = "Set1")+
  labs(y="pCO2", title = str_c("Date: ",i_ID," | Station: ",i_station))+
  theme_bw()


p_dep <-
Sensor %>%
  filter(ID == i_ID,
         station == i_station) %>%
  ggplot()+
  geom_path(aes(date_time, dep, col=cast, linetype = "raw"))+
  scale_color_brewer(palette = "Set1")+
  labs(y="Depth [m]")+
  theme_bw()


g <- gridExtra::arrangeGrob(p_pCO2, p_dep)

print(str_c(i_ID,"_",i_station,"_.jpg"))
ggsave(here::here("/Plots/TinaV/Sensor/all_profiles_RT", str_c(i_ID,"_",i_station,"_dep_RT120.jpg")), g,
       width = 5, height = 7)

}

    }
  }





# Offset corrected profiles diagnosis -------------------------------------

RT_diff <- Sensor %>% 
  mutate(dep_int = as.numeric(as.character( cut(dep, seq(0,30,2),seq(1,29,2))))) %>% 
  group_by(ID, station, dep_int, cast) %>%
  summarise_all("mean", na.rm = TRUE) %>% 
  ungroup() %>% 
  select(ID, station, dep_int, cast, pCO2, pCO2_RT, pCO2_RT_mean, pCO2_RT_median) %>% 
  pivot_longer(cols = c(pCO2, pCO2_RT, pCO2_RT_mean, pCO2_RT_median), names_to = "correction") %>% 
  pivot_wider(names_from = cast, values_from = value) %>% 
  mutate(d_pCO2 = down - up,
         mean_pCO2 = (down + up)/2)

RT_diff %>% 
  filter(!is.na(dep_int)) %>% 
  ggplot()+
  #geom_point(aes(ymd(ID), d_pCO2))+
  geom_violin(aes(parse_date(ID, format = "%y%m%d"), d_pCO2, group=ID))+
  scale_color_viridis_d()+
  facet_grid(dep_int~correction, scales = "free_y")+
  geom_hline(yintercept = 0)

RT_diff %>% 
  filter(!is.na(dep_int)) %>% 
  ggplot()+
  geom_violin(aes(dep_int, d_pCO2, group=dep_int))+
  scale_color_viridis_d()+
  facet_wrap(~correction)+
  geom_hline(yintercept = 0)+
  scale_x_reverse()+
  coord_flip()

RT_diff_sum <- Sensor %>% 
  mutate(dep_int = as.numeric(as.character( cut(dep, seq(0,30,2),seq(1,29,2))))) %>% 
  group_by(ID, station, dep_int, cast) %>%
  summarise_all("mean") %>% 
  ungroup() %>% 
  select(ID, station, dep_int, cast, pCO2, pCO2_RT, pCO2_RT_mean, pCO2_RT_median) %>% 
  pivot_longer(cols = c(pCO2, pCO2_RT, pCO2_RT_mean, pCO2_RT_median), names_to = "correction") %>% 
  pivot_wider(names_from = cast, values_from = value) %>% 
  mutate(d_pCO2 = down - up) %>% 
  group_by(dep_int, correction) %>% 
  summarise(mean_d_pCO2 = mean(d_pCO2, na.rm = TRUE)) %>% 
  ungroup()

RT_diff_sum %>% 
  ggplot()+
  geom_path(aes(-mean_d_pCO2, dep_int, col=correction))+
  scale_y_reverse()






