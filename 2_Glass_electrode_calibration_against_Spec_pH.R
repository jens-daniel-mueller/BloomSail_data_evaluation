#### load required packages ####

library(tidyverse)
library(seacarb)
library(here)
library(moderndive)


#### Calibrate glass electrode from lab results #####

##### load absorbance data file

df <- read_tsv(here("Data/TinaV/Sensor/GE_calibration",
                    "180518_glass_electrode_SBE_27-0319_calibration_ArchivedSpectra"), col_names = FALSE)

df <- df[,c(1,seq(3,12,1),15,seq(18,719,1))]
names(df)<-c("Tem.level","date","time","Rep","X1","X2","pK.Carter","A5","A4","A7","Ai","Sal","pH.Carter", seq(200,900,1))

df <- df %>% 
  mutate(A5c = A5-A7,
         A4c = A4-A7,
         R = A5/A4) %>% 
  select("Tem.level", "date", "Rep", "Sal", "R")

#### load calibration data GE

GE <- read_csv(here("Data/TinaV/Sensor/GE_calibration","180518_calibration_GE_0319.csv"))

df <- inner_join(df, GE)
rm(GE)


#### recalculate R and pH values

df <- df %>% 
  mutate(pHT_spec = pHspec(Sal, Tem.spec, R),
         pHT_GE = pHinsi(pH=pHT_spec, ALK = 1722*1e-6, Tinsi = Tem.GE, Tlab = Tem.spec, 
                         S=Sal, k1k2 = "m10")) %>% 
  filter(Tem.level != "25C" | Sal != 10.9)

df %>% 
  ggplot(aes(Tem.GE,pHT_GE-pHT_spec, col=as.factor(Sal)))+
  geom_point()


df %>%
  ggplot()+
  #geom_point(aes(Spec.pH, V0, col="Spec_25_Carter"))+
  geom_point(aes(pHT_spec, V0, col="Spec_25"))+
  geom_point(aes(pHT_GE, V0, col="Spec_insitu"))+
  geom_smooth(aes(pHT_GE, V0, col="Spec_insitu"),method = "lm", se=FALSE)+
  facet_grid(Tem.level~Sal)

df %>%
  ggplot(aes(pHT_spec, V0, col=as.factor(Sal)))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  facet_wrap(~Tem.level)

df <- df %>% 
  filter(Sal == 7)

df %>%
  ggplot(aes(pHT_GE, V0, col=Tem.level))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  facet_wrap(~Sal)


#lm_V_pH <- lm(V0 ~ I(pHT_GE*(Tem.GE+273.15)*1.98416*1e-4) + I(7*(Tem.GE+273.15)*1.98416*1e-4), data = df)
lm_V_pH <- lm(V0 ~ I((Tem.GE+273.15)*(pHT_GE-7)), data = df)
lm_V_pH <- lm(pHT_GE ~ Tem.GE + V0, data = df)

get_regression_table(lm_V_pH)

intercept <- lm_V_pH$coefficients[[1]]
slope <- lm_V_pH$coefficients[[2]]
slope2 <- lm_V_pH$coefficients[[3]]

intercept_def <- 2.5749
slope_def <- 4.7681*1.98416*1e-4

df <- df %>% 
  mutate(#pHT_GE_calc = 7+ ((V0-intercept) / (slope*(Tem.GE+273.15))),
         pHT_GE_calc = intercept + slope*Tem.GE + slope2*V0,
         pHT_GE_def = 7+ ((V0-intercept_def) / (slope_def*(Tem.GE+273.15))))
  #mutate(pHT_GE_calc = (V0-Tem.GE*slope_Tem-intercept) / (slope_pH_Tem*Tem.GE))

df %>%
  ggplot()+
  geom_point(aes(pHT_GE, V0, col=Tem.level, shape="spec"))+
  geom_point(aes(pHT_GE_def, V0, col=Tem.level, shape="default"))+
  geom_point(aes(pHT_GE_calc, V0, col=Tem.level, shape="calc"))+
  geom_smooth(aes(pHT_GE, V0, col=Tem.level, linetype="spec"), method = "lm", se=FALSE)+
  facet_wrap(~Sal)

#### Read HydroFIA pH, Glaselektrode in-situ, and lab CO2 measurements ####

HF <- read_csv(here("Data/_summarized_data_files", "TinaV_bottle_pH_HydroFIA.csv"))
GE <- read_csv(here("Data/_summarized_data_files", "Tina_V_Sensor_Profiles_Transects.csv"))
CO2 <- read_csv(here("Data/_summarized_data_files", "TinaV_bottle_CO2_lab.csv"))

#### Subset and merge HydroFIA pH and Glaselektrode in-situ measurements ####

HF <- HF %>% 
  filter(station %in% c("P07", "P10"))

GE <- GE %>% 
  filter(station %in% c("P07", "P10")) %>% 
  select(date_time, ID, station, cast, dep, sal, tem, pH_GE = pH, V_GE = V_pH)

GE_HF <- GE %>% 
  full_join(HF, by=c("ID","station","dep"))

# GE %>% 
#   #filter(V_GE > 3.1) %>% 
#   ggplot(aes(V_GE, pH_GE, col=tem))+
#   scale_color_viridis_c()+
#   geom_point()


GE_HF <- GE_HF %>% 
  mutate(dep_int = cut(dep, seq(-2.5,42.5,5), labels = seq(0,40,5)))

GE_HF %>%
  ggplot()+
  geom_path(aes(pH_GE,dep,col=dep_int))+
  geom_point(aes(pHT,dep,col=dep_int))+
  scale_y_reverse()+
  facet_wrap(~interaction(station,ID))

GE_HF_dep <- GE_HF %>% 
  group_by(ID, station, dep_int) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  ungroup() %>% 
  filter(!is.na(pHT)) %>% 
  mutate(dep_int = as.numeric(as.character(dep_int)))



#### Compute mean surface AT and correct HydroFIA pH measurements to in-situ temperature ####

CO2 %>% 
  filter(dep <7) %>% 
  select(AT) %>% 
  summarise_all("mean", na.rm=TRUE)

GE_HF_dep <- GE_HF_dep %>% 
  mutate(pHT_insitu = pHinsi(pH=pHT, ALK = 1722*1e-6, Tinsi = tem, Tlab = tem_meas, 
                             S=sal, Pinsi = dep/10, k1k2 = "m10"))

GE_HF_dep %>% 
  ggplot(aes(tem,pHT-pHT_insitu, col=sal))+
  geom_point()
  
GE_HF_dep %>%
  ggplot()+
  geom_path(aes(pH_GE,dep,col="GE_original"))+
  geom_point(aes(pHT,dep,col="HF_25"))+
  geom_point(aes(pHT_insitu,dep,col="HF_insitu"))+
  scale_y_reverse()+
  facet_wrap(~interaction(station,ID))
  
#### Plot and Fit glasselektrode voltage reading to HydroFIA pH data ####

GE_HF_dep %>% 
  ggplot(aes(pHT, V_GE, col=tem))+
  scale_color_viridis_c()+
  geom_point()

GE_HF_dep %>% 
  ggplot(aes(pHT_insitu, V_GE, col=tem))+
  scale_color_viridis_c()+
  geom_point()


# lm_V_pH <- lm(V_GE ~ I((pHT_insitu-7)*(tem+273.15)), data = GE_HF_dep)
# get_regression_table(lm_V_pH)
# intercept <- lm_V_pH$coefficients[[1]]
# slope <- lm_V_pH$coefficients[[2]]

GE_HF_dep <- GE_HF_dep %>% 
  mutate(#pHT_GE = 7+ (V_GE-intercept) / (slope*(tem+273.15)),
         pHT_GE = intercept + slope*tem + slope2*V_GE)

GE_HF_dep %>% 
  ggplot()+
  #scale_color_viridis_c()+
  geom_point(aes(pHT_insitu, V_GE, col="HydroFIA_insitu"))+
  geom_point(aes(pHT, V_GE, col="HydroFIA_25"))+
  geom_point(aes(pHT_GE, V_GE, col="recalc"))+
  geom_point(aes(pH_GE, V_GE, col="original"))

GE_HF_dep <- GE_HF_dep %>% 
  mutate(dpH_25 = pHT_GE - pHT,
         dpH_insitu = pHT_GE - pHT_insitu)


#### Plot and Compare glasselektrode and HydroFIA pH data ####

GE_HF_dep %>% 
  ggplot()+
  geom_path(aes(pH_GE,dep_int, col="GE_original"))+
  geom_path(aes(pHT_GE,dep_int, col="GE_calib"))+
  geom_point(aes(pHT_GE,dep_int, col="GE_calib"))+
  geom_path(aes(pHT_insitu,dep_int, col="HF_insitu"))+
  geom_point(aes(pHT_insitu,dep_int, col="HF_insitu"))+
  scale_y_reverse()+
  facet_wrap(~interaction(station,ID))+
  scale_color_brewer(palette = "Set1")


GE_HF_dep %>% 
  ggplot(aes(dpH_insitu, dep_int, col=tem))+
  geom_vline(xintercept = 0)+
  geom_point()+
  scale_y_reverse()+
  scale_x_continuous(breaks = seq(-1,1,0.1))+
  scale_color_viridis_c()

GE_HF_dep %>% 
  ggplot(aes(date_time, dpH_insitu, col=as.factor(dep_int)))+
  geom_point()+
  geom_path()+
  facet_grid(dep_int~station)


#### Recompute full water column pH profiles from glasselectrode voltage readings ####

GE_HF <- GE_HF %>% 
  mutate(pHT_GE = ((V_GE-intercept) / (slope*(tem+273.15)))+7 )

GE_HF %>% 
  ggplot()+
  geom_path(aes(pHT_GE, dep, col="recalc"))+
  geom_path(aes(pH_GE, dep, col="original"))+
  #geom_path(aes(V_GE, dep, col="V"))+
  geom_point(data=GE_HF_dep, aes(pHT_insitu, dep))+
  scale_y_reverse()+
  facet_wrap(~interaction(station,ID))+
  xlim(7.5,9.5)


GE_HF %>% 
  filter(dep<20, cast=="down") %>% 
  ggplot(aes(pHT_GE, dep, col=as.factor(ID)))+
  geom_point()+
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~station)






