library(seacarb)
library(ggplot2)
library(viridis)
library(data.table)
library(tidyr)
library(tidyverse)
library(MASS)
library(boot)

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/GE_calibration")


#### defining mCP characterization according to Mueller and Rehder (2018)

pHT.Mueller <- function(Sal, Tem, Rspec){
  
  #first set of coefficients defines pK2e2 = f(Sal, Tem)
  
  1.08071477e+03                      -
    1.35394946e-01  *Sal^0.5            -   
    1.98063716e+02  *Sal^1.5            +
    6.31924397e+01  *Sal^2              -
    5.18141866e+00  *Sal^2.5            -
    2.66457425e+04  *Tem^-1             +
    5.08796578e+03  *Sal^1.5 * Tem^-1   -
    1.62454827e+03  *Sal^2 * Tem^-1     +
    1.33276788e+02  *Sal^2.5 * Tem^-1   -
    1.89671212e+02  *log(Tem)           +
    3.49038762e+01  *Sal^1.5 * log(Tem) -
    1.11336508e+01  *Sal^2 * log(Tem)   +
    9.12761930e-01  *Sal^2.5 * log(Tem) +
    3.27430677e-01  *Tem              -
    7.51448528e-04  *Sal^0.5 * Tem      +
    3.94838229e-04  *Sal * Tem          -
    6.00237876e-02  *Sal^1.5 * Tem      +
    1.90997693e-02  *Sal^2 * Tem        -
    1.56396488e-03  *Sal^2.5 * Tem      +
    
    #second set of coefficients includes the definition of mCP absorptivity ratios e1 and e3/e3
    #as determined by Liu et al. (2011) and defines the log-term calculation 
    
    
    log10(
      (Rspec -
         (-0.007762 + 4.5174e-5*Tem)) /
        (1 - (Rspec *  (- 0.020813 + 2.60262e-4*Tem + 1.0436e-4*(Sal-35))))
    )
}

pHT.Mueller (20, 298.15, 1)

##### load absorbance data file(s)

df <- read.delim("180518_glass_electrode_SBE_27-0319_calibration_ArchivedSpectra", header=FALSE)


df <- df[,c(1,seq(3,12,1),15,seq(18,719,1))]
names(df)<-c("Tem.level","date","time","Rep","X1","X2","pK.Carter","A5","A4","A7","Ai","Sal","pH.Carter", seq(200,900,1))
df <- data.table(df)

df$date <-
  as.POSIXct(strptime(paste(df$date, df$time), format = "%m/%d/%Y %H:%M"))

df$time <- NULL

####plot spectra
# 
# df_long <- gather(df, lam, abs, 13:713)
# df_long<-data.table(df_long)
# df_long$lam <- as.numeric(df_long$lam)
# 
# 
# fig <-
#   ggplot()+
#   geom_vline(xintercept = c(434, 578))+
#   geom_hline(yintercept = c(0.4, 1))+
#   geom_line(data=df_long,
#             aes(lam, abs, col=as.factor(Tem.level), linetype=as.factor(Rep)))+
#   facet_grid(~Sal)+
#   xlim(380, 620)+
#   scale_colour_brewer(palette="Spectral", name="T (?C)", direction = -1)+
#   labs(x="wavelength [nm]", y="absorption")
# 
# tiff("./mCP_spectra.tiff", width = 200, height = 150, units = 'mm', res = 600, compression = 'lzw')
# fig
# dev.off()
# rm(fig)
# 

#### load calibration data GE

GE <- read.csv("180518_calibration_GE_0319.csv")


df$A5c <- df$A5-df$A7
df$A4c <- df$A4-df$A7
df$R<-df$A5/df$A4
df <- df[,c("Tem.level", "date", "Rep", "Sal", "R")]


df <- merge(df, GE)

rm(GE)



#### recalculate R and pH values

df <- df %>%
  mutate(pHT = pHT.Mueller(Sal, Tem.spec+273.15, R)) %>% 
  filter(Tem.level != "25C" | Sal != 10.9)

df %>% 
ggplot(aes(pHT, V0, col=Tem.level))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  facet_wrap(~Sal)

df %>% 
ggplot(aes(pHT, V0, col=as.factor(Sal)))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  facet_wrap(~Tem.level)


fit <- lm(data=df, pHT ~
            I(V0)+
            I(V0^2)+
            I(V0*Tem.GE)+
            I(V0^2*Tem.GE)+
            I(V0*Sal)+
            I(V0^2*Sal)+
            I(V0*Sal*Tem.GE)+
            I(V0^2*Sal*Tem.GE))

fit

step <- stepAIC(fit, direction="both")
step$anova # display results 



# cv.error10 <- cv.glm(df, fit, K=10)$delta[1]
# plot(cv.error10)
# which.min(cv.error10)


# add residuals to fitted data
df$residual <- fit$residuals



prd <- data.table(expand.grid(
  Sal = seq(5,15,5),
  Tem.GE = seq(5, 25, 10),
  V0 = seq(2.5,3.2,0.2)))
prd$fit <- predict(fit, newdata = prd, se.fit = FALSE)

prd %>% 
ggplot(aes(fit, V0, col=as.factor(Tem.GE)))+
  geom_line()+
  facet_wrap(~Sal)+
  scale_color_viridis_d(option = "A")


prd %>% 
ggplot(aes(fit, V0, col=as.factor(Sal)))+
  geom_line()+
  facet_wrap(~Tem.GE)+
  scale_color_viridis_d(option = "A")



df %>% 
ggplot(aes(pHT, residual, fill=Tem.GE))+
  geom_hline(yintercept = 0)+
  geom_point(shape=21)+
  scale_fill_viridis_c(option = "B")+
  facet_wrap(~Sal)

