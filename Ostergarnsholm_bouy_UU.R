library(ggplot2)
library(tidyverse)
library(data.table)
library(lubridate)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Merged_data_files")
Sensor <- data.table(read.csv("BloomSail_Sensor_Track_data.csv"))

Sensor$date <- ymd_hms(Sensor$date)
Sensor$start.date <- ymd_hms(Sensor$start.date)




