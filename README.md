# BloomSail data evaluation

Evaluation of marine biogeochemical data recorded in the Central Baltic Sea (east coast of Gotland) during the BloomSail expedtion in Summer 2018 in order to better understand organic matter production and control of cyanobacteria blooms.

Code is written in R and relies to a large degree on the tidyverse packages and programming principles.

Data are stored in a folder "Data", which is not synchronized with this repo before publication of results is finalized. In this folder, data are organized in subfolders containing observations from different platforms (Tina_V, VOS_Finnmaid, ARGO, Remote_sensing, ...) and sampling techniques (Sensor, discrete sampling, ...).

1) R scripts named 1_XXX.R read-in data files from individual platforms/sampling techniques, perform some basic quality checks, and safe summarized date sets into the folder "Data/_summarized_data_files".

-Column names:
  -date, time, date_time
  -ID: unique identifier of each transect referring to the starting date
  -type: Operation mode of the sensor package (T: towed transect; P: Profiling at station)
  -station: Sampling location, usually on out of P01-P13, OGB, or few others
  -dep: Water depth (m)
  -sal: Salinity
  -extension "_meas" refers to measurement conditions (such as T=25C for spec pH measurements), as in contrast of in-situ conditions

-GPS data from one transect in June still missing

2) Calculations and data manipulation

3) Merging summarized data sets and writing into folder "Data/_merged_data_files".

4) Plot results

# Status by parameter

* pCO2
  + postprocessed (drift corrected) data set provided by KM Contros
  + deployment periods identified
  + test and handling deployments removed
  + Flush periods identified as post-zeroing phase
  + Response time determined for each Flush period





