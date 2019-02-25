# BloomSail_data_evaluation

Evaluation of marine biogeochemical data recorded in the Central Baltic Sea (east coast of Gotland) during the BloomSail expedtion in Summer 2018 in order to better understand organic matter production and control of cyanobacteria blooms.

Code is written in R and relies to a large degree on the tidyverse packages and programming principles.

Data are stored in a folder "Data", which is not snychronized with this repo before publication of results is finalized. In this folder, data are organized in subfolders containing observations from different platforms (Tina_V, VOS_Finnmaid, ARGO, Remote_sensing, ...) and sampling techniques (Sensor, discrete sampling, ...).

R script named 1_XXX.R read-in data files from individual platforms/sampling techniques, perform same basic quality checks, and safe summarized date sets into the folder "Data/Summarized_data_files".

