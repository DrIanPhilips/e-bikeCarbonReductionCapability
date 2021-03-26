#master script to call a model run 

#this series calls scripts which compare car lifecycle versus e-bike lifecycle emissions


#------------ setup 01 ------------------------------------------
getwd()


library(data.table)
library(tmap)
library(tidyverse)

#Set a datetime stamp
datetime <- gsub(" ","_",Sys.time()) 
datetime <- gsub(":","-",datetime) 
datetime

sensiblename <-  "car_lifecycle_ebike_lifecycle"  

datetime <- paste0(sensiblename,datetime)

# datetime <- car_lifecycle_ebike_lifecycle_no_langmuir2021-03-21_12-54-59
#------------------- 02_read_base_population

popfile <- fread("MDOfiles/popfile.csv")



#---------------- 03_Maximum distance by e-bike walk or cycle ---


source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/03_MDOMax_dist_e-bike_bike_walk.R")

#---------------- 04 make multiple draws - and 05 calculate car km travelled and savings ----------
#Script 04 calls script 05 
source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/04_ call_multiple_draws150321carlifecycle.R")



#---------------06 summarise multiple draws 

source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/06_indicators_from_multiple_draws.R")


#---------------07 map results  ------------ 

source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/07_map_results.R")


#--------------- 08 map results in context of vulnerability  -------------- 

source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/08_results_in_Context_vulnerability_ruralUrb150321.R")