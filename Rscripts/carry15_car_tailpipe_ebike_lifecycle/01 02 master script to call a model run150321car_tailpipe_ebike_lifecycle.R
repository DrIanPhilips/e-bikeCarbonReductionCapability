#master script to call a model run carry15_car_tailpipe_ebike_lifecycle


#------------ setup 01 ------------------------------------------
getwd()


library(data.table)
library(tmap)
library(tidyverse)

#Set a datetime stamp
datetime <- gsub(" ","_",Sys.time()) 
datetime <- gsub(":","-",datetime) 
datetime


#corrected the allocation of car km so that only England rather than whol UK car km are allocates
sensiblename <-  "car_tailpipe_ebike_lifecycle"  

datetime <- paste0(sensiblename,datetime)

# datetime <- 
#------------------- 02_read_base_population

popfile <- fread("MDOfiles/popfile.csv")



#---------------- 03_Maximum distance by e-bike walk or cycle ---


source("Rscripts/03_MDOMax_dist_e-bike_bike_walk.R")

#---------------- 04 make multiple draws - and 05 calculate car km travelled and savings ----------
#Script 04 calls script 05 
source("Rscripts/carry15_car_tailpipe_ebike_lifecycle/04_ call_multiple_draws150321car_tailpipe_ebike_lifecycle.R")



#---------------06 summarise multiple draws 

source("Rscripts/06_indicators_from_multiple_draws.R")


#---------------07 map results  ------------ 

source("Rscripts/carry15_car_tailpipe_ebike_lifecycle/07_map_resultscar_tailpipe_ebike_lifecycle.R")


#--------------- 08 map results in context of vulnerability  -------------- 

source("Rscripts/08_results_in_Context_vulnerability_ruralUrb150321.R")
