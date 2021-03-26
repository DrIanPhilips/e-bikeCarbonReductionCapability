#master script to call a model run 

#This will run the minimal example 

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


#corrected the allocation of car km so that only England rather than whol UK car km are allocates
sensiblename <-  "car_lifecycle_ebike_example"  

datetime <- paste0(sensiblename,datetime)


#------------------- 02_read_base_population

popfile <- fread("MDOfiles/DUMMYDATApopfile.csv")



#---------------- 03_Maximum distance by e-bike walk or cycle ---


source("Rscripts/03_MDOMax_dist_e-bike_bike_walk.R")

#---------------- 04 make multiple draws - and 05 calculate car km travelled and savings ----------
#Script 04 calls script 05 
source("Rscripts/04_ call_multiple_draws150321carlifecycle.R")



#---------------06 summarise multiple draws 

source("Rscripts/06_indicators_from_multiple_draws.R")


#---------------07 map results  ------------ 
#not run
#source("Rscripts/07_map_results.R")


#--------------- 08 map results in context of vulnerability  -------------- 
#notrun
#source("Rscripts/08_results_in_Context_vulnerability_ruralUrb.R")