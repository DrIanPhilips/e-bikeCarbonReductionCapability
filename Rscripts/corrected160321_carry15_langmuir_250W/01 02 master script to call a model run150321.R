#master script to call a model run 


#This series incorporates the error corrections of trip distribution and double counting vkt at the replacement stage 

#It also runs using the overly conservative langmuir constraints on travel distance
#and it caps total power output at 250W for e-bikes 
# rather than output being upto 250w motor assist + rider power


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
sensiblename <-  "Langmuir_250Wcap"  

datetime <- paste0(sensiblename,datetime)

# datetime <- "Langmuir_250Wcap2021-03-18_11-10-35" # 
#------------------- 02_read_base_population

popfile <- fread("MDOfiles/popfile.csv")



#---------------- 03_Maximum distance by e-bike walk or cycle ---

#NB in this series this script and it's calling of functions in speedDistanceFuncBikeEbike160120df3.R
#is where the langmuir constraints and 250 watt total power output are accounted for
source("Rscripts/corrected160321_carry15_langmuir_250W/03_MDOITS121_150120_CARRY15aero8ms20.R")

#> summary(MDO1$ebikemaxroaddist)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   10.80   16.11   14.28   18.84   25.53 



#---------------- 04 make multiple draws - and 05 calculate car km travelled and savings ----------
#Script 04 calls script 05 
source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/04_ call_multiple_draws150321.R")



#---------------06 summarise multiple draws 

source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/06_indicators_from_multiple_draws.R")


#---------------07 map results  ------------ 
#note some map rescaling may be needed
source("Rscripts/carry15_car_lifecycle_ebike_lifecycle/07_map_results.R")


#--------------- 08 map results in context of vulnerability  -------------- 
#note some map rescaling may be needed
#source("Rscripts/corrected160321_carry15_langmuir_250W/08_results_in_Context_vulnerability_ruralUrb150321.R")