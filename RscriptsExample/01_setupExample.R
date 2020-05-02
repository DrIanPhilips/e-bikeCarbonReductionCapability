#setup


#------------ setup ------------------------------------------
getwd()

library(data.table)
library(tmap)
library(tidyverse)


#This will create outputs with a common datetime 
#making it easy gather all the model outputs from this 
#run
datetime <- gsub(" ","_",Sys.time()) 
datetime <- gsub(":","-",datetime) 
datetime

