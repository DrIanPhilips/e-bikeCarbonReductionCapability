# This script carries out multiple draws using monte-carlo sampling 
# It calls 05_MultiDrawMDOcalcs130220.R which in turn calls several functions
# 



#-------- setup for multiple runs ------------ 
require(tidyverse)
require(data.table)


#--------clear memory and collect garbage ------------ 

#if you set datetime to  "2020-04-28_19-56-19", it will load example data

#the run used in for submitted paper
#datetime<- "2020-02-13_20-30-30"
#remove everything except for datetime object used 
#for naming output files and keeping them consistent 
#with other scripts in the modelrun. 

if(!exists("MDO1")){rm(list=setdiff(ls(), c("datetime")))}   
gc()

#------ create space to save results ----

dir.create(paste0("Results",datetime))

#------------ loop for multiple draws --------- 

#------ this script has sampling so set seed --- 


#----- Draw 1 --------
rm(list=setdiff(ls(), c("datetime")))
gc()
#start the clock to time the run
ptm <- proc.time()
seed_number <- 1240
set.seed(seed_number)
# This is effectively a 'poor mans function'.  
#I have some unfinished code to functionalise this but it is not running
#I think there may be scope issues.  
source("RscriptsExample/05_MultiDrawMDOcalcs130220.R")
# Stop the clock
proc.time() - ptm #



#---- Draw 2  --------- 
rm(list=setdiff(ls(), c("datetime")))
gc()
#start the clock to time the run
#ptm <- proc.time()
seed_number <- 99
set.seed(seed_number)
source("RscriptsExample/05_MultiDrawMDOcalcs130220.R")
# Stop the clock
#proc.time() - ptm #


#----- Draw  3 ----------
 rm(list=setdiff(ls(), c("datetime")))
 gc()
 seed_number <- 42
 set.seed(seed_number)
 source("RscriptsExample/05_MultiDrawMDOcalcs130220.R")
 
#------------------ Draw 4 ----------------- 
 
 rm(list=setdiff(ls(), c("datetime")))
 gc()
 seed_number <- 10
 set.seed(seed_number)
 source("RscriptsExample/05_MultiDrawMDOcalcs130220.R")
 
#-------- Draw 5 ------------------ 
#repeated code 
 rm(list=setdiff(ls(), c("datetime")))
 gc()
 seed_number <- 90210
 set.seed(seed_number)
 source("RscriptsExample/05_MultiDrawMDOcalcs130220.R")
 
#------ Draw 6 -----------------

#If R crashes between draws, it may be because it
 # is not releasing memory 
 #you can run the commented code if that happens 
 #It might help
# .rs.restartR()
# library(tidyverse)
# library(data.table)
 #setwd("Y:/IPCmodel2019EngCarry15Testing") #ITS1-21 path to r project issues
 #datetime <-  "2020-01-15_16-50-54"

 
 rm(list=setdiff(ls(), c("datetime")))
 gc()
 seed_number <- 8850
 set.seed(seed_number)
 source("RscriptsExample/05_MultiDrawMDOcalcs130220.R") 

#------- Draw 7 --------------
 rm(list=setdiff(ls(), c("datetime")))
 gc()
 seed_number <- 1345
 set.seed(seed_number)
 source("RscriptsExample/05_MultiDrawMDOcalcs130220.R") 
 
# -------- Draw 9 -------------- 
 rm(list=setdiff(ls(), c("datetime")))
 gc()
 seed_number <- 978
 set.seed(seed_number)
 source("RscriptsExample/05_MultiDrawMDOcalcs130220.R")

 
 