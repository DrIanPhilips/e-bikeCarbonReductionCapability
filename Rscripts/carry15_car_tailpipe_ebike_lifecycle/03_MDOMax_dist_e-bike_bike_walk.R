#MDOITS121_carry15

#Calculate the maximum distance given different active travel options (MDO)
#This script estimates how far a person can travel in in an hour by 
#e-bike, bicycle and walking.  
#at a rate of exertion which means they could 
# travel for 1 hour in the morning
#and one hour in an evening 
#each day without risk of over exertion and injury.

#tidy up
if(exists("spc")){rm(spc)}
if(exists("roadslope")){rm(roadslope)}
if(exists("test")){rm(test)}


#--- minor wrangling of the popfile------------

names(popfile)
#remove missing data from popfile (records under 16 years old)
popfile <- na.omit(popfile)
popfile <- popfile %>% filter(age >=16)
gc()


#------- Use functions to calculate MDO --------------- 

#The functions in the R file below are used to calculate the speed and maximum distance. 
source("functions/speedDistanceFuncBikeEbike160120df3_101220.R")

#time the process using this line to start a timer
ptm <- proc.time() 

#This calculates the maximum distance a person can travel by bike or e-bike
#It returns the dataframe with the distances and other variables.  

#Assumptions
#We assume that people carry 15kg  - e.g. change of clothes and a laptop 
#shopping, toddler in a child seat.  


testmdo <- functioncycle_dist(data = popfile,
                              personID = "personID",
                              eW = 250,
                              assistpc = 300,
                              Ww = "power",
                              Vw= "wind",
                              m = "imputeweight",
                              bicycle_weight = 15, 
                              ebike_weight = 20,
                              s = "slope",
                              circuity = 1.4,
                              coeff_rr = 0.008,
                              cargo = 15,
                              dragSlow = 0, #0.08
                              cargoSlow = 0,#0.2
                              return_a_vector = F
                           )
                           

# Stop the timer here
proc.time() - ptm #

#----- join bike and ebike speed estimates to the popfile --

names(popfile)
names(testmdo)
gc()
MDO1 <- left_join(popfile, testmdo, by =("personID"="personID"))
gc()

#tidy up 
rm(popfile)
gc()
rm(testmdo)
gc()


#-------------------- estimate walking speed ------------
source("functions/naismithwalkspeed160120.R")

walkspeedkmh <- naismithwalkspeed(vo2max = MDO1$vo2max_unif,
                                  slope = MDO1$slope,
                                  cargo = 15, #assume carrying 15kg 
                                  )

MDO1  <- cbind(MDO1, walkspeedkmh)
glimpse(MDO1)
gc()
rm(walkspeedkmh)
gc()

names(MDO1)



#------ write intermediate output  -----------

#dir.create("MDOfiles")
MDOname <- paste0("MDOfiles/MDO1_", datetime, ".csv")
#fwrite is faster than write_csv for large files
fwrite(MDO1,MDOname)

#------------------ optional make mdo files foe each district ----

# ptm <- proc.time()  
# for (i in 1:(nrow(LADnames))){
#   #  for (i in 1:2){  
#   test <- MDO1 %>% filter(LAD11NM == LADnames$LAD11NM[i])
#   write_csv(test,paste0("MDOfiles/",LADnames$LAD11NM[i],"MDO.csv"))
# }
# # Stop the clock
# 
