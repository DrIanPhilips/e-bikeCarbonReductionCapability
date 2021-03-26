#MDOITS121_carry15

#This script estimates how far a person can travel in in an hour by 
#bike e-bike and walking.  
#at a rade of exertion which means they could 
#do travel for 1 hour in the morning
#and one hour in an evening 
#each day.  

#tidy up
if(exists("spc")){rm(spc)}
if(exists("roadslope")){rm(roadslope)}
if(exists("test")){rm(test)}


#------- calculate MDO as a function so it can work on whole popfile or a district version --------------- 

#The functions in the R file below are used to calculate the speed and maximum distance. 
source("functions/speedDistanceFuncBikeEbike160120df3.R")
names(popfile)


#remove missing data from popfile (records under 16 years old)
popfile <- na.omit(popfile)
popfile <- popfile %>% filter(age >=16)
gc()

#time the process using this line to start a timer
ptm <- proc.time() 

#This calculates the maximum distance a person can travel by bike or e-bike
#It returns the datafreame with the distances and other variables.  
#We assume that people carry 15kg  - e.g. change of clothes and a laptop 
#shopping, 
#toddler in a child seat.  
#We assume carrying bags, paniers etc will increase drag. 
#8% is a basic estimate 
#https://www.cyclingabout.com/speed-difference-between-panniers-bikepacking-bags-aerodynamic-testing-results/
#We also assume that carrying cargo will increase muscular effort 
#following the basic notion of Langmuir's corrections 
#(Langmiur estimated hiking speeds) that carrying 15kg, 
#would reduce speed by 20% 

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
                              dragSlow = 0.08,
                              cargoSlow = 0.2,
                              return_a_vector = F
                           )
                           

# Stop the timer here
proc.time() - ptm #

#----- join bike and ebike speed estimates to the popfile --
#names(popfile)
#names(testmdo)
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

walkspeedkmh <- naismithwalkspeed(vo2max = MDO1$vo2max_unif,slope = MDO1$slope,cargo = 0.2)

MDO1  <-cbind(MDO1, walkspeedkmh)
glimpse(MDO1)
gc()
rm(walkspeedkmh)
gc()

names(MDO1)



#------ write intermediate output  -----------

#dir.create("MDOfiles")
#write_csv(MDO1,"MDOfiles/MDO1.csv")

MDOname <- paste0("MDOfiles/MDO1_", datetime, ".csv")
#fwtire is fadter than write_csv for large files
fwrite(MDO1,MDOname)

#------------------ optional make mdo files foe each district ----
# ptm <- proc.time()  
# for (i in 1:(nrow(LADnames))){
#   #  for (i in 1:2){  
#   test <- MDO1 %>% filter(LAD11NM == LADnames$LAD11NM[i])
#   write_csv(test,paste0("MDOfiles/",LADnames$LAD11NM[i],"MDO.csv"))
# }
# # Stop the clock
# proc.time() - ptm #  1392
# 
