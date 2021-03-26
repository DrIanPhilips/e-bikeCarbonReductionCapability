
# this script is called several times by 04_senstest
#once for each Monte-Carlo sampling draw.  

library(data.table)
library(tidyverse)


print(paste0("#########################",seed_number,"########"))


print("#--- read results of script 03, max dist by active mode in 1 hour--")
#In script 03 we estimated the physical capability to 
#travel by walking cycling or e-bike for one hour.  
#Below we read in the results of script 03 as MDO1

MDO1 <- fread(paste0("MDOfiles/MDO1_",datetime, ".csv"), 
              drop = c("upspeed","flatspeed","downspeed",
                       "ebikeupspeed","ebikeflatspeed","ebikedownspeed",
                       "kmtime","ebikekmtime",
                       "Ww","Vw","m","s",
                       "maxeuclideandist","ebikemaxeuclideandist") ,#nrows = 10000
              )
gc()
#convert MDO1 from data.table format to tibble 
#so dplyr functions will work on it 
MDO1 <- as_tibble(MDO1)
glimpse(MDO1)
gc()


print("#---- read in cumulative probability of car trip lengths  -----------")

#These contain the proportion car travel recorded in the NTS travel diary 
#in 1km bins upto 35km
# by population subgroup (sex age and OAC supergroup n=128 subgroups)
#there is a file for AM car trips and one for PM car trips
subpopAM <- read_csv("input_data/AMsubpopgroupvarsSexageOACbikeown150321.csv")
subpopPM <- read_csv("input_data/PMsubpopgroupvarsSexageOACbikeown150321.csv")


#Physiologically, 
#a person is physically capable of travelling the equivalent of
#2 hours during the day.
#by walking cycling / ebiking, if i) they do not exceed a particular 
#cardio-vascular threshold - use in the calculation of pedal power
#ii) it is likely that they will have
#a rest from walking / cycling / e-biking in between 
#(see ref to Philips et al 2018 &Philips 2014 in paper )

#To estimate this we look at  car journey km in the AM which could be replaced,
#and separately car journey km in the PM which could be replaced.  
#to get estimates of the proportion of car km which could be replaced 
# by a person capable of travelling a given distance km by active modes.
#I used individual 
#anonymised records from the NTS.

print("#---------------------check summaries -------------------") 
names(MDO1)
#summary(MDO1)
gc()
print("#-------- read in OAC  -----------")
OAC <- read_csv("input_data/2011OACsupergroups.csv")

#------ Join OAC to MDO1 --------
names(OAC)
names(MDO1)
MDO1 <- left_join(MDO1,OAC,by=c("ZoneID" = "OA11CD"))
gc()


print("#--- Join MDO to a weighting factor of annual miles pp by gender age and OAC ----") 

#this gives a weighting to each sex-age-oac segment of the population
#to account for within area variation in travel based on these demographic variables
#this is derived from the 2016 NTS individual anonymous records from UKdata service.  

names(subpopAM)
#make a df which will hold the result of vehicle miles travelled weighting
vmt_weight <- subpopAM %>% select(SexAgeconst, HHoldOAClass2011_B03ID,sumweeklytripdist_mean)

#add together the am and pm mean weekly travel distance from NTS travel diary
vmt_weight$sumweeklytripdist_mean <- subpopPM$sumweeklytripdist_mean + vmt_weight$sumweeklytripdist_mean

# #bar plot to see differences
# p1 <- ggplot( vmt_weight, aes( x = HHoldOAClass2011_B03ID, y = sumweeklytripdist_mean ) ) + 
# #   p1 <- ggplot( vmt_weight, aes( x = SexAgeconst, y = sumweeklytripdist_mean ) ) + 
#      geom_bar( stat = "identity" ) + 
#    facet_wrap( ~  SexAgeconst) + 
#    #facet_wrap( ~  HHoldOAClass2011_B03ID) + 
#    xlab("OAC supergroup")
#  p1
# #ggsave(filename = "vmt_weights_sexage_oac",device = "png")

gc()
#calculate the mean weekly trip distance for all subgroups of the population
meanofmeankmpp <- mean(vmt_weight$sumweeklytripdist_mean)
#to get vmt_weight as a proportion for each subgroup
#calculate ratio weekly trip distance for group x / weekly trip distance for all groups
vmt_weight$vmt_proportion <- vmt_weight$sumweeklytripdist_mean / meanofmeankmpp 

# names(vmt_weight)
# p1 <- ggplot( vmt_weight, aes( x = HHoldOAClass2011_B03ID, y = vmt_proportion ) ) + 
#   geom_bar( stat = "identity" ) + 
#   facet_wrap( ~  SexAgeconst) + 
#   xlab("OAC supergroup")
# p1
#ggsave(filename = "vmt_weights_sexage_oac_vmt_propn",device = "png")

names(MDO1)
names(vmt_weight)

gc()

#Join the vmt_weightings to each individual 
MDO1 <- left_join(MDO1,vmt_weight, by = c("sexageconst" = "SexAgeconst", "Supergroup_Code" = "HHoldOAClass2011_B03ID"))
names(MDO1)
gc()  

print("#------------------------- join MDO to data on Vehicle km travelled by OA ---------")

#The simulated individuals are those who are in a position to alter their emissions
#Children are not simulated, but, we assume chaildren are not making mode choice decisions.  

#MOT data is an estimate of the mean car km per person per LSOA, 
#and this has been allocated to OAs
# as our base population is at OA resolution.  There are on average ~5 OAs per LSOA 

# We make use of km per person to account for vehicle occupancy and avoid double counting.  
# In the simulation we take the vehicle miles travelled per lsoa 
# and in a simple way assign the vehicle miles travelled in an LSOA 
# amongst its population who have access to a car, so our units are 
# vehicle km per person which is different to passenger km per person. 


#This reads in the carkm travelled in every LSOA in England in 2011
MOTdata <- read_csv("input_data/v9carsAndPopOAEngland.csv")

sum(MOTdata$totprivkmOA) # 305665212081  = 306bnkm #private car km travelled in England
#private car km travelled in England in 2011
summary(MOTdata$privkm_pp) 
# mean km travelled by car per person is 5815.3 
#That is total km / the usual resident population 

names(MOTdata)

gc()
glimpse(MOTdata)
names(MDO1)
MDO1 <- left_join(MDO1,MOTdata,by=c("ZoneID" = "OA11CD"))
gc()


print("#---------------- sample which individuals have no access to a car --------")
#read in an LSOA estimate of % of people with no access to a car
#source is census taken from MOT project contextualising dataset and 
#uses data from the 2011 census
nocar <- read_csv("input_data/noCarPr2011LSOA.csv")
names(nocar)
names(MDO1)

# the join links each individual to a probability of having no car
MDO1 <- left_join(MDO1,nocar,by=c("LSOA11CD"= "LSOA11CD"))
#draw the possibility of having no access to a car
MDO1$draw_NoCar <- runif(length(MDO1$NOCAR_pr),0,1) 
#assign whether person has access to a car
MDO1 <- MDO1 %>% mutate(hasCar = ifelse(test = draw_NoCar < NOCAR_pr,
                                        yes = 0,
                                        no = 1)
)

summary(MDO1$hasCar) #NA155133 of 43million simulated individuals

#assume any NA has access to a car 
MDO1 <- MDO1 %>% mutate(hasCar = ifelse(test = is.na(hasCar),
                                        yes = 1,
                                        no = hasCar)
)

#check that number of people with car is sensible
sum(MDO1$hasCar) # 34095783 simulated adults in England have access to a private car.  
sum(MDO1$hasCar) / nrow(MDO1) #80%
# 0.8009834 #0.8045608
#This is close to the mean % of LSOA residents with no car
mean(nocar$NOCAR_pr)
##[1] 0.1930372



print("#---- get number of people per OA with access to a car ------------")

MDO2 <- MDO1 %>% select(ZoneID, totprivkmOA,hasCar,privkm_pp)
groups <- group_by(MDO2,ZoneID)
gc()

#make a df of km per person (whole population) and include number of people with a car
simpop_caraccess_OA <- dplyr::summarize(groups,adultsimOA = n(),
                                 totprivkm = mean(totprivkmOA),
                                 privkm_pp = mean(privkm_pp),
                                 hasCarOA = sum(hasCar)
)
summary(simpop_caraccess_OA$hasCarOA)
sum(simpop_caraccess_OA$totprivkm) # 305641586317 # This is privkm in England
#The MOt data read in above is for England 

print("# -------assign km per simulated person based on who has access to a car -----------")
simpop_caraccess_OA$kmppsim_car <- simpop_caraccess_OA$totprivkm / simpop_caraccess_OA$hasCarOA

sum(simpop_caraccess_OA$totprivkm) # this should be 305bn, check = yes.

names(simpop_caraccess_OA)
simpop_caraccess_OA2 <- simpop_caraccess_OA %>% select(ZoneID,kmppsim_car)

#join 
MDO1 <- left_join(MDO1,simpop_caraccess_OA2, by = c("ZoneID" = "ZoneID"))
#set people without car access to zero car use 
#acknowledging this is a simplifying assumption.   
MDO1$kmppsim_car[MDO1$hasCar == 0] <- 0 


names(MDO1)
sum(MDO1$kmppsim_car)#305641586317

#we have assigned the private car km travelled to those
#simulated adults who have access to a car.  
#but we have not yet adjusted for demographics e.g.  
#age and gender are associated with difference in how far people drive



print("#----------- assign demographic weighting to car km per person ---------")
#using the demographic weighting metric "vmt_proportion" 
#weight vkm by demographics
#if no car then kmppsim_car = 0
MDO1 <- MDO1 %>% mutate(
  weightedkm_pp = vmt_proportion * kmppsim_car)

sum(MDO1$weightedkm_pp) # 352bn so this is now an over estimate of total km travelled
#next we have to calibrate the results 

print("#------------calibrate to the total private car km------------- ")

#To account for a difference in totals we need to use a calibration_value 
calibration_value <-  sum(MDO1$weightedkm_pp) / sum(simpop_caraccess_OA$totprivkm)
calibration_value #1.15
sum(MDO1$weightedkm_pp) # 352409628248 
#Once we weight by demographics, we get an over estimate of 
#total car km

MDO1$weightedkm_pp <- MDO1$weightedkm_pp / calibration_value
sum(MDO1$weightedkm_pp) #305641586317 # This is the total km travelled in the MOTdataset

#demogweighted is not still used.  However
#before deleting check that it doesn't affect any references to column numbers
MDO1$demogweighted <- MDO1$weightedkm_pp

sum(MDO1$demogweighted) # 305641586317



print("#---------------- prepare lookup of bike availabilty  -----------------") 
#prepare bikelookup 
#make a lookup which has for each combination of 
#sexage and OAC a proportion of that subgroup who own a bike.  
#Data for this is based on NTS individual anonymous records 2016.  
#the proportion of people in each subgroup with a bike are in the subpop data frames
names(subpopAM)
bikelookup <- subpopAM %>% select(SexAgeconst = SexAgeconst, 
                                  OAC = HHoldOAClass2011_B03ID,
                                  HasBike_mean = HasBike_mean)
MDO1 <- left_join(MDO1,bikelookup,by =c("sexageconst" = "SexAgeconst",
                                        "Supergroup_Code" = "OAC"))

gc()


#---------- tidy --------
rm(OAC)
rm(MOTdata)
gc()

print("#----subset the variables in MDO1 so that there is not too much data in memory ---") 
names(MDO1)

MDOVarsKeep <- c("personID","weightedkm_pp","maxroaddist",
                 "walkspeedkmh","HasBike_mean","co2EF" ,
                 #"emfac_co2_av_Y",
                 "Supergroup_Code","sexageconst",
                 "ebikemaxroaddist","sexw","age",
                 "hasCar","demogweighted")

MDO1 <- MDO1 %>% select(MDOVarsKeep)
gc()
#filter any missing values from MDO1
MDO1 <- MDO1 %>% filter(weightedkm_pp >= 0)
gc()
names(MDO1)
#change the name of emfac so it works in the functions 
MDO1 <- MDO1 %>% rename(emfac_co2_av_Y = co2EF) #For renaming tibble column using dplyrpipe 



print("#---------- use functions to calculate MDD and km savings --------- ")

#read in functions
#functionDODrawDistandSavings calculates the replaceable car km and the co2 saving
source("functions/functionDODrawDistandSavingsALL081220.R",local = TRUE)


#function_MDD_savings290819 is called by the functionDODrawDistandSavings,
#it draws whether people 
#have a bike / ebike or not, and sets their maximum travel distance
# their Maximum Drawn Distance (MDD). 
source("functions/function_MDD_savings290819.R",local = TRUE)

gc()

print("#------- draw a value to compare against probability of having a bike ----------") 

MDO1$draw_bike <- runif(length(MDO1$HasBike_mean),0,1) 
names(MDO1)



#scenarios 1,2,4, are those discussed in the paper 
print("########---- Scenario 1 current walking and cycling no e-bikes ------")

#ptm <- proc.time()
returned <-functionDODrawDistSavingsALL081220(roaddist = MDO1$maxroaddist,draw_bike = MDO1$draw_bike)
MDO1$MDD <- returned$roundedMDD
MDO1$MDDkmreplace <- returned$replaceabledistance
#added 160321 93gkm is fuelcycle 47gkm  + manufacture 46gkm from carbon brief 2019 
#https://www.carbonbrief.org/factcheck-how-electric-vehicles-help-to-tackle-climate-change
returned$co2save <- returned$replaceabledistance * (returned$emfac_co2 + 93)

MDO1$MDDco2save <- returned$co2save

rm(returned)
#proc.time() - ptm 
gc()

print("######## Scenario 2  everyone has a bike --------------")
#in this scenario all parameters in the function are the defaults
returned <-functionDODrawDistSavingsALL081220()
MDO1$AllBikekmreplace <- returned$replaceabledistance
#added 160321 93gkm is fuelcycle 47gkm  + manufacture 46gkm from carbon brief 2019 
#https://www.carbonbrief.org/factcheck-how-electric-vehicles-help-to-tackle-climate-change
returned$co2save <- returned$replaceabledistance * (returned$emfac_co2 + 93)


MDO1$Allbikeco2save <- returned$co2save
rm(returned)
gc()

print("######## Scenario 3  current bike owners have an e-bike ----") 
returned <-functionDODrawDistSavingsALL081220(roaddist = MDO1$ebikemaxroaddist,draw_bike = MDO1$draw_bike)
MDO1$currentEBike <- returned$drawMDD
MDO1$currentEBikekmreplace <- returned$replaceabledistance
#added 160321 93gkm is fuelcycle 47gkm  + manufacture 46gkm from carbon brief 2019 
#https://www.carbonbrief.org/factcheck-how-electric-vehicles-help-to-tackle-climate-change
returned$co2save <- returned$replaceabledistance * (returned$emfac_co2 + 93)


MDO1$currentEbikeco2save <- returned$co2save
rm(returned)
gc()


print("######## Scenario 4 everyone has  an e-bike ----") 
returned <-functionDODrawDistSavingsALL081220(roaddist = MDO1$ebikemaxroaddist,draw_bike = NULL)
MDO1$AllEBikekmreplace <- returned$replaceabledistance
#added 160321 93gkm is fuelcycle 47gkm  + manufacture 46gkm from carbon brief 2019 
#https://www.carbonbrief.org/factcheck-how-electric-vehicles-help-to-tackle-climate-change
returned$co2save <- returned$replaceabledistance * (returned$emfac_co2 + 93)


MDO1$AllEbikeco2save <- returned$co2save
rm(returned)
gc()




# we experimented with other scenarios but didn't develop them fully 


print("####### Scenario 5 Cairns 20% km replaced with an e-bike ---")  
#Cairns et al 2017 carried out a study in Brighton UK, with an e-bike loan intervention which 
#resulted in study participants replacing 20% of their car km 
#with e-bike use.  
#2/3 live within 5 miles of work where the scheme is based
#Brighton's urban extent is about 5 miles from seafront to city edge.  
#there are other towns within 10 miles (92 % lived within 10 miles of work)
#we made the assumption that virtually none of the
#people in the study live in rural areas
#Also, almost all the participants (95%) were under 60.   
#

MDO1$cairns<- 0
#Supergroup 1 is rural
MDO1$cairns[MDO1$Supergroup_Code != 1 & MDO1$age < 60 ]<-  0.2 * MDO1$weightedkm_pp
summary(MDO1$cairns)
MDO1$cairnsco2save <- MDO1$cairns * (MDO1$emfac_co2_av_Y +93)
summary(MDO1$cairnsco2save)

print("####### Scenario 6 Just walking --------")
#assume walking is the only option. 
returned <-functionDODrawDistSavingsALL081220(roaddist = MDO1$walkspeedkmh,draw_bike = NULL)
MDO1$Walkkmreplace <- returned$replaceabledistance
#added 160321 93gkm is fuelcycle 47gkm  + manufacture 46gkm from carbon brief 2019 
#https://www.carbonbrief.org/factcheck-how-electric-vehicles-help-to-tackle-climate-change
returned$co2save <- returned$replaceabledistance * (returned$emfac_co2 + 93)


MDO1$Walkco2save <- returned$co2save
rm(returned)
gc()

print("########Scenario 7 walkers get e-bikes cyclists ride their bikes")
#If in MDD the person is deemed not to have a bike, then give them an 
# #ebike
MDO1 <- MDO1 %>% mutate(walktoebikekmreplace = ifelse(test = draw_bike <= HasBike_mean, 
                                                      yes = AllBikekmreplace,
                                                      no = AllEBikekmreplace
)
)

#estimate the co2saving using the emissions factor
MDO1$walktoebikeco2save <-  MDO1$walktoebikekmreplace * MDO1$emfac_co2_av_Y
gc()


#The following scenarios are berhavioural scenarios not capability indicators
print("#------------ scenario e-bikes 8 -30 minute neighbourhood ------")
#people are willing to travel upto 30 minutes in the morning
#and again in the evening
#for justification see literature on 'the 30 minute city'
#Cairns et al 2017 found the median time spent using an e-bike 
#amongst their participants is 60 minutes per day.
#the marchetti constant is also a well known hypothesis 
#that people are willing to commute 30 mins each way per day. 
#this is a scenario taking account of behaviour soa as such isn't a capability scenario


#In this scenario we halve the distance people can travel am and pm 
#by halving the maxroad distance because we halve the time.  
returned <-functionDODrawDistSavingsALL081220(roaddist = MDO1$ebikemaxroaddist/2,draw_bike = NULL)
MDO1$AllEBikekmreplace30m <- returned$replaceabledistance
#added 160321 93gkm is fuelcycle 47gkm  + manufacture 46gkm from carbon brief 2019 
#https://www.carbonbrief.org/factcheck-how-electric-vehicles-help-to-tackle-climate-change
returned$co2save <- returned$replaceabledistance * (returned$emfac_co2 + 93)


MDO1$AllEbikeco2save30m <- returned$co2save
rm(returned)
gc()


print("#-------------scenario 9 ebikes - flex mobility ---- ")
#people choose not to use active modes every day, 
#on average they use their e-bikes 1.88 days per week (0.26915 of the time)
#this is the mean of the distribution of usage in the study by Cairns et al 2017
#see their figure 1.


MDO1$AllEBikekmreplaceFlex <- MDO1$AllEBikekmreplace * 0.26915
MDO1$AllEbikeco2saveFlex <- MDO1$AllEbikeco2save * 0.26915
gc()


print("#------------scenario 10 ebikes  - flex mobility and 30 minute neighbourhood")

MDO1$AllEBikekmreplace30mFlex <-  MDO1$AllEBikekmreplace30m * 0.26915
MDO1$AllEbikeco2save30mFlex <- MDO1$AllEbikeco2save30m * 0.26915
gc()





print("#-- accounting for emissions of building e-bikes and riding bikes ebikes and walking-")

#ECF (2011) estimate lifecycle values
#source: 
#https://ecf.com/sites/ecf.com/files/ECF_CO2_WEB.pdf (See p9-11)
#with e-bikes having lower energy use emissions
#because they consider the production emissions of food used by 
#bike riders vs the renewable electricity production of e-bike users.  
#Berners Lee (There is no Planet B) also points this out.  
ECFbikeLifecyle <- 21 #g/gm
ECFebikeLifecycle <- 22 #g/km
#we will use this with MDD because no new bikes are bought here
#we assum just continued use of current bike fleet
ECFbikewelltoWheel <- 21 - 5 #16g/km 
names(MDO1)


#above we have calculated the reduction in car emissions.  
#We account for the emissions associated with walking, cycling and e-biking 
#associated with all the saved km
#for MDD it is well to wheel because the bikes are already available
#and don't need making
gc()
#Berners-Lee in there is no planet B estimates the calories per km of
#walking is approx double that of cycling
#below yes is has bike, no is walk
MDO1 <- MDO1 %>% 
  mutate(MDDco2save = 
           ifelse(test = draw_bike <= HasBike_mean, 
                  yes = MDDco2save - (ECFbikewelltoWheel * MDO1$MDDkmreplace),
                  no = MDDco2save - (2 * ECFbikewelltoWheel *  MDO1$MDDkmreplace)
           )
  )

#ggplot(MDO1)+
#  geom_histogram(aes(MDDco2save))
#ggsave("MDDco2Save121119.png")
gc()
#If we give everyone an ebike then we 
#have to produce them so we have to have a value for e-bike lifecycle
#including production
MDO1$AllEbikeco2save <- MDO1$AllEbikeco2save - (ECFebikeLifecycle * MDO1$AllEBikekmreplace)
#summary(MDO1$AllBikeco2save) 
MDO1$Allbikeco2save <- MDO1$Allbikeco2save - (MDO1$AllBikekmreplace * ECFbikeLifecyle)

#apply the ammended co2 savings to scenario 7 results
#and ammend the other scenarios where necessary
MDO1 <-  MDO1 %>% 
  mutate(walktoebikeco2save = 
           ifelse(test = draw_bike <= HasBike_mean, 
                  yes = Allbikeco2save,
                  no = AllEbikeco2save
           )
  )
gc()
MDO1  <-  MDO1 %>% 
  mutate(walktoebikeco2save = ifelse(test = draw_bike <= HasBike_mean, 
                                     yes = AllEbikeco2save,
                                     no = Walkco2save
  )
  )
gc()
MDO1$cairnsco2save <- MDO1$cairnsco2save - (MDO1$cairns * ECFebikeLifecycle)

gc()


#---------- account for ebike emissions scenarios 8-10 --- 

MDO1$AllEbikeco2save30m <- MDO1$AllEbikeco2save30m - (ECFebikeLifecycle * MDO1$AllEBikekmreplace30m)
MDO1$AllEbikeco2saveFlex <- MDO1$AllEbikeco2saveFlex - (ECFebikeLifecycle * MDO1$AllEBikekmreplaceFlex)
MDO1$AllEbikeco2save30mFlex <- MDO1$AllEbikeco2save30mFlex - (ECFebikeLifecycle * MDO1$AllEBikekmreplace30mFlex)


#---- compare km saved  to total car travel ------------ 

MDO1  <-  MDO1 %>% 
  mutate(kmEbike_mdd = ifelse(test = (AllEBikekmreplace -  MDDkmreplace) < 1
                              & hasCar == 1 &
                                maxroaddist > 2 , 
                              yes = 1,
                              no = 0
  )
  )

gc()
MDO1  <-  MDO1 %>% 
  mutate(kmEbike_car = ifelse(test = (weightedkm_pp - AllEBikekmreplace) < 1 
                              & hasCar == 1 & maxroaddist > 2 , 
                              yes = 1,
                              no = 0
  )
  )

MDO1  <-  MDO1 %>% 
  mutate(kmEbike_car30m = ifelse(test = (weightedkm_pp - AllEBikekmreplace30m) < 1 
                                 & hasCar == 1 & maxroaddist > 2 , 
                                 yes = 1,
                                 no = 0
  )
  )

MDO1  <-  MDO1 %>% 
  mutate(kmmdd_car = ifelse(test = (weightedkm_pp - MDDkmreplace) < 1 
                            & hasCar == 1 & maxroaddist > 2 , 
                            yes = 1,
                            no = 0
  )
  )

MDO1  <-  MDO1 %>% 
  mutate(kmcarmddno_ebikeyes = ifelse(test =(kmmdd_car ==0 &
                                               kmEbike_car == 1 &
                                               hasCar == 1 &
                                               maxroaddist > 2), 
                                      yes = 1,
                                      no = 0
  )
  )



gc()

print("#--- calculate differences between key scenarios---------- ")
MDO1$diffMDD_currentEbikeco2save <- MDO1$currentEbikeco2save - MDO1$MDDco2save
MDO1$diffAllbike_AllEbikeco2save <- MDO1$AllEbikeco2save - MDO1$Allbikeco2save
MDO1$diffMDDco2save_AllEbikeco2save <- MDO1$AllEbikeco2save - MDO1$MDDco2save

MDO1$diffAllEbikeco2save_AllEbikeco2save30m <- MDO1$AllEbikeco2save - MDO1$AllEbikeco2save30m
MDO1$diffAllEbikeco2save_AllEbikeco2saveFlex <- MDO1$AllEbikeco2save - MDO1$AllEbikeco2saveFlex
MDO1$diffAllEbikeco2save_AllEbikeco2save30mFlex <- MDO1$AllEbikeco2save - MDO1$AllEbikeco2save30mFlex

gc()

print("#------- Make a sumary of all the variables --------") 
summarydf <-   do.call(cbind, lapply(MDO1, summary))
sumarydf <- as.data.frame(summarydf)
tsummarydf <- as.data.frame(t(summarydf)) 
tsummarydf$namesvec <- as.vector((names(MDO1)))
write_csv(tsummarydf, paste0("Results",datetime,"/","summaryMDO1vars",seed_number,"_", datetime, ".csv"))
gc()


#---- tidy ----- 
rm(MDO1k)
rm(MDOVarsKeep)
rm(bikelookup)
rm(vmt_weight)
rm(subpopAM)
rm(subpopPM)
rm(filtered)
rm(test_cardraw)
rm(lsoa_test)
rm(nocar)
rm(p1)
rm(ECFbikeLifecyle,ECFbikewelltoWheel,ECFebikeLifecycle,
   function_MDD,functionDODrawDistSavings040919,
   functionDODrawDistSavingsALLBIKE040919)

rm(summarydf,sumarydf,tsummarydf)
rm(WeisslifeCycleBike,WeisslifeCycleEBike,
   WeissWellWheelBike,WeissWellWheelEbike
)
gc()


#remove any unnecessary columns from MDO1
MDO1 <- MDO1 %>% select(-c(demogweighted,draw_bike))
gc()


print("#------- aggregate to LSOA -----------------------")
names(MDO1)
gc()

# read in and join to geographical identifiers
MDO3 <- fread(paste0("MDOfiles/MDO1_",datetime, ".csv"),
              select = c("ZoneID","personID","LSOA11CD")
)
gc()
MDO3 <- as_tibble(MDO3)
glimpse(MDO3)
gc()
# Join
MDO1 <- left_join(MDO1,MDO3, by = c("personID"="personID"))
gc()
glimpse(MDO1)
names(MDO1)
rm(MDO3)
gc()


#using datatable here is less memory hungry than dplyr.  
MDO1 <- as.data.table(MDO1)
gc()
MDO_LSOA <- MDO1[,.(totaladultsim = .N,                        
                        meanweightedkm_pp = mean(weightedkm_pp,na.rm = T),
                        sumweightedkm_pp = sum(weightedkm_pp,na.rm = T),
                        meanmaxbikedist = mean(maxroaddist,na.rm = T),                
                        meanwalkspeedkmh =   mean(walkspeedkmh,na.rm = T), 
                        HasBike_mean = mean(HasBike_mean,na.rm = T),
                        emfac_co2_av_Y = mean(emfac_co2_av_Y,na.rm = T),             
                        countMale = sum(sexw,na.rm = T),
                        meanage =  mean(age,na.rm = T),
                        meanMDD = mean(MDD,na.rm = T), 
                        meanMDDkmreplace = mean(MDDkmreplace,na.rm = T), 
                        sumMDDkmreplace = sum(MDDkmreplace,na.rm = T), 
                        meanMDDco2save = mean(MDDco2save,na.rm = T),    
                        sumMDDco2save = sum(MDDco2save,na.rm = T), 
                        meanAllBikekmreplace = mean(AllBikekmreplace,na.rm = T),  
                        sumAllBikekmreplace = sum(AllBikekmreplace,na.rm = T),  
                        meanAllbikeco2save =mean(Allbikeco2save,na.rm = T),
                        sumAllbikeco2save = sum(Allbikeco2save,na.rm = T),
                        meancurrentEBike = mean(currentEBike,na.rm=T),  
                        sumcurrentEBike = sum(currentEBike,na.rm=T), 
                        meancurrentEBikekmreplace = mean(currentEBikekmreplace,na.rm = T),
                        sumcurrentEBikekmreplace = sum(currentEBikekmreplace,na.rm = T),
                        meancurrentEbikeco2save = mean(currentEbikeco2save,na.rm = T), 
                        sumcurrentEbikeco2save = sum(currentEbikeco2save,na.rm = T),
                        meanAllEBikekmreplace = mean(AllEBikekmreplace,na.rm=T),          
                        sumAllEBikekmreplace = sum(AllEBikekmreplace,na.rm=T), 
                        meanAllEbikeco2save = mean(AllEbikeco2save,na.rm = T), 
                        sumAllEbikeco2save = sum(AllEbikeco2save,na.rm = T),
                        meancairns = mean(cairns,na.rm= T),  
                        sumcairns = sum(cairns,na.rm= T),    
                        meancairnsco2save = mean(cairnsco2save,na.rm = T),              
                        sumcairnsco2save = sum(cairnsco2save,na.rm = T), 
                        meanWalkkmreplace = mean(Walkkmreplace,na.rm = T),
                        sumWalkkmreplace = sum(Walkkmreplace,na.rm = T), 
                        meanWalkco2save = mean(Walkco2save,na.rm = T),
                        sumWalkco2save = sum(Walkco2save,na.rm = T),
                        meanwalktoebikekmreplace = mean(walktoebikekmreplace,na.rm=T), 
                        sumwalktoebikekmreplace = sum(walktoebikekmreplace,na.rm=T),
                        meanwalktoebikeco2save = mean(walktoebikeco2save,na.rm = T),
                        sumwalktoebikeco2save = sum(walktoebikeco2save,na.rm = T),
                        meandiffMDD_currentEbikeco2save = mean(diffMDD_currentEbikeco2save,na.rm = T),
                        sumdiffMDD_currentEbikeco2save = sum(diffMDD_currentEbikeco2save,na.rm = T),
                        meandiffAllbike_AllEbikeco2save = mean(diffAllbike_AllEbikeco2save,na.rm = T),
                        sumdiffAllbike_AllEbikeco2save = sum(diffAllbike_AllEbikeco2save,na.rm = T),
                        sumdiffMDDco2save_AllEbikeco2save = sum(diffMDDco2save_AllEbikeco2save,na.rm=T),
                        meandiffMDDco2save_AllEbikeco2save = mean(diffMDDco2save_AllEbikeco2save,na.rm=T),
                        caraccess = sum(hasCar,na.rm = T),
                        sumkmEbike_mdd = sum(kmEbike_mdd,na.rm=T),
                        sumkmmdd_car = sum(kmmdd_car,na.rm=T),
                        sumEbike_car = sum(kmEbike_car,na.rm=T),
                        sumkmcarmddno_ebikeyes = sum(kmcarmddno_ebikeyes,na.rm=T),
                        
                        
                        meanAllEBikekmreplace30m = mean(AllEBikekmreplace30m,na.rm=T),          
                        sumAllEBikekmreplace30m = sum(AllEBikekmreplace30m,na.rm=T), 
                        meanAllEbikeco2save30m = mean(AllEbikeco2save30m,na.rm = T), 
                        sumAllEbikeco2save30m = sum(AllEbikeco2save30m,na.rm = T),
                        
                        
                        
                        meanAllEBikekmreplaceFlex = mean(AllEBikekmreplaceFlex,na.rm=T),          
                        sumAllEBikekmreplaceFlex = sum(AllEBikekmreplaceFlex,na.rm=T), 
                        meanAllEbikeco2saveFlex = mean(AllEbikeco2saveFlex,na.rm = T), 
                        sumAllEbikeco2saveFlex = sum(AllEbikeco2saveFlex,na.rm = T),							 
                        
                        meanAllEBikekmreplace30mFlex = mean(AllEBikekmreplace30mFlex,na.rm=T),          
                        sumAllEBikekmreplace30mFlex = sum(AllEBikekmreplace30mFlex,na.rm=T), 
                        meanAllEbikeco2save30mFlex = mean(AllEbikeco2save30mFlex,na.rm = T), 
                        sumAllEbikeco2save30mFlex = sum(AllEbikeco2save30mFlex,na.rm = T),
                        
                        kmEbike_mdd  =      sum( kmEbike_mdd,na.rm = T),                       
                        kmEbike_car   =      sum(kmEbike_car,na.rm = T),                      
                        kmmdd_car      =     sum(kmmdd_car,na.rm = T),                      
                        kmcarmddno_ebikeyes= sum(kmcarmddno_ebikeyes,na.rm = T),
                        kmEbike_car30m = sum(kmEbike_car30m,na.rm = T),
                        
                        meandiffAllEbikeco2save_AllEbikeco2save30m   = mean(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                        meandiffAllEbikeco2save_AllEbikeco2saveFlex  = mean(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                        meandiffAllEbikeco2save_AllEbikeco2save30mFlex = mean(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                        sumdiffAllEbikeco2save_AllEbikeco2save30m   = sum(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                        sumdiffAllEbikeco2save_AllEbikeco2saveFlex  = sum(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                        sumdiffAllEbikeco2save_AllEbikeco2save30mFlex = sum(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                        ebikeDailyDist = mean(ebikemaxroaddist,na.rm = T)
),by = LSOA11CD
]
gc()

write_csv(MDO_LSOA,paste0("Results",datetime,"/","MDO_LSOA",seed_number,"_", datetime, ".csv"))

# print("#-------- get Max ebike daily distance ----- ")
# 
# #this could be tidied up so that the value ebikemaxroaddist
# #is included in the table above.  
# #note that walk distance in 1 hour walkspeedkmh 
# # and bike distance in 1 hour maxroaddist 
# #and distance by bike if you have one walk otherwise MDD
# #are already in the aggregation above.   
# 

# #Make a histogram of every variable (not run)
# namevec <- names(MDO1)
# for(i in 1:length(namevec)){
#   hist(MDO1[[i]])
#   savePlot(filename = paste0(nameveci,"CARRY15"),type = "png")
# }

#rm(groups)
#gc()
#in multiple loops don't write this out
#fwrite(MDO1, "MDOfiles/MDO1complete121119.csv")
gc()



print("#---------------------- summarise results by OAC -------------")


MDO_supergroup <- MDO1[,.(totaladultsim = .N,                        
                    meanweightedkm_pp = mean(weightedkm_pp,na.rm = T),
                    sumweightedkm_pp = sum(weightedkm_pp,na.rm = T),
                    meanmaxbikedist = mean(maxroaddist,na.rm = T),                
                    meanwalkspeedkmh =   mean(walkspeedkmh,na.rm = T), 
                    HasBike_mean = mean(HasBike_mean,na.rm = T),
                    emfac_co2_av_Y = mean(emfac_co2_av_Y,na.rm = T),             
                    countMale = sum(sexw,na.rm = T),
                    meanage =  mean(age,na.rm = T),
                    meanMDD = mean(MDD,na.rm = T), 
                    meanMDDkmreplace = mean(MDDkmreplace,na.rm = T), 
                    sumMDDkmreplace = sum(MDDkmreplace,na.rm = T), 
                    meanMDDco2save = mean(MDDco2save,na.rm = T),    
                    sumMDDco2save = sum(MDDco2save,na.rm = T), 
                    meanAllBikekmreplace = mean(AllBikekmreplace,na.rm = T),  
                    sumAllBikekmreplace = sum(AllBikekmreplace,na.rm = T),  
                    meanAllbikeco2save =mean(Allbikeco2save,na.rm = T),
                    sumAllbikeco2save = sum(Allbikeco2save,na.rm = T),
                    meancurrentEBike = mean(currentEBike,na.rm=T),  
                    sumcurrentEBike = sum(currentEBike,na.rm=T), 
                    meancurrentEBikekmreplace = mean(currentEBikekmreplace,na.rm = T),
                    sumcurrentEBikekmreplace = sum(currentEBikekmreplace,na.rm = T),
                    meancurrentEbikeco2save = mean(currentEbikeco2save,na.rm = T), 
                    sumcurrentEbikeco2save = sum(currentEbikeco2save,na.rm = T),
                    meanAllEBikekmreplace = mean(AllEBikekmreplace,na.rm=T),          
                    sumAllEBikekmreplace = sum(AllEBikekmreplace,na.rm=T), 
                    meanAllEbikeco2save = mean(AllEbikeco2save,na.rm = T), 
                    sumAllEbikeco2save = sum(AllEbikeco2save,na.rm = T),
                    meancairns = mean(cairns,na.rm= T),  
                    sumcairns = sum(cairns,na.rm= T),    
                    meancairnsco2save = mean(cairnsco2save,na.rm = T),              
                    sumcairnsco2save = sum(cairnsco2save,na.rm = T), 
                    meanWalkkmreplace = mean(Walkkmreplace,na.rm = T),
                    sumWalkkmreplace = sum(Walkkmreplace,na.rm = T), 
                    meanWalkco2save = mean(Walkco2save,na.rm = T),
                    sumWalkco2save = sum(Walkco2save,na.rm = T),
                    meanwalktoebikekmreplace = mean(walktoebikekmreplace,na.rm=T), 
                    sumwalktoebikekmreplace = sum(walktoebikekmreplace,na.rm=T),
                    meanwalktoebikeco2save = mean(walktoebikeco2save,na.rm = T),
                    sumwalktoebikeco2save = sum(walktoebikeco2save,na.rm = T),
                    meandiffMDD_currentEbikeco2save = mean(diffMDD_currentEbikeco2save,na.rm = T),
                    sumdiffMDD_currentEbikeco2save = sum(diffMDD_currentEbikeco2save,na.rm = T),
                    meandiffAllbike_AllEbikeco2save = mean(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffAllbike_AllEbikeco2save = sum(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffMDDco2save_AllEbikeco2save = sum(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    meandiffMDDco2save_AllEbikeco2save = mean(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    caraccess = sum(hasCar,na.rm = T),
                    sumkmEbike_mdd = sum(kmEbike_mdd,na.rm=T),
                    sumkmmdd_car = sum(kmmdd_car,na.rm=T),
                    sumEbike_car = sum(kmEbike_car,na.rm=T),
                    sumkmcarmddno_ebikeyes = sum(kmcarmddno_ebikeyes,na.rm=T),
                    
                    
                    meanAllEBikekmreplace30m = mean(AllEBikekmreplace30m,na.rm=T),          
                    sumAllEBikekmreplace30m = sum(AllEBikekmreplace30m,na.rm=T), 
                    meanAllEbikeco2save30m = mean(AllEbikeco2save30m,na.rm = T), 
                    sumAllEbikeco2save30m = sum(AllEbikeco2save30m,na.rm = T),
                    
                    
                    
                    meanAllEBikekmreplaceFlex = mean(AllEBikekmreplaceFlex,na.rm=T),          
                    sumAllEBikekmreplaceFlex = sum(AllEBikekmreplaceFlex,na.rm=T), 
                    meanAllEbikeco2saveFlex = mean(AllEbikeco2saveFlex,na.rm = T), 
                    sumAllEbikeco2saveFlex = sum(AllEbikeco2saveFlex,na.rm = T),							 
                    
                    meanAllEBikekmreplace30mFlex = mean(AllEBikekmreplace30mFlex,na.rm=T),          
                    sumAllEBikekmreplace30mFlex = sum(AllEBikekmreplace30mFlex,na.rm=T), 
                    meanAllEbikeco2save30mFlex = mean(AllEbikeco2save30mFlex,na.rm = T), 
                    sumAllEbikeco2save30mFlex = sum(AllEbikeco2save30mFlex,na.rm = T),
                    
                    kmEbike_mdd  =      sum( kmEbike_mdd,na.rm = T),                       
                    kmEbike_car   =      sum(kmEbike_car,na.rm = T),                      
                    kmmdd_car      =     sum(kmmdd_car,na.rm = T),                      
                    kmcarmddno_ebikeyes= sum(kmcarmddno_ebikeyes,na.rm = T),
                    kmEbike_car30m = sum(kmEbike_car30m,na.rm = T),
                    
                    meandiffAllEbikeco2save_AllEbikeco2save30m   = mean(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2saveFlex  = mean(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2save30mFlex = mean(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30m   = sum(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2saveFlex  = sum(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30mFlex = sum(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    ebikeDailyDist = mean(ebikemaxroaddist,na.rm = T)
),by = Supergroup_Code
]

gc()


write_csv(MDO_supergroup,paste0("Results",datetime,"/","MDO1_Supergroup",seed_number,"_", datetime, ".csv"))

#------------------- summarise by age and gender ----------------


MDO_sexage <- MDO1[,.(totaladultsim = .N,                        
                    meanweightedkm_pp = mean(weightedkm_pp,na.rm = T),
                    sumweightedkm_pp = sum(weightedkm_pp,na.rm = T),
                    meanmaxbikedist = mean(maxroaddist,na.rm = T),                
                    meanwalkspeedkmh =   mean(walkspeedkmh,na.rm = T), 
                    HasBike_mean = mean(HasBike_mean,na.rm = T),
                    emfac_co2_av_Y = mean(emfac_co2_av_Y,na.rm = T),             
                    countMale = sum(sexw,na.rm = T),
                    meanage =  mean(age,na.rm = T),
                    meanMDD = mean(MDD,na.rm = T), 
                    meanMDDkmreplace = mean(MDDkmreplace,na.rm = T), 
                    sumMDDkmreplace = sum(MDDkmreplace,na.rm = T), 
                    meanMDDco2save = mean(MDDco2save,na.rm = T),    
                    sumMDDco2save = sum(MDDco2save,na.rm = T), 
                    meanAllBikekmreplace = mean(AllBikekmreplace,na.rm = T),  
                    sumAllBikekmreplace = sum(AllBikekmreplace,na.rm = T),  
                    meanAllbikeco2save =mean(Allbikeco2save,na.rm = T),
                    sumAllbikeco2save = sum(Allbikeco2save,na.rm = T),
                    meancurrentEBike = mean(currentEBike,na.rm=T),  
                    sumcurrentEBike = sum(currentEBike,na.rm=T), 
                    meancurrentEBikekmreplace = mean(currentEBikekmreplace,na.rm = T),
                    sumcurrentEBikekmreplace = sum(currentEBikekmreplace,na.rm = T),
                    meancurrentEbikeco2save = mean(currentEbikeco2save,na.rm = T), 
                    sumcurrentEbikeco2save = sum(currentEbikeco2save,na.rm = T),
                    meanAllEBikekmreplace = mean(AllEBikekmreplace,na.rm=T),          
                    sumAllEBikekmreplace = sum(AllEBikekmreplace,na.rm=T), 
                    meanAllEbikeco2save = mean(AllEbikeco2save,na.rm = T), 
                    sumAllEbikeco2save = sum(AllEbikeco2save,na.rm = T),
                    meancairns = mean(cairns,na.rm= T),  
                    sumcairns = sum(cairns,na.rm= T),    
                    meancairnsco2save = mean(cairnsco2save,na.rm = T),              
                    sumcairnsco2save = sum(cairnsco2save,na.rm = T), 
                    meanWalkkmreplace = mean(Walkkmreplace,na.rm = T),
                    sumWalkkmreplace = sum(Walkkmreplace,na.rm = T), 
                    meanWalkco2save = mean(Walkco2save,na.rm = T),
                    sumWalkco2save = sum(Walkco2save,na.rm = T),
                    meanwalktoebikekmreplace = mean(walktoebikekmreplace,na.rm=T), 
                    sumwalktoebikekmreplace = sum(walktoebikekmreplace,na.rm=T),
                    meanwalktoebikeco2save = mean(walktoebikeco2save,na.rm = T),
                    sumwalktoebikeco2save = sum(walktoebikeco2save,na.rm = T),
                    meandiffMDD_currentEbikeco2save = mean(diffMDD_currentEbikeco2save,na.rm = T),
                    sumdiffMDD_currentEbikeco2save = sum(diffMDD_currentEbikeco2save,na.rm = T),
                    meandiffAllbike_AllEbikeco2save = mean(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffAllbike_AllEbikeco2save = sum(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffMDDco2save_AllEbikeco2save = sum(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    meandiffMDDco2save_AllEbikeco2save = mean(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    caraccess = sum(hasCar,na.rm = T),
                    sumkmEbike_mdd = sum(kmEbike_mdd,na.rm=T),
                    sumkmmdd_car = sum(kmmdd_car,na.rm=T),
                    sumEbike_car = sum(kmEbike_car,na.rm=T),
                    sumkmcarmddno_ebikeyes = sum(kmcarmddno_ebikeyes,na.rm=T),
                    
                    
                    meanAllEBikekmreplace30m = mean(AllEBikekmreplace30m,na.rm=T),          
                    sumAllEBikekmreplace30m = sum(AllEBikekmreplace30m,na.rm=T), 
                    meanAllEbikeco2save30m = mean(AllEbikeco2save30m,na.rm = T), 
                    sumAllEbikeco2save30m = sum(AllEbikeco2save30m,na.rm = T),
                    
                    
                    
                    meanAllEBikekmreplaceFlex = mean(AllEBikekmreplaceFlex,na.rm=T),          
                    sumAllEBikekmreplaceFlex = sum(AllEBikekmreplaceFlex,na.rm=T), 
                    meanAllEbikeco2saveFlex = mean(AllEbikeco2saveFlex,na.rm = T), 
                    sumAllEbikeco2saveFlex = sum(AllEbikeco2saveFlex,na.rm = T),							 
                    
                    meanAllEBikekmreplace30mFlex = mean(AllEBikekmreplace30mFlex,na.rm=T),          
                    sumAllEBikekmreplace30mFlex = sum(AllEBikekmreplace30mFlex,na.rm=T), 
                    meanAllEbikeco2save30mFlex = mean(AllEbikeco2save30mFlex,na.rm = T), 
                    sumAllEbikeco2save30mFlex = sum(AllEbikeco2save30mFlex,na.rm = T),
                    
                    kmEbike_mdd  =      sum( kmEbike_mdd,na.rm = T),                       
                    kmEbike_car   =      sum(kmEbike_car,na.rm = T),                      
                    kmmdd_car      =     sum(kmmdd_car,na.rm = T),                      
                    kmcarmddno_ebikeyes= sum(kmcarmddno_ebikeyes,na.rm = T),
                    kmEbike_car30m = sum(kmEbike_car30m,na.rm = T),
                    
                    meandiffAllEbikeco2save_AllEbikeco2save30m   = mean(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2saveFlex  = mean(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2save30mFlex = mean(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30m   = sum(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2saveFlex  = sum(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30mFlex = sum(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    ebikeDailyDist = mean(ebikemaxroaddist,na.rm = T)
),by = sexageconst
]

gc()


write_csv(MDO_sexage,paste0("Results",datetime,"/","MDO1_sexage",seed_number,"_", datetime, ".csv"))



gc()
